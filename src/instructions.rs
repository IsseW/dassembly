#![allow(dead_code)]
use num_traits::FromPrimitive;
use phf::{phf_map};
use futures::{FutureExt, lock::Mutex};
use futures::Future;
use paste::paste;
use tokio::time::{self, Sleep};
use std::pin::Pin;
use std::task::Poll;

#[macro_use]
use num_derive::{FromPrimitive, ToPrimitive};
struct MemoryAllocation<'a> {
    references : usize,
    data : &'a [u8],
}

struct Assembly<'a> {
    memory: Vec<MemoryAllocation<'a>>,
    statics: Vec<MemoryAllocation<'a>>,
    main_thread : Thread<'a>,
}
impl<'a> Assembly<'a> {

    fn type_of(&self, memptr : usize) -> Option<OperandType> {
        Assembly::type_of_memory(self.memory[memptr].data)
    }
    fn type_of_memory(memory : &[u8]) -> Option<OperandType> {
        let mut mem: [u8; 4] = Default::default();
        mem.copy_from_slice(memory);
        FromPrimitive::from_u32(u32::from_le_bytes(mem))
    }
    fn read_field(memory : &[u8], fieldPtr : usize) -> [u8; 8] {
        let mut mem: [u8; 8] = Default::default();
        let index = 4 + fieldPtr * 8;
        mem.copy_from_slice(&memory[index..index + 8]);
        mem
    }
    fn load_field_from_memory(memory : &[u8], fieldPtr : usize) -> Option<Operand> {
        Some(parse_operand(Assembly::type_of_memory(memory)?, Assembly::read_field(memory, fieldPtr)))
    }

    fn load_field(&self, ptr : usize, fieldPtr : usize) -> Option<Operand> {
        Assembly::load_field_from_memory(self.memory[fieldPtr].data, fieldPtr)
    }
} 

struct Thread<'a> {
    assembly: &'a mut Assembly<'a>,
    instructions : &'a Vec<(Opcode, Operand)>,
    locals : Vec<Operand>,
    evaluation : Vec<Operand>,
}

impl<'a> Thread<'a> {
    fn create<'b>(assembly: &'a mut Assembly<'a>, instructions: &'a Vec<(Opcode, Operand)>, parameters : &'b [Operand]) -> Thread<'a> {
        Thread {
            assembly : assembly,
            instructions : instructions,
            locals : parameters.to_vec(),
            evaluation : vec![],
        }
    }

    fn run(&mut self) {

    }

    fn pop_evaluation(&mut self) -> Option<Operand> {
        self.evaluation.pop()
    }

    fn push_evaluation(&mut self, operand : Operand) {
        self.evaluation.push(operand);
    }

    fn get_local(&self, ptr : usize) -> Option<Operand> {
        self.locals.get(ptr).cloned()
    }

    fn set_local(&mut self, ptr : usize, value : Operand) -> Result<(), ErrType> {
        if ptr > self.locals.len() {
            Err(ErrType::UnknownLocal)
        }
        else {
            self.locals[ptr] = value;
            Ok(())
        }
    }
    fn push_local(&mut self, value : Operand) {
        self.locals.push(value);
    }
}


macro_rules! Operands {
    ($($name:ident : $type:ty), *) => {
        #[derive(Copy, Clone)]
        pub enum Operand {
            $($name($type)), *,
            None(bool)
        }
        #[derive(Copy, Clone, FromPrimitive, ToPrimitive)]
        pub enum OperandType {
            $($name), *,
            None
        }

        pub fn parse_operand(operand_type: OperandType, data: [u8; 8]) -> Operand {
            match operand_type {
                $(OperandType::$name => Operand::$name(<$type>::from_le_bytes(data))), *,
                _ => Operand::None(false),
            }
        }

        pub fn get_operand_type(operand: &Operand) -> OperandType {
            match operand {
                $(Operand::$name(_) => OperandType::$name), *,
                _ => OperandType::None,
            }
        }

        
        impl<'a> Thread<'a> {
            paste!{$(
                #[allow(dead_code)]
                fn [< pop_ $name:lower >] (&mut self) -> Option<$type> { 
                    match self.pop_evaluation() { 
                        Some(Operand::$name(t)) => Some(t), 
                        _ => None 
                    }
                }
                #[allow(dead_code)]
                fn [< push_ $name:lower >] (&mut self, data : $type) {
                    self.push_evaluation(Operand::$name(data))
                }
        
            )*}
        }
    };
}

Operands! {
    Integer : i64,
    Floating : f64,
    Pointer : usize,
    MemoryPointer : usize,
    Error : usize
}

enum ErrType {
    WrongOperand,
    Error,
    EmptyEvaluationStack,
    MemoryReadError,
    UnknownLocal,
}

macro_rules! count_idents {
    ($($idents:ident),* $(,)*) => {
        {
            #[derive(Copy, Clone)]
            #[allow(dead_code, non_camel_case_types)]
            enum IdentsCounter { $($idents,)* __CountIdentsLast }
            const COUNT: usize = IdentsCounter::__CountIdentsLast as usize;
            COUNT
        }
    };
}


macro_rules! define_const_map {
    ($name:ident, $key_type:ty, $value_type:ty, $($key:expr => $value:expr),*) => {
        const $name: phf::Map<$key_type, $value_type> = phf_map! {
            $($key => $value), *
        };
    }
}

macro_rules! opcodes {
    ($($opcode:ident $name:literal $description:literal $($operand_type: ident $code : expr) *), *) => {
        
        #[derive(Copy, Clone)]
        #[allow(non_camel_case_types)]
        pub enum Opcode {
            $($opcode), *
        }
        const NUM_OPCODES: usize = count_idents!($($opcode), *);

        define_const_map!(NAME_TO_OPCODE, &'static str, Opcode, $($name => Opcode::$opcode), *);

        const OPCODE_TO_NAME: [&'static str; NUM_OPCODES] = [$(stringify!($opcode)), *];
        const OPCODE_DESCRIPTIONS: [&'static str; NUM_OPCODES] = [$($description), *];
        const OPCODE_FUNCTIONS: [fn(&mut Thread, Operand) -> Result<(), ErrType>; NUM_OPCODES] = [$(|thread, operand| { map_type!(thread, operand, $($operand_type $code), *) }), *];
        const OPCODE_EXPECTED_TYPE: [&'static [OperandType]; NUM_OPCODES] = [$(&[$(OperandType::$operand_type), *]), *];
    };
}

macro_rules! map_type {
    ($thread : ident, $operand:ident, $($operand_type:ident $code:expr), *) => {
        match $operand {
            $(Operand::$operand_type(data) => {
                let x = $code;
                return x($thread, data);
            }), *
            _ => Err(ErrType::WrongOperand),
        }
    };
}

// Opcodes can support no and/or one operand
opcodes! {
    ldfld "ldfld" "Loads a field from the value on the top of the evaluation stack and pushes it onto the evaluation stack."
    Pointer |thread : &mut Thread, data| {
        
        let ptr = thread.pop_memorypointer().ok_or(ErrType::EmptyEvaluationStack)?;
        let operand = thread.assembly.load_field(ptr, data).ok_or(ErrType::MemoryReadError)?;

        thread.push_evaluation(operand);

        Ok(())
        
    }
    None |thread : &mut Thread, data| {
        Ok(())
    },
    ldlcl "ldlcl" "Loads a local variable and pushes it onto the evaluation stack."
    Pointer |thread : &mut Thread, data| {
        thread.push_evaluation(thread.get_local(data).ok_or(ErrType::UnknownLocal)?);
        Ok(())
    },
    ldcnst "ldstc" "Loads a static variable and pushes it onto the evaluation stack."
    Pointer |thread : &mut Thread, data| {
        thread.push_evaluation(thread.get_local(data).ok_or(ErrType::UnknownLocal)?);
        Ok(())
    }
}

pub fn get_opcode(name: &str) -> Option<Opcode> {
    NAME_TO_OPCODE.get(name).cloned()
}

pub fn get_opcode_name(opcode : Opcode) -> &'static str {
    OPCODE_TO_NAME[opcode as usize]
}
pub fn get_opcode_description(opcode : Opcode) -> &'static str {
    OPCODE_DESCRIPTIONS[opcode as usize]
}