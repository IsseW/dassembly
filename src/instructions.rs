use phf::{phf_map};
use std::sync::{Mutex, Arc};

struct MemoryAllocation<'a> {
    references : usize,
    data : &'a [u8],
}

struct Assembly<'a> {
    memory: Vec<Mutex<MemoryAllocation<'a>>>,
    main_thread : Thread<'a>,
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

    fn run() {

    }
}

macro_rules! Operands {
    ($($name:ident : $type:ty), *) => {
        #[derive(Copy, Clone)]
        pub enum Operand {
            $($name($type)), *,
            None(bool)
        }
        #[derive(Copy, Clone)]
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
    };
}

Operands! {
    Integer : i64,
    Floating : f64,
    Pointer : usize,
    MemoryPointer : usize
}

enum OpcodeResult {
    Ok,
    WrongOperand,
    Error(&'static str),
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
        const OPCODE_FUNCTIONS: [fn(&mut Thread, Operand) -> OpcodeResult; NUM_OPCODES] = [$(|thread, operand| { map_type!(thread, operand, $($operand_type $code), *) }), *];
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
            _ => OpcodeResult::WrongOperand,
        }
    };
}

// Opcodes can support no and/or one operand
opcodes! {
    ldfld "ldfld" "Loads a field from the value on the top of the evaluation stack and pushes it onto the evaluation stack."
    Pointer |thread, data| {
        if data == 0 {
            OpcodeResult::Ok 
        } else { 
            OpcodeResult::Error("Some error")
        }
    },
    ldlcl "ldlcl" "Loads a local variable and pushes it onto the evaluation stack."
    Pointer |thread, data| {
        if data == 0 { 
            OpcodeResult::Ok 
        } else { 
            OpcodeResult::Error("Some error")
        }
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