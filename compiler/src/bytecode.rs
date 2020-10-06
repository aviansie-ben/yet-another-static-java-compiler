use std::fmt;

use byteorder::{BigEndian, ByteOrder};

use crate::classfile::{AttributeCode, AttributeData, ConstantPoolEntry, Method, PrimitiveType};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BytecodeCondition {
    Eq,
    Ne,
    Lt,
    Ge,
    Gt,
    Le
}

impl fmt::Display for BytecodeCondition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            BytecodeCondition::Eq => "eq",
            BytecodeCondition::Ne => "ne",
            BytecodeCondition::Lt => "lt",
            BytecodeCondition::Ge => "ge",
            BytecodeCondition::Gt => "gt",
            BytecodeCondition::Le => "le"
        })
    }
}

#[derive(Debug, Clone)]
pub enum BytecodeInstruction {
    AALoad,
    AAStore,
    AConstNull,
    ALoad(u16),
    ANewArray(u16),
    AReturn,
    ArrayLength,
    AStore(u16),
    AThrow,
    BALoad,
    BAStore,
    CALoad,
    CAStore,
    CheckCast(u16),
    D2F,
    D2I,
    D2L,
    DAdd,
    DALoad,
    DAStore,
    DCmpG,
    DCmpL,
    DConst(u64),
    DDiv,
    DLoad(u16),
    DMul,
    DNeg,
    DRem,
    DReturn,
    DStore(u16),
    DSub,
    Dup,
    DupX1,
    DupX2,
    Dup2,
    Dup2X1,
    Dup2X2,
    F2D,
    F2I,
    F2L,
    FAdd,
    FALoad,
    FAStore,
    FCmpG,
    FCmpL,
    FConst(u32),
    FDiv,
    FLoad(u16),
    FMul,
    FNeg,
    FRem,
    FReturn,
    FStore(u16),
    FSub,
    GetField(u16),
    GetStatic(u16),
    Goto(usize),
    I2B,
    I2C,
    I2D,
    I2F,
    I2L,
    I2S,
    IAdd,
    IALoad,
    IAnd,
    IAStore,
    IConst(i32),
    IDiv,
    IfACmp(BytecodeCondition, usize),
    IfICmp(BytecodeCondition, usize),
    If(BytecodeCondition, usize),
    IfNonNull(usize),
    IfNull(usize),
    IInc(u16, i16),
    ILoad(u16),
    IMul,
    INeg,
    InstanceOf(u16),
    InvokeDynamic(u16),
    InvokeInterface(u16, u8),
    InvokeSpecial(u16),
    InvokeStatic(u16),
    InvokeVirtual(u16),
    IOr,
    IRem,
    IReturn,
    IShl,
    IShr,
    IStore(u16),
    ISub,
    IUShr,
    IXor,
    JSR(i32),
    L2D,
    L2F,
    L2I,
    LAdd,
    LALoad,
    LAnd,
    LAStore,
    LCmp,
    LConst(i64),
    Ldc(u16),
    Ldc2(u16),
    LDiv,
    LLoad(u16),
    LMul,
    LNeg,
    LookupSwitch(usize, Vec<(i32, usize)>),
    LOr,
    LRem,
    LReturn,
    LShl,
    LShr,
    LStore(u16),
    LSub,
    LUShr,
    LXor,
    MonitorEnter,
    MonitorExit,
    MultiANewArray(u16, u8),
    New(u16),
    NewArray(PrimitiveType),
    Nop,
    Pop,
    Pop2,
    PutField(u16),
    PutStatic(u16),
    Ret(u8),
    Return,
    SALoad,
    SAStore,
    Swap,
    TableSwitch(i32, usize, Vec<usize>)
}

impl BytecodeInstruction {
    pub fn pretty<'a>(&'a self, cp: &'a [ConstantPoolEntry]) -> impl fmt::Display + 'a {
        PrettyBytecodeInstruction(self, cp)
    }
}

struct PrettyConstantPoolEntry<'a>(u16, &'a [ConstantPoolEntry]);
struct PrettyBytecodeInstruction<'a>(&'a BytecodeInstruction, &'a [ConstantPoolEntry]);

impl <'a> fmt::Display for PrettyConstantPoolEntry<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let PrettyConstantPoolEntry(idx, cp) = *self;
        write!(f, "{} ", idx)?;
        match cp[idx as usize] {
            ConstantPoolEntry::Class(ref cpe) => write!(f, "<class {}>", cpe.name),
            ConstantPoolEntry::Fieldref(ref cpe) => write!(f, "<field {}.{} {}>",
                match cp[cpe.class as usize] {
                    ConstantPoolEntry::Class(ref cpe) => &cpe.name,
                    _ => "???"
                },
                cpe.name,
                cpe.descriptor
            ),
            ConstantPoolEntry::Methodref(ref cpe) | ConstantPoolEntry::InterfaceMethodref(ref cpe) => write!(f, "<method {}.{}{}>",
                match cp[cpe.class as usize] {
                    ConstantPoolEntry::Class(ref cpe) => &cpe.name,
                    _ => "???"
                },
                cpe.name,
                cpe.descriptor
            ),
            ConstantPoolEntry::String(ref cpe) => write!(f, "<string {:?}>", cpe.contents.as_ref()),
            ConstantPoolEntry::Integer(val) => write!(f, "<int {}>", val),
            ConstantPoolEntry::Float(val) => write!(f, "<float {} (0x{:08x})>", f32::from_bits(val), val),
            ConstantPoolEntry::Long(val) => write!(f, "<long {}>", val),
            ConstantPoolEntry::Double(val) => write!(f, "<double {} (0x{:016x})>", f64::from_bits(val), val),
            ConstantPoolEntry::NameAndType((ref name, ref ty)) => write!(f, "<nameandtype {} {}>", name, ty),
            ConstantPoolEntry::Utf8(ref val) => write!(f, "<utf8 {:?}>", val.as_ref()),
            ConstantPoolEntry::MethodHandle(_) => write!(f, "<methodhandle>"),
            ConstantPoolEntry::MethodType(_) => write!(f, "<methodtype>"),
            ConstantPoolEntry::InvokeDynamic(_) => write!(f, "<invokedynamic>"),
            ConstantPoolEntry::Empty => write!(f, "<empty>")
        }
    }
}

impl <'a> fmt::Display for PrettyBytecodeInstruction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let PrettyBytecodeInstruction(instr, cp) = *self;

        match *instr {
            BytecodeInstruction::AALoad => write!(f, "aaload"),
            BytecodeInstruction::AAStore => write!(f, "aastore"),
            BytecodeInstruction::AConstNull => write!(f, "aconst_null"),
            BytecodeInstruction::ALoad(idx) => write!(f, "aload {}", idx),
            BytecodeInstruction::ANewArray(idx) => write!(f, "anewarray {}", PrettyConstantPoolEntry(idx, cp)),
            BytecodeInstruction::AReturn => write!(f, "areturn"),
            BytecodeInstruction::ArrayLength => write!(f, "arraylength"),
            BytecodeInstruction::AStore(idx) => write!(f, "astore {}", idx),
            BytecodeInstruction::AThrow => write!(f, "athrow"),
            BytecodeInstruction::BALoad => write!(f, "baload"),
            BytecodeInstruction::BAStore => write!(f, "bastore"),
            BytecodeInstruction::CALoad => write!(f, "caload"),
            BytecodeInstruction::CAStore => write!(f, "castore"),
            BytecodeInstruction::CheckCast(idx) => write!(f, "checkcast {}", PrettyConstantPoolEntry(idx, cp)),
            BytecodeInstruction::D2F => write!(f, "d2f"),
            BytecodeInstruction::D2I => write!(f, "d2i"),
            BytecodeInstruction::D2L => write!(f, "d2l"),
            BytecodeInstruction::DAdd => write!(f, "dadd"),
            BytecodeInstruction::DALoad => write!(f, "daload"),
            BytecodeInstruction::DAStore => write!(f, "dastore"),
            BytecodeInstruction::DCmpG => write!(f, "dcmpg"),
            BytecodeInstruction::DCmpL => write!(f, "dcmpl"),
            BytecodeInstruction::DConst(val) => write!(f, "dconst {}", f64::from_bits(val)),
            BytecodeInstruction::DDiv => write!(f, "ddiv"),
            BytecodeInstruction::DLoad(idx) => write!(f, "dload {}", idx),
            BytecodeInstruction::DMul => write!(f, "dmul"),
            BytecodeInstruction::DNeg => write!(f, "dneg"),
            BytecodeInstruction::DRem => write!(f, "drem"),
            BytecodeInstruction::DReturn => write!(f, "dreturn"),
            BytecodeInstruction::DStore(idx) => write!(f, "dstore {}", idx),
            BytecodeInstruction::DSub => write!(f, "dsub"),
            BytecodeInstruction::Dup => write!(f, "dup"),
            BytecodeInstruction::DupX1 => write!(f, "dup_x1"),
            BytecodeInstruction::DupX2 => write!(f, "dup_x2"),
            BytecodeInstruction::Dup2 => write!(f, "dup2"),
            BytecodeInstruction::Dup2X1 => write!(f, "dup2_x1"),
            BytecodeInstruction::Dup2X2 => write!(f, "dup2_x2"),
            BytecodeInstruction::F2D => write!(f, "f2d"),
            BytecodeInstruction::F2I => write!(f, "f2i"),
            BytecodeInstruction::F2L => write!(f, "f2l"),
            BytecodeInstruction::FAdd => write!(f, "fadd"),
            BytecodeInstruction::FALoad => write!(f, "faload"),
            BytecodeInstruction::FAStore => write!(f, "fastore"),
            BytecodeInstruction::FCmpG => write!(f, "fcmpg"),
            BytecodeInstruction::FCmpL => write!(f, "fcmpl"),
            BytecodeInstruction::FConst(val) => write!(f, "fconst {}", f32::from_bits(val)),
            BytecodeInstruction::FDiv => write!(f, "fdiv"),
            BytecodeInstruction::FLoad(idx) => write!(f, "fload {}", idx),
            BytecodeInstruction::FMul => write!(f, "fmul"),
            BytecodeInstruction::FNeg => write!(f, "fneg"),
            BytecodeInstruction::FRem => write!(f, "frem"),
            BytecodeInstruction::FReturn => write!(f, "freturn"),
            BytecodeInstruction::FStore(idx) => write!(f, "fstore {}", idx),
            BytecodeInstruction::FSub => write!(f, "fsub"),
            BytecodeInstruction::GetField(idx) => write!(f, "getfield {}", PrettyConstantPoolEntry(idx, cp)),
            BytecodeInstruction::GetStatic(idx) => write!(f, "getstatic {}", PrettyConstantPoolEntry(idx, cp)),
            BytecodeInstruction::Goto(tgt) => write!(f, "goto {}", tgt),
            BytecodeInstruction::I2B => write!(f, "i2b"),
            BytecodeInstruction::I2C => write!(f, "i2c"),
            BytecodeInstruction::I2D => write!(f, "i2d"),
            BytecodeInstruction::I2F => write!(f, "i2f"),
            BytecodeInstruction::I2L => write!(f, "i2l"),
            BytecodeInstruction::I2S => write!(f, "i2s"),
            BytecodeInstruction::IAdd => write!(f, "iadd"),
            BytecodeInstruction::IALoad => write!(f, "iaload"),
            BytecodeInstruction::IAnd => write!(f, "iand"),
            BytecodeInstruction::IAStore => write!(f, "iastore"),
            BytecodeInstruction::IConst(val) => write!(f, "iconst {}", val),
            BytecodeInstruction::IDiv => write!(f, "idiv"),
            BytecodeInstruction::IfACmp(cond, tgt) => write!(f, "if_acmp{} {}", cond, tgt),
            BytecodeInstruction::IfICmp(cond, tgt) => write!(f, "if_icmp{} {}", cond, tgt),
            BytecodeInstruction::If(cond, tgt) => write!(f, "if{} {}", cond, tgt),
            BytecodeInstruction::IfNonNull(tgt) => write!(f, "ifnonnull {}", tgt),
            BytecodeInstruction::IfNull(tgt) => write!(f, "ifnull {}", tgt),
            BytecodeInstruction::IInc(idx, val) => write!(f, "iinc {} {}", idx, val),
            BytecodeInstruction::ILoad(idx) => write!(f, "iload {}", idx),
            BytecodeInstruction::IMul => write!(f, "imul"),
            BytecodeInstruction::INeg => write!(f, "ineg"),
            BytecodeInstruction::InstanceOf(idx) => write!(f, "instanceof {}", PrettyConstantPoolEntry(idx, cp)),
            BytecodeInstruction::InvokeDynamic(idx) => write!(f, "invokedynamic {}", PrettyConstantPoolEntry(idx, cp)),
            BytecodeInstruction::InvokeInterface(idx, _) => write!(f, "invokeinterface {}", PrettyConstantPoolEntry(idx, cp)),
            BytecodeInstruction::InvokeSpecial(idx) => write!(f, "invokespecial {}", PrettyConstantPoolEntry(idx, cp)),
            BytecodeInstruction::InvokeStatic(idx) => write!(f, "invokestatic {}", PrettyConstantPoolEntry(idx, cp)),
            BytecodeInstruction::InvokeVirtual(idx) => write!(f, "invokevirtual {}", PrettyConstantPoolEntry(idx, cp)),
            BytecodeInstruction::IOr => write!(f, "ior"),
            BytecodeInstruction::IRem => write!(f, "irem"),
            BytecodeInstruction::IReturn => write!(f, "ireturn"),
            BytecodeInstruction::IShl => write!(f, "ishl"),
            BytecodeInstruction::IShr => write!(f, "ishr"),
            BytecodeInstruction::IStore(idx) => write!(f, "istore {}", idx),
            BytecodeInstruction::ISub => write!(f, "isub"),
            BytecodeInstruction::IUShr => write!(f, "iushr"),
            BytecodeInstruction::IXor => write!(f, "ixor"),
            BytecodeInstruction::JSR(tgt) => write!(f, "jsr {}", tgt),
            BytecodeInstruction::L2D => write!(f, "l2d"),
            BytecodeInstruction::L2F => write!(f, "l2f"),
            BytecodeInstruction::L2I => write!(f, "l2i"),
            BytecodeInstruction::LAdd => write!(f, "ladd"),
            BytecodeInstruction::LALoad => write!(f, "laload"),
            BytecodeInstruction::LAnd => write!(f, "land"),
            BytecodeInstruction::LAStore => write!(f, "lastore"),
            BytecodeInstruction::LCmp => write!(f, "lcmp"),
            BytecodeInstruction::LConst(val) => write!(f, "lconst {}", val),
            BytecodeInstruction::Ldc(idx) => write!(f, "ldc {}", PrettyConstantPoolEntry(idx, cp)),
            BytecodeInstruction::Ldc2(idx) => write!(f, "ldc2 {}", PrettyConstantPoolEntry(idx, cp)),
            BytecodeInstruction::LDiv => write!(f, "ldiv"),
            BytecodeInstruction::LLoad(idx) => write!(f, "lload {}", idx),
            BytecodeInstruction::LMul => write!(f, "lmul"),
            BytecodeInstruction::LNeg => write!(f, "lneg"),
            BytecodeInstruction::LookupSwitch(default_tgt, ref table) => try {
                write!(f, "lookupswitch [ ")?;
                for &(val, tgt) in table.iter() {
                    write!(f, "{}:{} ", val, tgt)?;
                };
                write!(f, "default:{} ]", default_tgt)?;
                ()
            },
            BytecodeInstruction::LOr => write!(f, "lor"),
            BytecodeInstruction::LRem => write!(f, "lrem"),
            BytecodeInstruction::LReturn => write!(f, "lreturn"),
            BytecodeInstruction::LShl => write!(f, "lshl"),
            BytecodeInstruction::LShr => write!(f, "lshr"),
            BytecodeInstruction::LStore(idx) => write!(f, "lstore {}", idx),
            BytecodeInstruction::LSub => write!(f, "lsub"),
            BytecodeInstruction::LUShr => write!(f, "lushr"),
            BytecodeInstruction::LXor => write!(f, "lxor"),
            BytecodeInstruction::MonitorEnter => write!(f, "monitorenter"),
            BytecodeInstruction::MonitorExit => write!(f, "monitorexit"),
            BytecodeInstruction::MultiANewArray(idx, dims) => write!(f, "multianewarray {} {}", PrettyConstantPoolEntry(idx, cp), dims),
            BytecodeInstruction::New(idx) => write!(f, "new {}", PrettyConstantPoolEntry(idx, cp)),
            BytecodeInstruction::NewArray(ty) => write!(f, "newarray {}", ty.as_char()),
            BytecodeInstruction::Nop => write!(f, "nop"),
            BytecodeInstruction::Pop => write!(f, "pop"),
            BytecodeInstruction::Pop2 => write!(f, "pop2"),
            BytecodeInstruction::PutField(idx) => write!(f, "putfield {}", PrettyConstantPoolEntry(idx, cp)),
            BytecodeInstruction::PutStatic(idx) => write!(f, "putstatic {}", PrettyConstantPoolEntry(idx, cp)),
            BytecodeInstruction::Ret(idx) => write!(f, "ret {}", idx),
            BytecodeInstruction::Return => write!(f, "return"),
            BytecodeInstruction::SALoad => write!(f, "saload"),
            BytecodeInstruction::SAStore => write!(f, "sastore"),
            BytecodeInstruction::Swap => write!(f, "swap"),
            BytecodeInstruction::TableSwitch(start_val, default_tgt, ref table) => try {
                write!(f, "tableswitch [ ")?;
                for (off, &tgt) in table.iter().enumerate() {
                    write!(f, "{}:{} ", start_val.wrapping_add(off as i32), tgt)?;
                };
                write!(f, "default:{} ]", default_tgt)?;
                ()
            }
        }
    }
}

fn read_u8(bytecode: &[u8], off: usize) -> Result<u8, usize> {
    let bytecode = &bytecode[off..];

    if bytecode.len() >= 1 {
        Result::Ok(bytecode[0])
    } else {
        Result::Err(off)
    }
}

fn read_u16(bytecode: &[u8], off: usize) -> Result<u16, usize> {
    let bytecode = &bytecode[off..];

    if bytecode.len() >= 2 {
        Result::Ok(BigEndian::read_u16(bytecode))
    } else {
        Result::Err(off)
    }
}

fn read_u32(bytecode: &[u8], off: usize) -> Result<u32, usize> {
    let bytecode = &bytecode[off..];

    if bytecode.len() >= 4 {
        Result::Ok(BigEndian::read_u32(bytecode))
    } else {
        Result::Err(off)
    }
}

pub fn read_op(bytecode: &[u8], off: usize) -> Result<(BytecodeInstruction, usize), usize> {
    if bytecode.len() <= off {
        return Result::Err(off);
    };

    Result::Ok(match bytecode[off] {
        0x32 => (BytecodeInstruction::AALoad, 1),
        0x53 => (BytecodeInstruction::AAStore, 1),
        0x01 => (BytecodeInstruction::AConstNull, 1),
        0x19 => (BytecodeInstruction::ALoad(read_u8(bytecode, off + 1)? as u16), 2),
        0x2a => (BytecodeInstruction::ALoad(0), 1),
        0x2b => (BytecodeInstruction::ALoad(1), 1),
        0x2c => (BytecodeInstruction::ALoad(2), 1),
        0x2d => (BytecodeInstruction::ALoad(3), 1),
        0xbd => (BytecodeInstruction::ANewArray(read_u16(bytecode, off + 1)?), 3),
        0xb0 => (BytecodeInstruction::AReturn, 1),
        0xbe => (BytecodeInstruction::ArrayLength, 1),
        0x3a => (BytecodeInstruction::AStore(read_u8(bytecode, off + 1)? as u16), 2),
        0x4b => (BytecodeInstruction::AStore(0), 1),
        0x4c => (BytecodeInstruction::AStore(1), 1),
        0x4d => (BytecodeInstruction::AStore(2), 1),
        0x4e => (BytecodeInstruction::AStore(3), 1),
        0xbf => (BytecodeInstruction::AThrow, 1),
        0x33 => (BytecodeInstruction::BALoad, 1),
        0x54 => (BytecodeInstruction::BAStore, 1),
        0x10 => (BytecodeInstruction::IConst(read_u8(bytecode, off + 1)? as i8 as i32), 2),
        0x34 => (BytecodeInstruction::CALoad, 1),
        0x55 => (BytecodeInstruction::CAStore, 1),
        0xc0 => (BytecodeInstruction::CheckCast(read_u16(bytecode, off + 1)?), 3),
        0x90 => (BytecodeInstruction::D2F, 1),
        0x8e => (BytecodeInstruction::D2I, 1),
        0x8f => (BytecodeInstruction::D2L, 1),
        0x63 => (BytecodeInstruction::DAdd, 1),
        0x31 => (BytecodeInstruction::DALoad, 1),
        0x52 => (BytecodeInstruction::DAStore, 1),
        0x98 => (BytecodeInstruction::DCmpG, 1),
        0x97 => (BytecodeInstruction::DCmpL, 1),
        0x0e => (BytecodeInstruction::DConst(f64::to_bits(0.0)), 1),
        0x0f => (BytecodeInstruction::DConst(f64::to_bits(1.0)), 1),
        0x6f => (BytecodeInstruction::DDiv, 1),
        0x18 => (BytecodeInstruction::DLoad(read_u8(bytecode, off + 1)? as u16), 2),
        0x26 => (BytecodeInstruction::DLoad(0), 1),
        0x27 => (BytecodeInstruction::DLoad(1), 1),
        0x28 => (BytecodeInstruction::DLoad(2), 1),
        0x29 => (BytecodeInstruction::DLoad(3), 1),
        0x6b => (BytecodeInstruction::DMul, 1),
        0x77 => (BytecodeInstruction::DNeg, 1),
        0x73 => (BytecodeInstruction::DRem, 1),
        0xaf => (BytecodeInstruction::DReturn, 1),
        0x39 => (BytecodeInstruction::DStore(read_u8(bytecode, off + 1)? as u16), 2),
        0x47 => (BytecodeInstruction::DStore(0), 1),
        0x48 => (BytecodeInstruction::DStore(1), 1),
        0x49 => (BytecodeInstruction::DStore(2), 1),
        0x4a => (BytecodeInstruction::DStore(3), 1),
        0x67 => (BytecodeInstruction::DSub, 1),
        0x59 => (BytecodeInstruction::Dup, 1),
        0x5a => (BytecodeInstruction::DupX1, 1),
        0x5b => (BytecodeInstruction::DupX2, 1),
        0x5c => (BytecodeInstruction::Dup2, 1),
        0x5d => (BytecodeInstruction::Dup2X1, 1),
        0x5e => (BytecodeInstruction::Dup2X2, 1),
        0x8d => (BytecodeInstruction::F2D, 1),
        0x8b => (BytecodeInstruction::F2I, 1),
        0x8c => (BytecodeInstruction::F2L, 1),
        0x62 => (BytecodeInstruction::FAdd, 1),
        0x30 => (BytecodeInstruction::FALoad, 1),
        0x51 => (BytecodeInstruction::FAStore, 1),
        0x96 => (BytecodeInstruction::FCmpG, 1),
        0x95 => (BytecodeInstruction::FCmpL, 1),
        0x0b => (BytecodeInstruction::FConst(f32::to_bits(0.0)), 1),
        0x0c => (BytecodeInstruction::FConst(f32::to_bits(1.0)), 1),
        0x0d => (BytecodeInstruction::FConst(f32::to_bits(2.0)), 1),
        0x6e => (BytecodeInstruction::FDiv, 1),
        0x17 => (BytecodeInstruction::FLoad(read_u8(bytecode, off + 1)? as u16), 2),
        0x22 => (BytecodeInstruction::FLoad(0), 1),
        0x23 => (BytecodeInstruction::FLoad(1), 1),
        0x24 => (BytecodeInstruction::FLoad(2), 1),
        0x25 => (BytecodeInstruction::FLoad(3), 1),
        0x6a => (BytecodeInstruction::FMul, 1),
        0x76 => (BytecodeInstruction::FNeg, 1),
        0x72 => (BytecodeInstruction::FRem, 1),
        0xae => (BytecodeInstruction::FReturn, 1),
        0x38 => (BytecodeInstruction::FStore(read_u8(bytecode, off + 1)? as u16), 2),
        0x43 => (BytecodeInstruction::FStore(0), 1),
        0x44 => (BytecodeInstruction::FStore(1), 1),
        0x45 => (BytecodeInstruction::FStore(2), 1),
        0x46 => (BytecodeInstruction::FStore(3), 1),
        0x66 => (BytecodeInstruction::FSub, 1),
        0xb4 => (BytecodeInstruction::GetField(read_u16(bytecode, off + 1)?), 3),
        0xb2 => (BytecodeInstruction::GetStatic(read_u16(bytecode, off + 1)?), 3),
        0xa7 => (BytecodeInstruction::Goto(off.wrapping_add(read_u16(bytecode, off + 1)? as i16 as isize as usize)), 3),
        0xc8 => (BytecodeInstruction::Goto(off.wrapping_add(read_u32(bytecode, off + 1)? as i32 as isize as usize)), 5),
        0x91 => (BytecodeInstruction::I2B, 1),
        0x92 => (BytecodeInstruction::I2C, 1),
        0x87 => (BytecodeInstruction::I2D, 1),
        0x86 => (BytecodeInstruction::I2F, 1),
        0x85 => (BytecodeInstruction::I2L, 1),
        0x93 => (BytecodeInstruction::I2S, 1),
        0x60 => (BytecodeInstruction::IAdd, 1),
        0x2e => (BytecodeInstruction::IALoad, 1),
        0x7e => (BytecodeInstruction::IAnd, 1),
        0x4f => (BytecodeInstruction::IAStore, 1),
        0x02 => (BytecodeInstruction::IConst(-1), 1),
        0x03 => (BytecodeInstruction::IConst(0), 1),
        0x04 => (BytecodeInstruction::IConst(1), 1),
        0x05 => (BytecodeInstruction::IConst(2), 1),
        0x06 => (BytecodeInstruction::IConst(3), 1),
        0x07 => (BytecodeInstruction::IConst(4), 1),
        0x08 => (BytecodeInstruction::IConst(5), 1),
        0x6c => (BytecodeInstruction::IDiv, 1),
        0xa5 => (BytecodeInstruction::IfACmp(BytecodeCondition::Eq, off.wrapping_add(read_u16(bytecode, off + 1)? as i16 as isize as usize)), 3),
        0xa6 => (BytecodeInstruction::IfACmp(BytecodeCondition::Ne, off.wrapping_add(read_u16(bytecode, off + 1)? as i16 as isize as usize)), 3),
        0x9f => (BytecodeInstruction::IfICmp(BytecodeCondition::Eq, off.wrapping_add(read_u16(bytecode, off + 1)? as i16 as isize as usize)), 3),
        0xa0 => (BytecodeInstruction::IfICmp(BytecodeCondition::Ne, off.wrapping_add(read_u16(bytecode, off + 1)? as i16 as isize as usize)), 3),
        0xa1 => (BytecodeInstruction::IfICmp(BytecodeCondition::Lt, off.wrapping_add(read_u16(bytecode, off + 1)? as i16 as isize as usize)), 3),
        0xa2 => (BytecodeInstruction::IfICmp(BytecodeCondition::Ge, off.wrapping_add(read_u16(bytecode, off + 1)? as i16 as isize as usize)), 3),
        0xa3 => (BytecodeInstruction::IfICmp(BytecodeCondition::Gt, off.wrapping_add(read_u16(bytecode, off + 1)? as i16 as isize as usize)), 3),
        0xa4 => (BytecodeInstruction::IfICmp(BytecodeCondition::Le, off.wrapping_add(read_u16(bytecode, off + 1)? as i16 as isize as usize)), 3),
        0x99 => (BytecodeInstruction::If(BytecodeCondition::Eq, off.wrapping_add(read_u16(bytecode, off + 1)? as i16 as isize as usize)), 3),
        0x9a => (BytecodeInstruction::If(BytecodeCondition::Ne, off.wrapping_add(read_u16(bytecode, off + 1)? as i16 as isize as usize)), 3),
        0x9b => (BytecodeInstruction::If(BytecodeCondition::Lt, off.wrapping_add(read_u16(bytecode, off + 1)? as i16 as isize as usize)), 3),
        0x9c => (BytecodeInstruction::If(BytecodeCondition::Ge, off.wrapping_add(read_u16(bytecode, off + 1)? as i16 as isize as usize)), 3),
        0x9d => (BytecodeInstruction::If(BytecodeCondition::Gt, off.wrapping_add(read_u16(bytecode, off + 1)? as i16 as isize as usize)), 3),
        0x9e => (BytecodeInstruction::If(BytecodeCondition::Le, off.wrapping_add(read_u16(bytecode, off + 1)? as i16 as isize as usize)), 3),
        0xc7 => (BytecodeInstruction::IfNonNull(off.wrapping_add(read_u16(bytecode, off + 1)? as i16 as isize as usize)), 3),
        0xc6 => (BytecodeInstruction::IfNull(off.wrapping_add(read_u16(bytecode, off + 1)? as i16 as isize as usize)), 3),
        0x84 => (BytecodeInstruction::IInc(read_u8(bytecode, off + 1)? as u16, read_u8(bytecode, off + 2)? as i8 as i16), 3),
        0x15 => (BytecodeInstruction::ILoad(read_u8(bytecode, off + 1)? as u16), 2),
        0x1a => (BytecodeInstruction::ILoad(0), 1),
        0x1b => (BytecodeInstruction::ILoad(1), 1),
        0x1c => (BytecodeInstruction::ILoad(2), 1),
        0x1d => (BytecodeInstruction::ILoad(3), 1),
        0x68 => (BytecodeInstruction::IMul, 1),
        0x74 => (BytecodeInstruction::INeg, 1),
        0xc1 => (BytecodeInstruction::InstanceOf(read_u16(bytecode, off + 1)?), 3),
        0xba => {
            let ind = read_u16(bytecode, off + 1)?;
            if read_u16(bytecode, off + 3)? != 0 {
                return Result::Err(off + 3);
            };

            (BytecodeInstruction::InvokeDynamic(ind), 5)
        },
        0xb9 => {
            let ind = read_u16(bytecode, off + 1)?;
            let count = read_u8(bytecode, off + 3)?;
            if read_u8(bytecode, off + 4)? != 0 {
                return Result::Err(off + 4);
            };

            (BytecodeInstruction::InvokeInterface(ind, count), 5)
        },
        0xb7 => (BytecodeInstruction::InvokeSpecial(read_u16(bytecode, off + 1)?), 3),
        0xb8 => (BytecodeInstruction::InvokeStatic(read_u16(bytecode, off + 1)?), 3),
        0xb6 => (BytecodeInstruction::InvokeVirtual(read_u16(bytecode, off + 1)?), 3),
        0x80 => (BytecodeInstruction::IOr, 1),
        0x70 => (BytecodeInstruction::IRem, 1),
        0xac => (BytecodeInstruction::IReturn, 1),
        0x78 => (BytecodeInstruction::IShl, 1),
        0x7a => (BytecodeInstruction::IShr, 1),
        0x36 => (BytecodeInstruction::IStore(read_u8(bytecode, off + 1)? as u16), 2),
        0x3b => (BytecodeInstruction::IStore(0), 1),
        0x3c => (BytecodeInstruction::IStore(1), 1),
        0x3d => (BytecodeInstruction::IStore(2), 1),
        0x3e => (BytecodeInstruction::IStore(3), 1),
        0x64 => (BytecodeInstruction::ISub, 1),
        0x7c => (BytecodeInstruction::IUShr, 1),
        0x82 => (BytecodeInstruction::IXor, 1),
        0xa8 => (BytecodeInstruction::JSR(read_u16(bytecode, off + 1)? as i16 as i32), 3),
        0xc9 => (BytecodeInstruction::JSR(read_u32(bytecode, off + 1)? as i32), 5),
        0x8a => (BytecodeInstruction::L2D, 1),
        0x89 => (BytecodeInstruction::L2F, 1),
        0x88 => (BytecodeInstruction::L2I, 1),
        0x61 => (BytecodeInstruction::LAdd, 1),
        0x2f => (BytecodeInstruction::LALoad, 1),
        0x7f => (BytecodeInstruction::LAnd, 1),
        0x50 => (BytecodeInstruction::LAStore, 1),
        0x94 => (BytecodeInstruction::LCmp, 1),
        0x09 => (BytecodeInstruction::LConst(0), 1),
        0x0a => (BytecodeInstruction::LConst(1), 1),
        0x12 => (BytecodeInstruction::Ldc(read_u8(bytecode, off + 1)? as u16), 2),
        0x13 => (BytecodeInstruction::Ldc(read_u16(bytecode, off + 1)?), 3),
        0x14 => (BytecodeInstruction::Ldc2(read_u16(bytecode, off + 1)?), 3),
        0x6d => (BytecodeInstruction::LDiv, 1),
        0x16 => (BytecodeInstruction::LLoad(read_u8(bytecode, off + 1)? as u16), 2),
        0x1e => (BytecodeInstruction::LLoad(0), 1),
        0x1f => (BytecodeInstruction::LLoad(1), 1),
        0x20 => (BytecodeInstruction::LLoad(2), 1),
        0x21 => (BytecodeInstruction::LLoad(3), 1),
        0x69 => (BytecodeInstruction::LMul, 1),
        0x75 => (BytecodeInstruction::LNeg, 1),
        0xab => {
            let pad = (4 - ((off + 1) & 0x3)) & 0x3;
            if bytecode.len() < off + 1 + pad {
                return Result::Err(bytecode.len());
            };

            let default = off.wrapping_add(read_u32(bytecode, off + pad + 1)? as i32 as isize as usize);
            let len = read_u32(bytecode, off + pad + 5)? as usize;

            let mut lookup = vec![];
            for i in 0..len {
                lookup.push((
                    read_u32(bytecode, off + pad + 9 + (i * 8))? as i32,
                    off.wrapping_add(read_u32(bytecode, off + pad + 13 + (i * 8))? as i32 as isize as usize)
                ));
            };

            (BytecodeInstruction::LookupSwitch(default, lookup), pad + 9 + (len * 8))
        },
        0x81 => (BytecodeInstruction::LOr, 1),
        0x71 => (BytecodeInstruction::LRem, 1),
        0xad => (BytecodeInstruction::LReturn, 1),
        0x79 => (BytecodeInstruction::LShl, 1),
        0x7b => (BytecodeInstruction::LShr, 1),
        0x37 => (BytecodeInstruction::LStore(read_u8(bytecode, off + 1)? as u16), 2),
        0x3f => (BytecodeInstruction::LStore(0), 1),
        0x40 => (BytecodeInstruction::LStore(1), 1),
        0x41 => (BytecodeInstruction::LStore(2), 1),
        0x42 => (BytecodeInstruction::LStore(3), 1),
        0x65 => (BytecodeInstruction::LSub, 1),
        0x7d => (BytecodeInstruction::LUShr, 1),
        0x83 => (BytecodeInstruction::LXor, 1),
        0xc2 => (BytecodeInstruction::MonitorEnter, 1),
        0xc3 => (BytecodeInstruction::MonitorExit, 1),
        0xc5 => (BytecodeInstruction::MultiANewArray(read_u16(bytecode, off + 1)?, read_u8(bytecode, off + 3)?), 4),
        0xbb => (BytecodeInstruction::New(read_u16(bytecode, off + 1)?), 3),
        0xbc => (BytecodeInstruction::NewArray(match read_u8(bytecode, off + 1)? {
            0x04 => PrimitiveType::Boolean,
            0x05 => PrimitiveType::Char,
            0x06 => PrimitiveType::Float,
            0x07 => PrimitiveType::Double,
            0x08 => PrimitiveType::Byte,
            0x09 => PrimitiveType::Short,
            0x0a => PrimitiveType::Int,
            0x0b => PrimitiveType::Long,
            _ => {
                return Result::Err(off + 1);
            }
        }), 2),
        0x00 => (BytecodeInstruction::Nop, 1),
        0x57 => (BytecodeInstruction::Pop, 1),
        0x58 => (BytecodeInstruction::Pop2, 1),
        0xb5 => (BytecodeInstruction::PutField(read_u16(bytecode, off + 1)?), 3),
        0xb3 => (BytecodeInstruction::PutStatic(read_u16(bytecode, off + 1)?), 3),
        0xa9 => (BytecodeInstruction::Ret(read_u8(bytecode, off + 1)?), 2),
        0xb1 => (BytecodeInstruction::Return, 1),
        0x35 => (BytecodeInstruction::SALoad, 1),
        0x56 => (BytecodeInstruction::SAStore, 1),
        0x11 => (BytecodeInstruction::IConst(read_u16(bytecode, off + 1)? as i16 as i32), 3),
        0x5f => (BytecodeInstruction::Swap, 1),
        0xaa => {
            let pad = (4 - ((off + 1) & 0x3)) & 0x3;
            if bytecode.len() < off + 1 + pad {
                return Result::Err(bytecode.len());
            };

            let default = off.wrapping_add(read_u32(bytecode, off + pad + 1)? as i32 as isize as usize);
            let lo = read_u32(bytecode, off + pad + 5)? as i32;
            let hi = read_u32(bytecode, off + pad + 9)? as i32;
            let len = (hi - lo + 1) as usize;

            let mut table = vec![];
            for i in 0..len {
                table.push(
                    off.wrapping_add(read_u32(bytecode, off + pad + 13 + (i * 4))? as i32 as isize as usize),
                );
            };

            (BytecodeInstruction::TableSwitch(lo, default, table), pad + 13 + (len * 4))
        },
        0xc4 => match read_u8(bytecode, off + 1)? {
            0x19 => (BytecodeInstruction::ALoad(read_u16(bytecode, off + 2)?), 4),
            0x3a => (BytecodeInstruction::AStore(read_u16(bytecode, off + 2)?), 4),
            0x18 => (BytecodeInstruction::DLoad(read_u16(bytecode, off + 2)?), 4),
            0x39 => (BytecodeInstruction::DStore(read_u16(bytecode, off + 2)?), 4),
            0x17 => (BytecodeInstruction::FLoad(read_u16(bytecode, off + 2)?), 4),
            0x38 => (BytecodeInstruction::FStore(read_u16(bytecode, off + 2)?), 4),
            0x84 => (BytecodeInstruction::IInc(read_u16(bytecode, off + 2)?, read_u16(bytecode, off + 4)? as i16), 6),
            0x15 => (BytecodeInstruction::ILoad(read_u16(bytecode, off + 2)?), 4),
            0x36 => (BytecodeInstruction::IStore(read_u16(bytecode, off + 2)?), 4),
            0x16 => (BytecodeInstruction::LLoad(read_u16(bytecode, off + 2)?), 4),
            0x37 => (BytecodeInstruction::LStore(read_u16(bytecode, off + 2)?), 4),
            _ => {
                return Result::Err(off + 1);
            }
        },
        _ => {
            return Result::Err(off);
        }
    })
}

#[derive(Debug, Clone, Copy)]
pub struct BytecodeIterator<'a>(pub &'a [u8], pub usize);

impl <'a> BytecodeIterator<'a> {
    pub fn for_code(code: &'a AttributeCode) -> BytecodeIterator<'a> {
        BytecodeIterator(&code.code, 0)
    }

    pub fn for_method(method: &'a Method) -> Option<BytecodeIterator<'a>> {
        for a in method.attributes.iter() {
            match a.data {
                AttributeData::Code(ref code) => {
                    return Some(BytecodeIterator::for_code(code));
                },
                _ => {}
            };
        };

        None
    }
}

impl <'a> PartialEq for BytecodeIterator<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0 as *const [u8] == other.0 as *const [u8] && self.1 == other.1
    }
}

impl <'a> Eq for BytecodeIterator<'a> {}

impl <'a> Iterator for BytecodeIterator<'a> {
    type Item = (usize, Result<BytecodeInstruction, usize>);

    fn next(&mut self) -> Option<Self::Item> {
        if self.1 != self.0.len() {
            let off = self.1;
            let instr = match read_op(self.0, self.1) {
                Result::Ok((instr, len)) => {
                    self.1 += len;
                    Result::Ok(instr)
                },
                Result::Err(err) => {
                    self.1 = self.0.len();
                    Result::Err(err)
                }
            };

            Some((off, instr))
        } else {
            None
        }
    }
}
