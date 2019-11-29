use byteorder::{BigEndian, ByteOrder};

use crate::classfile::{AttributeCode, AttributeData, Method, PrimitiveType};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BytecodeCondition {
    Eq,
    Ne,
    Lt,
    Ge,
    Gt,
    Le
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
    LookupSwitch(i32, Vec<(i32, i32)>),
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
    TableSwitch(i32, i32, Vec<i32>)
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

            let default = read_u32(bytecode, off + pad + 1)? as i32;
            let len = read_u32(bytecode, off + pad + 5)? as usize;

            let mut lookup = vec![];
            for i in 0..len {
                lookup.push((
                    read_u32(bytecode, off + pad + 9 + (i * 8))? as i32,
                    read_u32(bytecode, off + pad + 13 + (i * 8))? as i32
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

            let default = read_u32(bytecode, off + pad + 1)? as i32;
            let lo = read_u32(bytecode, off + pad + 5)? as i32;
            let hi = read_u32(bytecode, off + pad + 9)? as i32;
            let len = (hi - lo + 1) as usize;

            let mut table = vec![];
            for i in 0..len {
                table.push(
                    read_u32(bytecode, off + pad + 13 + (i * 4))? as i32,
                );
            };

            (BytecodeInstruction::TableSwitch(lo, default, table), pad + 13 + (len * 4))
        },
        0xc4 => match read_u8(bytecode, off + 1)? {
            0x19 => (BytecodeInstruction::ALoad(read_u16(bytecode, off + 1)?), 4),
            0x3a => (BytecodeInstruction::AStore(read_u16(bytecode, off + 1)?), 4),
            0x18 => (BytecodeInstruction::DLoad(read_u16(bytecode, off + 1)?), 4),
            0x39 => (BytecodeInstruction::DStore(read_u16(bytecode, off + 1)?), 4),
            0x17 => (BytecodeInstruction::FLoad(read_u16(bytecode, off + 1)?), 4),
            0x38 => (BytecodeInstruction::FStore(read_u16(bytecode, off + 1)?), 4),
            0x84 => (BytecodeInstruction::IInc(read_u16(bytecode, off + 1)?, read_u8(bytecode, off + 3)? as i16), 6),
            0x15 => (BytecodeInstruction::ILoad(read_u16(bytecode, off + 1)?), 4),
            0x36 => (BytecodeInstruction::IStore(read_u16(bytecode, off + 1)?), 4),
            0x16 => (BytecodeInstruction::LLoad(read_u16(bytecode, off + 1)?), 4),
            0x37 => (BytecodeInstruction::LStore(read_u16(bytecode, off + 1)?), 4),
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
