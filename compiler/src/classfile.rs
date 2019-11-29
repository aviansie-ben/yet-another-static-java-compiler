use std::fmt;
use std::io::{self, Read};
use std::sync::Arc;

use bitflags::bitflags;
use byteorder::{BigEndian, ReadBytesExt};
use lazy_static::lazy_static;

use crate::resolve::{ClassId, ConstantId, FieldId, MethodId};
use crate::layout::ObjectLayout;

bitflags! {
    pub struct ClassFlags: u16 {
        const PUBLIC = 0x0001;
        const FINAL = 0x0010;
        const SUPER = 0x0020;
        const INTERFACE = 0x0200;
        const ABSTRACT = 0x0400;
        const SYNTHETIC = 0x1000;
        const ANNOTATION = 0x2000;
        const ENUM = 0x4000;
    }
}

bitflags! {
    pub struct FieldFlags: u16 {
        const PUBLIC = 0x0001;
        const PRIVATE = 0x0002;
        const PROTECTED = 0x0004;
        const STATIC = 0x0008;
        const FINAL = 0x0010;
        const VOLATILE = 0x0040;
        const TRANSIENT = 0x0080;
        const SYNTHETIC = 0x1000;
        const ENUM = 0x4000;
    }
}

bitflags! {
    pub struct MethodFlags: u16 {
        const PUBLIC = 0x0001;
        const PRIVATE = 0x0002;
        const PROTECTED = 0x0004;
        const STATIC = 0x0008;
        const FINAL = 0x0010;
        const SYNCHRONIZED = 0x0020;
        const BRIDGE = 0x0040;
        const VARARGS = 0x0080;
        const NATIVE = 0x0100;
        const ABSTRACT = 0x0400;
        const STRICT = 0x0800;
        const SYNTHETIC = 0x1000;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveType {
    Byte,
    Char,
    Double,
    Float,
    Int,
    Long,
    Short,
    Boolean
}

impl PrimitiveType {
    pub fn as_char(&self) -> u8 {
        match *self {
            PrimitiveType::Byte => b'B',
            PrimitiveType::Char => b'C',
            PrimitiveType::Double => b'D',
            PrimitiveType::Float => b'F',
            PrimitiveType::Int => b'I',
            PrimitiveType::Long => b'J',
            PrimitiveType::Short => b'S',
            PrimitiveType::Boolean => b'Z'
        }
    }
}

impl fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", char::from(self.as_char()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FlatTypeDescriptor {
    Primitive(PrimitiveType),
    Reference(Arc<str>)
}

impl fmt::Display for FlatTypeDescriptor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            FlatTypeDescriptor::Primitive(t) => write!(f, "{}", t),
            FlatTypeDescriptor::Reference(ref name) => write!(f, "L{};", name)
        }
    }
}

impl FlatTypeDescriptor {
    pub fn parse(s: &str) -> Option<FlatTypeDescriptor> {
        let (d, l) = FlatTypeDescriptor::parse_partial(s)?;

        if l == s.len() {
            Some(d)
        } else {
            None
        }
    }

    fn parse_partial(s: &str) -> Option<(FlatTypeDescriptor, usize)> {
        match s.as_bytes().get(0).as_deref() {
            Some(b'B') => Some((FlatTypeDescriptor::Primitive(PrimitiveType::Byte), 1)),
            Some(b'C') => Some((FlatTypeDescriptor::Primitive(PrimitiveType::Char), 1)),
            Some(b'D') => Some((FlatTypeDescriptor::Primitive(PrimitiveType::Double), 1)),
            Some(b'F') => Some((FlatTypeDescriptor::Primitive(PrimitiveType::Float), 1)),
            Some(b'I') => Some((FlatTypeDescriptor::Primitive(PrimitiveType::Int), 1)),
            Some(b'J') => Some((FlatTypeDescriptor::Primitive(PrimitiveType::Long), 1)),
            Some(b'S') => Some((FlatTypeDescriptor::Primitive(PrimitiveType::Short), 1)),
            Some(b'Z') => Some((FlatTypeDescriptor::Primitive(PrimitiveType::Boolean), 1)),
            Some(b'L') => {
                let mut terminated = false;
                let mut name = String::new();

                for c in s[1..].chars() {
                    if c == ';' {
                        terminated = true;
                        break;
                    } else {
                        name.push(c);
                    };
                };

                if terminated {
                    let len = 2 + name.len();
                    Some((FlatTypeDescriptor::Reference(Arc::from(name.into_boxed_str())), len))
                } else {
                    None
                }
            },
            _ => None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDescriptor {
    pub flat: FlatTypeDescriptor,
    pub array_dims: u8
}

impl TypeDescriptor {
    pub fn parse(s: &str) -> Option<TypeDescriptor> {
        let (d, l) = TypeDescriptor::parse_partial(s)?;

        if l == s.len() {
            Some(d)
        } else {
            None
        }
    }

    fn parse_partial(s: &str) -> Option<(TypeDescriptor, usize)> {
        let mut array_dims = 0_u8;

        while s.as_bytes().get(array_dims as usize) == Some(&b'[') {
            array_dims = array_dims.checked_add(1)?;
        };

        let (flat, flat_len) = FlatTypeDescriptor::parse_partial(&s[(array_dims as usize)..])?;

        Some((TypeDescriptor { flat, array_dims }, array_dims as usize + flat_len))
    }
}

impl fmt::Display for TypeDescriptor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for _ in 0..(self.array_dims) {
            write!(f, "[")?;
        };

        write!(f, "{}", self.flat)?;
        Result::Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodDescriptor {
    pub return_type: Option<TypeDescriptor>,
    pub param_types: Vec<TypeDescriptor>
}

impl fmt::Display for MethodDescriptor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(")?;

        for p in self.param_types.iter() {
            write!(f, "{}", p)?;
        };

        if let Some(ref return_type) = self.return_type {
            write!(f, "){}", return_type)?;
        } else {
            write!(f, ")V")?;
        };

        Result::Ok(())
    }
}

impl MethodDescriptor {
    pub fn parse(s: &str) -> Option<MethodDescriptor> {
        let (d, l) = MethodDescriptor::parse_partial(s)?;

        if l == s.len() {
            Some(d)
        } else {
            None
        }
    }

    fn parse_partial(s: &str) -> Option<(MethodDescriptor, usize)> {
        if s.as_bytes().get(0) != Some(&b'(') {
            return None;
        };

        let mut i = 1;
        let mut param_types = vec![];

        while s.as_bytes().get(i) != Some(&b')') {
            let (param_d, param_len) = TypeDescriptor::parse_partial(&s[i..])?;

            param_types.push(param_d);
            i += param_len;
        };

        i = i + 1;
        let (return_type, return_len) = if s.as_bytes().get(i) == Some(&b'V') {
            (None, 1)
        } else {
            let (return_type, return_len) = TypeDescriptor::parse_partial(&s[i..])?;
            (Some(return_type), return_len)
        };

        Some((MethodDescriptor { return_type, param_types }, i + return_len))
    }
}

#[derive(Debug, Clone)]
pub struct ConstantClass {
    pub name: Arc<str>,
    pub class_id: ClassId,
    pub array_class_id: ClassId
}

#[derive(Debug, Clone)]
pub struct ConstantFieldref {
    pub class: u16,
    pub name: Arc<str>,
    pub descriptor: TypeDescriptor,
    pub field_id: FieldId,
    pub type_id: ClassId
}

#[derive(Debug, Clone)]
pub struct ConstantMethodref {
    pub class: u16,
    pub name: Arc<str>,
    pub descriptor: MethodDescriptor,
    pub method_id: MethodId
}

#[derive(Debug, Clone)]
pub struct ConstantString {
    pub contents: Arc<str>,
    pub index: usize
}

#[derive(Debug, Clone)]
pub struct ConstantMethodHandle {}

#[derive(Debug, Clone)]
pub struct ConstantMethodType {
    descriptor: MethodDescriptor
}

#[derive(Debug, Clone)]
pub struct ConstantInvokeDynamic {
    bootstrap_method_id: u16,
    name: Arc<str>,
    descriptor: MethodDescriptor
}

#[derive(Debug, Clone)]
pub enum ConstantPoolEntry {
    Class(ConstantClass),
    Fieldref(ConstantFieldref),
    Methodref(ConstantMethodref),
    InterfaceMethodref(ConstantMethodref),
    String(ConstantString),
    Integer(i32),
    Float(u32),
    Long(i64),
    Double(u64),
    NameAndType((Arc<str>, Arc<str>)),
    Utf8(Arc<str>),
    MethodHandle(ConstantMethodHandle),
    MethodType(ConstantMethodType),
    InvokeDynamic(ConstantInvokeDynamic),
    Empty
}

#[derive(Debug, Clone)]
enum RawConstantPoolEntry {
    Class { name_index: u16 },
    Fieldref { class_index: u16, name_and_type_index: u16 },
    Methodref { class_index: u16, name_and_type_index: u16 },
    InterfaceMethodref { class_index: u16, name_and_type_index: u16 },
    String { string_index: u16 },
    Integer(i32),
    Float(u32),
    Long(i64),
    Double(u64),
    NameAndType { name_index: u16, type_index: u16 },
    Utf8(Arc<str>),
    MethodHandle { reference_kind: u8, reference_index: u16 },
    MethodType { descriptor_index: u16 },
    InvokeDynamic { bootstrap_method_attr_index: u16, name_and_type_index: u16 },
    Empty
}

impl RawConstantPoolEntry {
    fn needs_next_empty(&self) -> bool {
        match *self {
            RawConstantPoolEntry::Long(_) => true,
            RawConstantPoolEntry::Double(_) => true,
            _ => false
        }
    }
}

#[derive(Debug, Clone)]
pub struct Field {
    pub flags: FieldFlags,
    pub name: Arc<str>,
    pub descriptor: TypeDescriptor,
    pub attributes: Vec<Attribute>,
    pub class_id: ClassId,
    pub off: u32
}

#[derive(Debug, Clone)]
pub struct MethodSummary {
    pub may_virtual_call: Vec<MethodId>,
    pub may_special_call: Vec<MethodId>,
    pub may_construct: Vec<ClassId>,
    pub may_clinit: Vec<ClassId>,
    pub uses_strings: Vec<ConstantId>
}

impl MethodSummary {
    pub fn empty() -> MethodSummary {
        MethodSummary {
            may_virtual_call: vec![],
            may_special_call: vec![],
            may_construct: vec![],
            may_clinit: vec![],
            uses_strings: vec![]
        }
    }
}

#[derive(Debug, Clone)]
pub struct MethodOverrideInfo {
    pub overrides_virtual: MethodId,
    pub overrides_interface: Vec<MethodId>,
    pub overridden_by: Vec<MethodId>
}

impl MethodOverrideInfo {
    pub fn empty() -> MethodOverrideInfo {
        MethodOverrideInfo {
            overrides_virtual: MethodId::UNRESOLVED,
            overrides_interface: vec![],
            overridden_by: vec![]
        }
    }
}

#[derive(Debug, Clone)]
pub struct Method {
    pub flags: MethodFlags,
    pub name: Arc<str>,
    pub descriptor: MethodDescriptor,
    pub param_types: Vec<ClassId>,
    pub return_type: ClassId,
    pub attributes: Vec<Attribute>,
    pub virtual_slot: u32,
    pub summary: MethodSummary,
    pub overrides: MethodOverrideInfo
}

impl Method {
    pub fn code_attribute(&self) -> Option<&AttributeCode> {
        for attr in self.attributes.iter() {
            match attr.data {
                AttributeData::Code(ref code) => {
                    return Some(code);
                },
                _ => {}
            }
        };
        None
    }
}

#[derive(Debug, Clone)]
pub struct ExceptionTableEntry {
    pub start_pc: u16,
    pub end_pc: u16,
    pub handler_pc: u16,
    pub catch_type: u16
}

#[derive(Debug, Clone)]
pub struct AttributeCode {
    pub max_stack: u16,
    pub max_locals: u16,
    pub code: Box<[u8]>,
    pub exception_table: Vec<ExceptionTableEntry>,
    pub attributes: Vec<Attribute>
}

#[derive(Debug, Clone)]
pub enum AttributeData {
    Unknown(Box<[u8]>),
    Code(AttributeCode),
    ConstantData(u16)
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub name: Arc<str>,
    pub data: AttributeData
}

#[derive(Debug, Clone)]
pub struct ClassMeta {
    pub this_id: ClassId,
    pub super_id: ClassId,
    pub interface_ids: Vec<ClassId>,
    pub clinit_method: Option<u16>,

    pub name: Arc<str>
}

#[derive(Debug, Clone)]
pub struct Class {
    pub version: (u16, u16),
    pub constant_pool: Vec<ConstantPoolEntry>,
    pub flags: ClassFlags,
    pub this_class_cp: u16,
    pub super_class_cp: u16,
    pub interfaces: Vec<u16>,
    pub fields: Vec<Field>,
    pub methods: Vec<Method>,
    pub attributes: Vec<Attribute>,
    pub layout: ObjectLayout,

    pub meta: ClassMeta
}

impl Class {
    pub fn dummy_class() -> Class {
        Class {
            version: (0, 0),
            constant_pool: vec![],
            flags: ClassFlags::empty(),
            this_class_cp: 0,
            super_class_cp: 0,
            interfaces: vec![],
            fields: vec![],
            methods: vec![],
            attributes: vec![],
            layout: ObjectLayout::empty(),
            meta: ClassMeta {
                this_id: ClassId::UNRESOLVED,
                super_id: ClassId::UNRESOLVED,
                interface_ids: vec![],
                clinit_method: None,
                name: Arc::from(String::new().into_boxed_str())
            }
        }
    }
}

#[derive(Debug)]
pub enum ClassFileReadError {
    Io(io::Error),
    UnsupportedVersion(u16, u16),
    InvalidMagic,
    InvalidFlags,
    InvalidConstantPoolEntry(u16),
    InvalidField(u16),
    InvalidMethod(u16),
    InvalidAttribute(u16)
}

impl From<io::Error> for ClassFileReadError {
    fn from(err: io::Error) -> ClassFileReadError {
        ClassFileReadError::Io(err)
    }
}

fn parse_raw_constant_pool_entry<R: Read>(r: &mut R, i: u16) -> Result<RawConstantPoolEntry, ClassFileReadError> {
    Result::Ok(match r.read_u8()? {
        7 => RawConstantPoolEntry::Class {
            name_index: r.read_u16::<BigEndian>()?
        },
        9 => RawConstantPoolEntry::Fieldref {
            class_index: r.read_u16::<BigEndian>()?,
            name_and_type_index: r.read_u16::<BigEndian>()?
        },
        10 => RawConstantPoolEntry::Methodref {
            class_index: r.read_u16::<BigEndian>()?,
            name_and_type_index: r.read_u16::<BigEndian>()?
        },
        11 => RawConstantPoolEntry::InterfaceMethodref {
            class_index: r.read_u16::<BigEndian>()?,
            name_and_type_index: r.read_u16::<BigEndian>()?
        },
        8 => RawConstantPoolEntry::String {
            string_index: r.read_u16::<BigEndian>()?
        },
        3 => RawConstantPoolEntry::Integer(r.read_i32::<BigEndian>()?),
        4 => RawConstantPoolEntry::Float(r.read_u32::<BigEndian>()?),
        5 => RawConstantPoolEntry::Long(r.read_i64::<BigEndian>()?),
        6 => RawConstantPoolEntry::Double(r.read_u64::<BigEndian>()?),
        12 => RawConstantPoolEntry::NameAndType {
            name_index: r.read_u16::<BigEndian>()?,
            type_index: r.read_u16::<BigEndian>()?
        },
        1 => {
            let len = r.read_u16::<BigEndian>()? as usize;
            let mut data = vec![0; len];

            r.read_exact(&mut data[..])?;
            match cesu8::from_java_cesu8(&data) {
                Result::Ok(val) => RawConstantPoolEntry::Utf8(Arc::from(val.to_string().into_boxed_str())),
                Result::Err(_) => {
                    return Result::Err(ClassFileReadError::InvalidConstantPoolEntry(i));
                }
            }
        },
        15 => RawConstantPoolEntry::MethodHandle {
            reference_kind: r.read_u8()?,
            reference_index: r.read_u16::<BigEndian>()?
        },
        16 => RawConstantPoolEntry::MethodType {
            descriptor_index: r.read_u16::<BigEndian>()?
        },
        18 => RawConstantPoolEntry::InvokeDynamic {
            bootstrap_method_attr_index: r.read_u16::<BigEndian>()?,
            name_and_type_index: r.read_u16::<BigEndian>()?
        },
        _ => {
            return Result::Err(ClassFileReadError::InvalidConstantPoolEntry(i))
        }
    })
}

fn parse_raw_constant_pool<R: Read>(r: &mut R) -> Result<Vec<RawConstantPoolEntry>, ClassFileReadError> {
    let n = r.read_u16::<BigEndian>()?;
    let mut constant_pool = vec![];
    let mut skip_next = false;

    constant_pool.reserve_exact(n as usize);
    constant_pool.push(RawConstantPoolEntry::Empty);

    for i in 1..n {
        if skip_next {
            constant_pool.push(RawConstantPoolEntry::Empty);
            skip_next = false;
        } else {
            let entry = parse_raw_constant_pool_entry(r, i)?;

            skip_next = entry.needs_next_empty();
            constant_pool.push(entry);
        };
    };

    Result::Ok(constant_pool)
}

fn resolve_utf8(raw_constant_pool: &[RawConstantPoolEntry], i: u16, utf8_index: u16) -> Result<Arc<str>, ClassFileReadError> {
    match raw_constant_pool.get(utf8_index as usize) {
        Some(&RawConstantPoolEntry::Utf8(ref val)) => Result::Ok(val.clone()),
        _ => Result::Err(ClassFileReadError::InvalidConstantPoolEntry(i))
    }
}

fn resolve_name_and_type(raw_constant_pool: &[RawConstantPoolEntry], i: u16, nt_index: u16) -> Result<(Arc<str>, Arc<str>), ClassFileReadError> {
    match raw_constant_pool.get(nt_index as usize) {
        Some(&RawConstantPoolEntry::NameAndType { name_index, type_index }) => Result::Ok((
            resolve_utf8(raw_constant_pool, i, name_index)?,
            resolve_utf8(raw_constant_pool, i, type_index)?
        )),
        _ => Result::Err(ClassFileReadError::InvalidConstantPoolEntry(i))
    }
}

fn process_constant_pool(raw_constant_pool: Vec<RawConstantPoolEntry>) -> Result<Vec<ConstantPoolEntry>, ClassFileReadError> {
    let mut constant_pool = vec![];
    constant_pool.reserve_exact(raw_constant_pool.len());

    for (i, raw_entry) in raw_constant_pool.iter().enumerate() {
        let i = i as u16;
        constant_pool.push(match *raw_entry {
            RawConstantPoolEntry::Class { name_index } => ConstantPoolEntry::Class(ConstantClass {
                name: resolve_utf8(&raw_constant_pool, i, name_index)?,
                class_id: ClassId::UNRESOLVED,
                array_class_id: ClassId::UNRESOLVED
            }),
            RawConstantPoolEntry::Fieldref { class_index, name_and_type_index } => ConstantPoolEntry::Fieldref({
                let (name, descriptor) = resolve_name_and_type(&raw_constant_pool, i, name_and_type_index)?;
                ConstantFieldref {
                    class: class_index,
                    name,
                    descriptor: if let Some(d) = TypeDescriptor::parse(&descriptor) {
                        d
                    } else {
                        return Result::Err(ClassFileReadError::InvalidConstantPoolEntry(i));
                    },
                    field_id: FieldId::UNRESOLVED,
                    type_id: ClassId::UNRESOLVED
                }
            }),
            RawConstantPoolEntry::Methodref { class_index, name_and_type_index } => ConstantPoolEntry::Methodref({
                let (name, descriptor) = resolve_name_and_type(&raw_constant_pool, i, name_and_type_index)?;
                ConstantMethodref {
                    class: class_index,
                    name,
                    descriptor: if let Some(d) = MethodDescriptor::parse(&descriptor) {
                        d
                    } else {
                        return Result::Err(ClassFileReadError::InvalidConstantPoolEntry(i));
                    },
                    method_id: MethodId::UNRESOLVED
                }
            }),
            RawConstantPoolEntry::InterfaceMethodref { class_index, name_and_type_index } => ConstantPoolEntry::InterfaceMethodref({
                let (name, descriptor) = resolve_name_and_type(&raw_constant_pool, i, name_and_type_index)?;
                ConstantMethodref {
                    class: class_index,
                    name,
                    descriptor: if let Some(d) = MethodDescriptor::parse(&descriptor) {
                        d
                    } else {
                        return Result::Err(ClassFileReadError::InvalidConstantPoolEntry(i));
                    },
                    method_id: MethodId::UNRESOLVED
                }
            }),
            RawConstantPoolEntry::String { string_index } => ConstantPoolEntry::String(ConstantString {
                contents: resolve_utf8(&raw_constant_pool, i, string_index)?,
                index: !0
            }),
            RawConstantPoolEntry::Integer(val) => ConstantPoolEntry::Integer(val),
            RawConstantPoolEntry::Float(val) => ConstantPoolEntry::Float(val),
            RawConstantPoolEntry::Long(val) => ConstantPoolEntry::Long(val),
            RawConstantPoolEntry::Double(val) => ConstantPoolEntry::Double(val),
            RawConstantPoolEntry::NameAndType { name_index, type_index } => ConstantPoolEntry::NameAndType((
                resolve_utf8(&raw_constant_pool, i, name_index)?,
                resolve_utf8(&raw_constant_pool, i, type_index)?
            )),
            RawConstantPoolEntry::Utf8(ref val) => ConstantPoolEntry::Utf8(val.clone()),
            RawConstantPoolEntry::MethodHandle { reference_kind, reference_index } => ConstantPoolEntry::MethodHandle({
                // TODO Implement this
                ConstantMethodHandle {}
            }),
            RawConstantPoolEntry::MethodType { descriptor_index } => ConstantPoolEntry::MethodType({
                let descriptor = resolve_utf8(&raw_constant_pool, i, descriptor_index)?;
                let descriptor = if let Some(d) = MethodDescriptor::parse(&descriptor) {
                    d
                } else {
                    return Result::Err(ClassFileReadError::InvalidConstantPoolEntry(i))
                };

                ConstantMethodType { descriptor }
            }),
            RawConstantPoolEntry::InvokeDynamic { bootstrap_method_attr_index, name_and_type_index } => ConstantPoolEntry::InvokeDynamic({
                let (name, descriptor) = resolve_name_and_type(&raw_constant_pool, i, name_and_type_index)?;
                ConstantInvokeDynamic {
                    bootstrap_method_id: bootstrap_method_attr_index,
                    name,
                    descriptor: if let Some(d) = MethodDescriptor::parse(&descriptor) {
                        d
                    } else {
                        return Result::Err(ClassFileReadError::InvalidConstantPoolEntry(i));
                    }
                }
            }),
            RawConstantPoolEntry::Empty => ConstantPoolEntry::Empty
        });
    };

    Result::Ok(constant_pool)
}

fn parse_attribute<R: Read>(cp: &[ConstantPoolEntry], r: &mut R, i: u16) -> Result<Attribute, ClassFileReadError> {
    let name = match cp.get(r.read_u16::<BigEndian>()? as usize) {
        Some(&ConstantPoolEntry::Utf8(ref val)) => val.clone(),
        _ => {
            return Result::Err(ClassFileReadError::InvalidAttribute(i));
        }
    };

    let len = r.read_u32::<BigEndian>()? as usize;
    let data = match &*name {
        "Code" => {
            let max_stack = r.read_u16::<BigEndian>()?;
            let max_locals = r.read_u16::<BigEndian>()?;
            let code_len = r.read_u32::<BigEndian>()?;

            if code_len == 0 || code_len >= 65536 {
                return Result::Err(ClassFileReadError::InvalidAttribute(i));
            };

            let mut code = vec![0_u8; code_len as usize].into_boxed_slice();
            r.read_exact(&mut code)?;

            let exception_table_len = r.read_u16::<BigEndian>()?;
            let mut exception_table = Vec::with_capacity(exception_table_len as usize);

            for _ in 0..exception_table_len {
                exception_table.push(ExceptionTableEntry {
                    start_pc: r.read_u16::<BigEndian>()?,
                    end_pc: r.read_u16::<BigEndian>()?,
                    handler_pc: r.read_u16::<BigEndian>()?,
                    catch_type: r.read_u16::<BigEndian>()?
                });
            };

            let attributes = parse_attributes(cp, r)?;

            AttributeData::Code(AttributeCode {
                max_stack,
                max_locals,
                code,
                exception_table,
                attributes
            })
        },
        "ConstantData" => {
            AttributeData::ConstantData(r.read_u16::<BigEndian>()?)
        },
        _ => {
            let mut data = vec![0_u8; len].into_boxed_slice();
            r.read_exact(&mut data)?;
            AttributeData::Unknown(data)
        }
    };

    Result::Ok(Attribute { name, data })
}

fn parse_attributes<R: Read>(cp: &[ConstantPoolEntry], r: &mut R) -> Result<Vec<Attribute>, ClassFileReadError> {
    let num_attributes = r.read_u16::<BigEndian>()?;
    let mut attributes = Vec::with_capacity(num_attributes as usize);

    for i in 0..num_attributes {
        attributes.push(parse_attribute(cp, r, i)?);
    };

    Result::Ok(attributes)
}

fn parse_field<R: Read>(cp: &[ConstantPoolEntry], r: &mut R, i: u16) -> Result<Field, ClassFileReadError> {
    let flags = if let Some(flags) = FieldFlags::from_bits(r.read_u16::<BigEndian>()?) {
        flags
    } else {
        return Result::Err(ClassFileReadError::InvalidField(i));
    };

    let name = match cp.get(r.read_u16::<BigEndian>()? as usize) {
        Some(&ConstantPoolEntry::Utf8(ref val)) => val.clone(),
        _ => {
            return Result::Err(ClassFileReadError::InvalidField(i));
        }
    };

    let descriptor = match cp.get(r.read_u16::<BigEndian>()? as usize) {
        Some(&ConstantPoolEntry::Utf8(ref val)) => if let Some(d) = TypeDescriptor::parse(val) {
            d
        } else {
            return Result::Err(ClassFileReadError::InvalidField(i));
        },
        _ => {
            return Result::Err(ClassFileReadError::InvalidField(i));
        }
    };

    let attributes = parse_attributes(cp, r)?;

    Result::Ok(Field { flags, name, descriptor, attributes, class_id: ClassId::UNRESOLVED, off: !0 })
}

fn parse_fields<R: Read>(cp: &[ConstantPoolEntry], r: &mut R) -> Result<Vec<Field>, ClassFileReadError> {
    let num_fields = r.read_u16::<BigEndian>()?;
    let mut fields = Vec::with_capacity(num_fields as usize);

    for i in 0..num_fields {
        fields.push(parse_field(cp, r, i)?);
    };

    Result::Ok(fields)
}

fn parse_method<R: Read>(cp: &[ConstantPoolEntry], r: &mut R, i: u16) -> Result<Method, ClassFileReadError> {
    let flags = if let Some(flags) = MethodFlags::from_bits(r.read_u16::<BigEndian>()?) {
        flags
    } else {
        return Result::Err(ClassFileReadError::InvalidMethod(i));
    };

    let name = match cp.get(r.read_u16::<BigEndian>()? as usize) {
        Some(&ConstantPoolEntry::Utf8(ref val)) => val.clone(),
        _ => {
            return Result::Err(ClassFileReadError::InvalidMethod(i));
        }
    };

    let descriptor = match cp.get(r.read_u16::<BigEndian>()? as usize) {
        Some(&ConstantPoolEntry::Utf8(ref val)) => if let Some(d) = MethodDescriptor::parse(val) {
            d
        } else {
            return Result::Err(ClassFileReadError::InvalidMethod(i));
        },
        _ => {
            return Result::Err(ClassFileReadError::InvalidMethod(i));
        }
    };

    let attributes = parse_attributes(cp, r)?;

    Result::Ok(Method {
        flags,
        name,
        descriptor,
        param_types: vec![],
        return_type: ClassId::UNRESOLVED,
        attributes,
        virtual_slot: !0,
        summary: MethodSummary::empty(),
        overrides: MethodOverrideInfo::empty()
    })
}

fn parse_methods<R: Read>(cp: &[ConstantPoolEntry], r: &mut R) -> Result<Vec<Method>, ClassFileReadError> {
    let num_methods = r.read_u16::<BigEndian>()?;
    let mut methods = Vec::with_capacity(num_methods as usize);

    for i in 0..num_methods {
        methods.push(parse_method(cp, r, i)?);
    };

    Result::Ok(methods)
}

lazy_static! {
    static ref NULLARY_METHOD_DESCRIPTOR: MethodDescriptor = MethodDescriptor {
        return_type: None,
        param_types: vec![]
    };
}

fn find_clinit_method(methods: &[Method]) -> Option<u16> {
    methods.iter().enumerate()
        .filter(|(_, m)| m.name.as_ref() == "<clinit>" && m.descriptor == *NULLARY_METHOD_DESCRIPTOR)
        .map(|(i, _)| i as u16)
        .next()
}

pub fn parse_class_file<R: Read>(r: &mut R) -> Result<Class, ClassFileReadError> {
    if r.read_u32::<BigEndian>()? != 0xcafebabe {
        return Result::Err(ClassFileReadError::InvalidMagic);
    };

    let version_minor = r.read_u16::<BigEndian>()?;
    let version_major = r.read_u16::<BigEndian>()?;

    if version_major != 52 || version_minor != 0 {
        return Result::Err(ClassFileReadError::UnsupportedVersion(version_major, version_minor));
    };

    let constant_pool = process_constant_pool(parse_raw_constant_pool(r)?)?;

    let flags = if let Some(flags) = ClassFlags::from_bits(r.read_u16::<BigEndian>()?) {
        flags
    } else {
        return Result::Err(ClassFileReadError::InvalidFlags);
    };

    let this_class_cp = r.read_u16::<BigEndian>()?;
    let super_class_cp = r.read_u16::<BigEndian>()?;

    let num_interfaces = r.read_u16::<BigEndian>()?;
    let mut interfaces = Vec::with_capacity(num_interfaces as usize);

    for _ in 0..num_interfaces {
        interfaces.push(r.read_u16::<BigEndian>()?);
    };

    let fields = parse_fields(&constant_pool, r)?;
    let methods = parse_methods(&constant_pool, r)?;
    let attributes = parse_attributes(&constant_pool, r)?;

    let clinit_method = find_clinit_method(&methods);

    Result::Ok(Class {
        version: (version_major, version_minor),
        constant_pool,
        flags,
        this_class_cp,
        super_class_cp,
        interfaces,
        fields,
        methods,
        attributes,
        layout: ObjectLayout::empty(),
        meta: ClassMeta {
            this_id: ClassId::UNRESOLVED,
            super_id: ClassId::UNRESOLVED,
            interface_ids: vec![],
            clinit_method,
            name: Arc::from(String::new().into_boxed_str())
        }
    })
}
