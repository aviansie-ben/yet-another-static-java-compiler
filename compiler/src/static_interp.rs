use std::alloc::AllocErr;
use std::collections::HashMap;
use std::fmt;

use lazy_static::lazy_static;

use crate::bytecode::{BytecodeCondition, BytecodeInstruction, BytecodeIterator};
use crate::classfile::{AttributeData, Class, ConstantPoolEntry, FieldFlags, FlatTypeDescriptor, MethodFlags, MethodSummary, PrimitiveType};
use crate::resolve::{ClassEnvironment, ClassId, ConstantId, FieldId, MethodId, ResolvedClass};
use crate::layout;
use crate::static_heap::{self, ObjectFlags, JavaStaticHeap, JavaStaticRef};

fn add_may_virtual_call(summary: &mut MethodSummary, method_id: MethodId) {
    if method_id.0 != ClassId::UNRESOLVED && !summary.may_virtual_call.contains(&method_id) {
        summary.may_virtual_call.push(method_id);
    };
}

fn add_may_special_call(summary: &mut MethodSummary, method_id: MethodId) {
    if method_id.0 != ClassId::UNRESOLVED && !summary.may_special_call.contains(&method_id) {
        summary.may_special_call.push(method_id);
    };
}

fn add_may_construct(summary: &mut MethodSummary, class_id: ClassId) {
    if class_id != ClassId::UNRESOLVED && !summary.may_construct.contains(&class_id) {
        summary.may_construct.push(class_id);
    };
}

fn add_may_clinit(summary: &mut MethodSummary, class_id: ClassId) {
    if class_id != ClassId::UNRESOLVED && !summary.may_clinit.contains(&class_id) {
        summary.may_clinit.push(class_id);
    };
}

pub fn summarize_bytecode(instrs: BytecodeIterator, method_id: MethodId, cp: &[ConstantPoolEntry]) -> MethodSummary {
    let mut summary = MethodSummary {
        may_virtual_call: vec![],
        may_special_call: vec![],
        may_construct: vec![],
        may_clinit: vec![],
        uses_strings: vec![]
    };

    for (_, instr) in instrs {
        match instr.unwrap() {
            BytecodeInstruction::ANewArray(cpe) => {
                let cpe = match cp[cpe as usize] {
                    ConstantPoolEntry::Class(ref cpe) => cpe,
                    _ => unreachable!()
                };

                add_may_construct(&mut summary, cpe.array_class_id);
            },
            BytecodeInstruction::GetField(cpe) => {
                // TODO
            },
            BytecodeInstruction::GetStatic(cpe) => {
                let cpe = match cp[cpe as usize] {
                    ConstantPoolEntry::Fieldref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                add_may_clinit(&mut summary, cpe.field_id.0);
            },
            BytecodeInstruction::InvokeDynamic(_) => {
                // TODO
            },
            BytecodeInstruction::InvokeInterface(cpe, _) => {
                let cpe = match cp[cpe as usize] {
                    ConstantPoolEntry::InterfaceMethodref(ref cpe) => cpe,
                    ConstantPoolEntry::Methodref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                add_may_virtual_call(&mut summary, cpe.method_id);
            },
            BytecodeInstruction::InvokeSpecial(cpe) => {
                let cpe = match cp[cpe as usize] {
                    ConstantPoolEntry::InterfaceMethodref(ref cpe) => cpe,
                    ConstantPoolEntry::Methodref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                add_may_special_call(&mut summary, cpe.method_id);
            },
            BytecodeInstruction::InvokeStatic(cpe) => {
                let cpe = match cp[cpe as usize] {
                    ConstantPoolEntry::InterfaceMethodref(ref cpe) => cpe,
                    ConstantPoolEntry::Methodref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                add_may_clinit(&mut summary, cpe.method_id.0);
                add_may_special_call(&mut summary, cpe.method_id);
            },
            BytecodeInstruction::InvokeVirtual(cpe) => {
                let cpe = match cp[cpe as usize] {
                    ConstantPoolEntry::InterfaceMethodref(ref cpe) => cpe,
                    ConstantPoolEntry::Methodref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                add_may_virtual_call(&mut summary, cpe.method_id);
            },
            BytecodeInstruction::Ldc(idx) | BytecodeInstruction::Ldc2(idx) => {
                match cp[idx as usize] {
                    ConstantPoolEntry::Class(ref cpe) => {
                        add_may_clinit(&mut summary, cpe.class_id);
                    },
                    ConstantPoolEntry::String(ref cpe) => {
                        if !summary.uses_strings.contains(&ConstantId(method_id.0, idx)) {
                            summary.uses_strings.push(ConstantId(method_id.0, idx));
                        };
                    },
                    _ => {}
                };
            },
            BytecodeInstruction::MultiANewArray(_, _) => {
                // TODO
            },
            BytecodeInstruction::New(cpe) => {
                let cpe = match cp[cpe as usize] {
                    ConstantPoolEntry::Class(ref cpe) => cpe,
                    _ => unreachable!()
                };

                add_may_clinit(&mut summary, cpe.class_id);
                add_may_construct(&mut summary, cpe.class_id);
            },
            BytecodeInstruction::NewArray(_) => {
                // TODO
            },
            BytecodeInstruction::PutField(cpe) => {
                // TODO
            },
            BytecodeInstruction::PutStatic(cpe) => {
                let cpe = match cp[cpe as usize] {
                    ConstantPoolEntry::Fieldref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                add_may_clinit(&mut summary, cpe.field_id.0);
            },
            _ => {}
        };
    };

    summary
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value<'a> {
    Int(i32),
    Long(i64),
    Float(u32),
    Double(u64),
    Ref(Option<JavaStaticRef<'a>>),
    ReturnAddress(MethodId, usize, usize),
    Empty
}

impl <'a> Value<'a> {
    pub fn as_int(&self) -> Option<i32> {
        match *self {
            Value::Int(val) => Some(val),
            _ => None
        }
    }

    pub fn as_long(&self) -> Option<i64> {
        match *self {
            Value::Long(val) => Some(val),
            _ => None
        }
    }

    pub fn as_float(&self) -> Option<u32> {
        match *self {
            Value::Float(val) => Some(val),
            _ => None
        }
    }

    pub fn as_double(&self) -> Option<u64> {
        match *self {
            Value::Double(val) => Some(val),
            _ => None
        }
    }

    pub fn as_ref(&self) -> Option<Option<&JavaStaticRef<'a>>> {
        match *self {
            Value::Ref(ref val) => Some(val.as_ref()),
            _ => None
        }
    }

    pub fn into_ref(self) -> Result<Option<JavaStaticRef<'a>>, Value<'a>> {
        match self {
            Value::Ref(java_ref) => Result::Ok(java_ref),
            val => Result::Err(val)
        }
    }

    pub fn as_return_address(&self) -> Option<(MethodId, usize, usize)> {
        match *self {
            Value::ReturnAddress(method, off, drop_locals) => Some((method, off, drop_locals)),
            _ => None
        }
    }

    pub fn needs_dual_slot(&self) -> bool {
        match *self {
            Value::Long(_) => true,
            Value::Double(_) => true,
            _ => false
        }
    }
}

impl <'a> fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Int(val) => {
                write!(f, "Int({} [0x{:08x}])", val, val)?;
            },
            Value::Long(val) => {
                write!(f, "Long({} [0x{:016x}])", val, val)?;
            },
            Value::Float(val) => {
                write!(f, "Float({} [0x{:08x}])", f32::from_bits(val), val)?;
            },
            Value::Double(val) => {
                write!(f, "Double({} [0x{:016x}])", f64::from_bits(val), val)?;
            },
            Value::Ref(None) => {
                write!(f, "Ref(null)")?;
            },
            Value::Ref(Some(ref val)) => {
                write!(f, "Ref({})", val)?;
            },
            Value::ReturnAddress(method_id, off, drop_locals) => {
                write!(f, "ReturnAddress({:?}, {}, {})", method_id, off, drop_locals)?;
            },
            Value::Empty => {
                write!(f, "Empty")?;
            }
        };
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum StaticInterpretError {
    UnimplementedBytecode(MethodId, BytecodeInstruction),
    UnknownNativeCall(MethodId),
    OutOfMemory,
    WouldThrowException(ClassId),
    ExcludedClinit(ClassId)
}

impl From<AllocErr> for StaticInterpretError {
    fn from(_: AllocErr) -> Self {
        StaticInterpretError::OutOfMemory
    }
}

fn constant_from_cpe<'a>(cpe: &ConstantPoolEntry, heap: &'a JavaStaticHeap) -> Value<'a> {
    match *cpe {
        ConstantPoolEntry::Class(ref cpe) => Value::Ref(Some(heap.get_class_object(cpe.class_id))),
        ConstantPoolEntry::String(ref cpe) => Value::Ref(Some(heap.get_constant_string(cpe.index))),
        ConstantPoolEntry::Integer(val) => Value::Int(val),
        ConstantPoolEntry::Float(val) => Value::Float(val),
        ConstantPoolEntry::Long(val) => Value::Long(val),
        ConstantPoolEntry::Double(val) => Value::Double(val),
        _ => unreachable!()
    }
}

fn do_pre_clinit(env: &ClassEnvironment, heap: &JavaStaticHeap, class_id: ClassId, verbose: bool) -> Result<(), StaticInterpretError> {
    let class = match **env.get(class_id) {
        ResolvedClass::User(ref class) => class,
        _ => {
            return Result::Ok(());
        }
    };

    let class_obj = heap.get_class_object(class_id);

    for (i, field) in class.fields.iter().enumerate() {
        for attr in field.attributes.iter() {
            match attr.data {
                AttributeData::ConstantData(cpe) if field.flags.contains(FieldFlags::STATIC) => {
                    let value = constant_from_cpe(&class.constant_pool[cpe as usize], heap);

                    if verbose {
                        eprintln!("  Initializing constant value on static field {}.{} {} to {:?}", class.meta.name, field.name, field.descriptor, value);
                    };

                    class_obj.write_field(FieldId(class_id, i as u16), value);
                },
                _ => {}
            }
        };
    };

    Result::Ok(())
}

fn native_nop(_state: &mut InterpreterState) -> Result<(), StaticInterpretError> {
    Result::Ok(())
}

fn native_get_primitive_class(state: &mut InterpreterState) -> Result<(), StaticInterpretError> {
    let name = state.stack.pop().into_ref().unwrap();

    if let Some(name) = name {
        let name = name.read_string();

        let result = match name.as_str() {
            "void" => state.heap.get_class_object(ClassId::PRIMITIVE_VOID),
            "byte" => state.heap.get_class_object(ClassId::PRIMITIVE_BYTE),
            "char" => state.heap.get_class_object(ClassId::PRIMITIVE_CHAR),
            "double" => state.heap.get_class_object(ClassId::PRIMITIVE_DOUBLE),
            "float" => state.heap.get_class_object(ClassId::PRIMITIVE_FLOAT),
            "int" => state.heap.get_class_object(ClassId::PRIMITIVE_INT),
            "long" => state.heap.get_class_object(ClassId::PRIMITIVE_LONG),
            "short" => state.heap.get_class_object(ClassId::PRIMITIVE_SHORT),
            "boolean" => state.heap.get_class_object(ClassId::PRIMITIVE_BOOLEAN),
            _ => {
                eprintln!("WARNING: Call to Class.getPrimitiveClass with unknown type {:?}", name);
                return Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT));
            }
        };

        state.stack.push(Value::Ref(Some(result)));
        Result::Ok(())
    } else {
        Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT))
    }
}

fn native_class_get_component_type(state: &mut InterpreterState) -> Result<(), StaticInterpretError> {
    let class_id = ClassId(state.stack.pop().as_int().unwrap() as u32);

    state.stack.push(Value::Int(match **state.env.get(class_id) {
        ResolvedClass::Array(class_id) => class_id.0 as i32,
        ref class => {
            eprintln!("WARNING: Call to Class.getComponentType0 with non array type {}", class.name(state.env));
            return Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT));
        }
    }));
    Result::Ok(())
}

fn native_class_is_array(state: &mut InterpreterState) -> Result<(), StaticInterpretError> {
    let class_id = ClassId(state.stack.pop().as_int().unwrap() as u32);

    state.stack.push(Value::Int(match **state.env.get(class_id) {
        ResolvedClass::Array(_) => 1,
        _ => 0
    }));
    Result::Ok(())
}

fn native_class_is_primitive(state: &mut InterpreterState) -> Result<(), StaticInterpretError> {
    let class_id = ClassId(state.stack.pop().as_int().unwrap() as u32);

    state.stack.push(Value::Int(match **state.env.get(class_id) {
        ResolvedClass::Primitive(_) => 1,
        _ => 0
    }));
    Result::Ok(())
}

fn native_class_from_vtable(state: &mut InterpreterState) -> Result<(), StaticInterpretError> {
    let class_id = ClassId(state.stack.pop().as_int().unwrap() as u32);

    if class_id != ClassId::UNRESOLVED {
        state.stack.push(Value::Ref(Some(
            state.heap.get_class_object(class_id)
        )));
    } else {
        state.stack.push(Value::Ref(None));
    };

    Result::Ok(())
}

fn native_get_raw_float_bits(state: &mut InterpreterState) -> Result<(), StaticInterpretError> {
    let val = state.stack.pop().as_float().unwrap();
    state.stack.push(Value::Int(val as i32));
    Result::Ok(())
}

fn native_float_from_bits(state: &mut InterpreterState) -> Result<(), StaticInterpretError> {
    let val = state.stack.pop().as_int().unwrap();
    state.stack.push(Value::Float(val as u32));
    Result::Ok(())
}

fn native_get_raw_double_bits(state: &mut InterpreterState) -> Result<(), StaticInterpretError> {
    let val = state.stack.pop().as_double().unwrap();
    state.stack.push(Value::Long(val as i64));
    Result::Ok(())
}

fn native_double_from_bits(state: &mut InterpreterState) -> Result<(), StaticInterpretError> {
    let val = state.stack.pop().as_long().unwrap();
    state.stack.push(Value::Double(val as u64));
    Result::Ok(())
}

fn native_object_get_class(state: &mut InterpreterState) -> Result<(), StaticInterpretError> {
    let val = state.stack.pop().into_ref().unwrap();

    if let Some(val) = val {
        state.stack.push(Value::Ref(Some(state.heap.get_class_object(val.class_id()))));
        Result::Ok(())
    } else {
        Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT))
    }
}

fn native_arraycopy(state: &mut InterpreterState) -> Result<(), StaticInterpretError> {
    let len = state.stack.pop().as_int().unwrap();
    let dst_off = state.stack.pop().as_int().unwrap();
    let dst = if let Some(dst) = state.stack.pop().into_ref().unwrap() {
        dst
    } else {
        return Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT));
    };
    let src_off = state.stack.pop().as_int().unwrap();
    let src = if let Some(src) = state.stack.pop().into_ref().unwrap() {
        src
    } else {
        return Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT));
    };

    // TODO Check type correctness

    if dst_off < 0 || dst_off > dst.read_array_length() || dst.read_array_length() - dst_off < len {
        return Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT));
    };

    if src_off < 0 || src_off > src.read_array_length() || src.read_array_length() - src_off < len {
        return Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT));
    };

    if src_off > dst_off {
        for i in 0..len {
            dst.write_array_element(dst_off + len - i - 1, src.read_array_element(src_off + len - i - 1));
        };
    } else {
        for i in 0..len {
            dst.write_array_element(dst_off + i, src.read_array_element(src_off + i));
        };
    };

    Result::Ok(())
}

fn native_unsafe_get_array_base(state: &mut InterpreterState) -> Result<(), StaticInterpretError> {
    let class_obj = if let Some(class_obj) = state.stack.pop().into_ref().unwrap() {
        class_obj
    } else {
        return Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT));
    };
    let class_id = ClassId(class_obj.read_field(static_heap::JAVA_LANG_CLASS_VTABLE_PTR_FIELD).as_int().unwrap() as u32);

    if state.stack.pop().into_ref().unwrap().is_none() {
        return Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT));
    };

    let elem_class_id = if let ResolvedClass::Array(elem_class_id) = **state.env.get(class_id) {
        elem_class_id
    } else {
        return Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT));
    };

    state.stack.push(Value::Int(
        layout::get_array_header_size(layout::get_field_size_align(state.env, elem_class_id).1) as i32
    ));

    Result::Ok(())
}

fn native_unsafe_get_array_scale(state: &mut InterpreterState) -> Result<(), StaticInterpretError> {
    let class_obj = if let Some(class_obj) = state.stack.pop().into_ref().unwrap() {
        class_obj
    } else {
        return Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT));
    };
    let class_id = ClassId(class_obj.read_field(static_heap::JAVA_LANG_CLASS_VTABLE_PTR_FIELD).as_int().unwrap() as u32);

    if state.stack.pop().into_ref().unwrap().is_none() {
        return Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT));
    };

    let elem_class_id = if let ResolvedClass::Array(elem_class_id) = **state.env.get(class_id) {
        elem_class_id
    } else {
        return Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT));
    };

    state.stack.push(Value::Int(
        layout::get_field_size_align(state.env, elem_class_id).0 as i32
    ));

    Result::Ok(())
}

fn native_unsafe_address_size(state: &mut InterpreterState) -> Result<(), StaticInterpretError> {
    if state.stack.pop().into_ref().unwrap().is_none() {
        return Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT));
    };

    state.stack.push(Value::Int(8));
    Result::Ok(())
}

fn native_new_array(state: &mut InterpreterState) -> Result<(), StaticInterpretError> {
    let len = state.stack.pop().as_int().unwrap();
    let class_obj = state.stack.pop().into_ref().unwrap();

    if len < 0 {
        return Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT));
    };

    let class_obj = if let Some(class_obj) = class_obj {
        class_obj
    } else {
        return Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT));
    };

    let class_id = ClassId(class_obj.read_field(static_heap::JAVA_LANG_CLASS_VTABLE_PTR_FIELD).as_int().unwrap() as u32);

    if let Some(class_id) = state.env.try_find_array(class_id) {
        state.stack.push(Value::Ref(Some(state.heap.allocate_array(class_id, len as u32)?)));
        Ok(())
    } else {
        Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT))
    }
}

type StaticNative = fn (&mut InterpreterState) -> Result<(), StaticInterpretError>;

lazy_static! {
    static ref KNOWN_NATIVES: HashMap<&'static str, StaticNative> = {
        let mut known_natives = HashMap::new();

        known_natives.insert("java/lang/Object.registerNatives()V", native_nop as StaticNative);
        known_natives.insert("java/lang/Thread.registerNatives()V", native_nop as StaticNative);
        known_natives.insert("java/lang/System.registerNatives()V", native_nop as StaticNative);
        known_natives.insert("sun/misc/Unsafe.registerNatives()V", native_nop as StaticNative);

        known_natives.insert("java/io/FileInputStream.initIDs()V", native_nop as StaticNative);
        known_natives.insert("java/io/FileOutputStream.initIDs()V", native_nop as StaticNative);
        known_natives.insert("java/io/FileDescriptor.initIDs()V", native_nop as StaticNative);
        known_natives.insert("java/io/UnixFileSystem.initIDs()V", native_nop as StaticNative);

        known_natives.insert(
            "sun/misc/Unsafe.arrayBaseOffset(Ljava/lang/Class;)I",
            native_unsafe_get_array_base as StaticNative
        );
        known_natives.insert(
            "sun/misc/Unsafe.arrayIndexScale(Ljava/lang/Class;)I",
            native_unsafe_get_array_scale as StaticNative
        );
        known_natives.insert(
            "sun/misc/Unsafe.addressSize()I",
            native_unsafe_address_size as StaticNative
        );

        known_natives.insert(
            "java/lang/Object.getClass()Ljava/lang/Class;",
            native_object_get_class as StaticNative
        );

        known_natives.insert(
            "java/lang/System.arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V",
            native_arraycopy as StaticNative
        );

        known_natives.insert(
            "java/lang/Class.getPrimitiveClass(Ljava/lang/String;)Ljava/lang/Class;",
            native_get_primitive_class as StaticNative
        );
        known_natives.insert(
            "java/lang/Class.getClassForVTable(I)Ljava/lang/Class;",
            native_class_from_vtable as StaticNative
        );
        known_natives.insert(
            "java/lang/Class.getComponentType0(I)I",
            native_class_get_component_type as StaticNative
        );
        known_natives.insert(
            "java/lang/Class.isArray0(I)Z",
            native_class_is_array as StaticNative
        );
        known_natives.insert(
            "java/lang/Class.isPrimitive0(I)Z",
            native_class_is_primitive as StaticNative
        );

        known_natives.insert(
            "java/lang/reflect/Array.newArray(Ljava/lang/Class;I)Ljava/lang/Object;",
            native_new_array as StaticNative
        );

        known_natives.insert(
            "java/lang/Float.floatToRawIntBits(F)I",
            native_get_raw_float_bits as StaticNative
        );
        known_natives.insert(
            "java/lang/Float.intBitsToFloat(I)F",
            native_float_from_bits as StaticNative
        );
        known_natives.insert(
            "java/lang/Double.doubleToRawLongBits(D)J",
            native_get_raw_double_bits as StaticNative
        );
        known_natives.insert(
            "java/lang/Double.longBitsToDouble(J)D",
            native_double_from_bits as StaticNative
        );

        known_natives.insert(
            "java/lang/String.intern()Ljava/lang/String;",
            // TODO Actually implement this
            native_nop as StaticNative
        );

        known_natives
    };
}

struct InterpreterStack<'b> {
    stack: Vec<Value<'b>>
}

impl <'b> InterpreterStack<'b> {
    fn new() -> InterpreterStack<'b> {
        InterpreterStack { stack: vec![] }
    }

    fn push_slot(&mut self, val: Value<'b>) {
        self.stack.push(val);
    }

    fn push(&mut self, val: Value<'b>) {
        let needs_dual_slot = val.needs_dual_slot();
        self.push_slot(val);
        if needs_dual_slot {
            self.push_slot(Value::Empty);
        };
    }

    fn pop_slot(&mut self) -> Value<'b> {
        self.stack.pop().unwrap()
    }

    fn pop(&mut self) -> Value<'b> {
        match self.pop_slot() {
            Value::Empty => self.pop_slot(),
            val => val
        }
    }

    fn peek(&self) -> &Value<'b> {
        self.read(0)
    }

    fn read(&self, depth: usize) -> &Value<'b> {
        &self.stack[self.len() - 1 - depth]
    }

    fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }

    fn len(&self) -> usize {
        self.stack.len()
    }
}

struct InterpreterState<'a, 'b> {
    env: &'a ClassEnvironment,
    heap: &'a JavaStaticHeap<'b>,
    method_id: MethodId,
    class: &'a Class,
    instrs: BytecodeIterator<'a>,

    stack: InterpreterStack<'b>,
    locals: Vec<Value<'b>>
}

impl <'a, 'b> InterpreterState<'a, 'b> {
    fn set_local(&mut self, idx: usize, val: Value<'b>) {
        let idx = self.locals.len() - idx - 1;
        self.locals[idx] = val;
    }

    fn get_local(&mut self, idx: usize) -> &Value<'b> {
        let idx = self.locals.len() - idx - 1;
        &self.locals[idx]
    }

    fn exit_method(&mut self, verbose: bool) -> Result<bool, StaticInterpretError> {
        if self.stack.is_empty() {
            return Result::Ok(true);
        };

        let (method_id, off, drop_locals) = self.stack.pop().as_return_address().unwrap();
        let (class, method) = self.env.get_method(method_id);

        if verbose {
            eprintln!("    RETURN TO {}.{}{}", class.meta.name, method.name, method.descriptor);
        };

        for _ in 0..drop_locals {
            self.locals.pop();
        };

        self.class = class;
        self.method_id = method_id;
        self.instrs = BytecodeIterator(&method.code_attribute().unwrap().code, off);

        Result::Ok(false)
    }

    fn find_virtual_target(&mut self, method_id: MethodId, receiver: JavaStaticRef<'b>, verbose: bool) -> Result<MethodId, StaticInterpretError> {
        let decl_method = self.env.get_method(method_id).1;

        Result::Ok(if decl_method.virtual_slot == !0 {
            method_id
        } else {
            match receiver.class() {
                ResolvedClass::User(ref recv_class) => {
                    recv_class.layout.virtual_slots[decl_method.virtual_slot as usize]
                },
                _ => method_id
            }
        })
    }

    fn find_interface_target(&mut self, method_id: MethodId, receiver: JavaStaticRef<'b>, verbose: bool) -> Result<MethodId, StaticInterpretError> {
        let decl_method = self.env.get_method(method_id).1;

        Result::Ok(match receiver.class() {
            ResolvedClass::User(ref recv_class) => {
                recv_class.layout.interface_slots.iter()
                    .filter(|&&(interface_id, _)| interface_id == method_id.0)
                    .next().unwrap().1[decl_method.virtual_slot as usize]
            },
            _ => method_id
        })
    }

    fn find_receiver(&self, method_id: MethodId, verbose: bool) -> Result<Option<&JavaStaticRef<'b>>, StaticInterpretError> {
        let decl_method = self.env.get_method(method_id).1;

        let num_param_slots = decl_method.descriptor.param_types.iter().map(|t| {
            if t.array_dims == 0 {
                match t.flat {
                    FlatTypeDescriptor::Primitive(PrimitiveType::Long) => 2,
                    FlatTypeDescriptor::Primitive(PrimitiveType::Double) => 2,
                    _ => 1
                }
            } else {
                1
            }
        }).sum();

        Result::Ok(self.stack.read(num_param_slots).as_ref().unwrap())
    }

    fn enter_method(&mut self, method_id: MethodId, verbose: bool) -> Result<(), StaticInterpretError> {
        try_run_clinit_without_checkpoint(self.env, self.heap, method_id.0, verbose)?;

        let (class, method) = self.env.get_method(method_id);

        if !method.flags.contains(MethodFlags::NATIVE) {
            if verbose {
                eprintln!("    CALL {}.{}{}", class.meta.name, method.name, method.descriptor);
            };

            let code = method.code_attribute().unwrap();

            let mut num_param_slots = method.descriptor.param_types.iter().map(|t| {
                if t.array_dims == 0 {
                    match t.flat {
                        FlatTypeDescriptor::Primitive(PrimitiveType::Long) => 2,
                        FlatTypeDescriptor::Primitive(PrimitiveType::Double) => 2,
                        _ => 1
                    }
                } else {
                    1
                }
            }).sum();

            if !method.flags.contains(MethodFlags::STATIC) {
                num_param_slots += 1;
            };

            if verbose {
                for i in 0..num_param_slots {
                    eprintln!("      {}", self.stack.read(i));
                };
            };

            for _ in num_param_slots..(code.max_locals as usize) {
                self.locals.push(Value::Empty);
            };

            for _ in 0..num_param_slots {
                self.locals.push(self.stack.pop_slot());
            };

            self.stack.push(Value::ReturnAddress(self.method_id, self.instrs.1, code.max_locals as usize));

            self.class = class;
            self.method_id = method_id;
            self.instrs = BytecodeIterator::for_code(code);

            Result::Ok(())
        } else {
            let name = format!("{}.{}{}", class.meta.name, method.name, method.descriptor);

            if verbose {
                eprintln!("    CALL NATIVE {}", name);
            };

            if let Some(native) = KNOWN_NATIVES.get(&name.as_str()) {
                native(self)
            } else {
                Result::Err(StaticInterpretError::UnknownNativeCall(method_id))
            }
        }
    }
}

fn do_compare_int(i: i32, j: i32, cond: BytecodeCondition) -> bool {
    match cond {
        BytecodeCondition::Eq => i == j,
        BytecodeCondition::Ne => i != j,
        BytecodeCondition::Lt => i < j,
        BytecodeCondition::Ge => i >= j,
        BytecodeCondition::Gt => i > j,
        BytecodeCondition::Le => i <= j
    }
}

fn do_compare_ref<'a>(i: Option<JavaStaticRef<'a>>, j: Option<JavaStaticRef<'a>>, cond: BytecodeCondition) -> bool {
    match cond {
        BytecodeCondition::Eq => i == j,
        BytecodeCondition::Ne => i != j,
        _ => unreachable!()
    }
}

fn try_interpret(env: &ClassEnvironment, heap: &JavaStaticHeap, method_id: MethodId, verbose: bool) -> Result<(), StaticInterpretError> {
    let (class, method) = env.get_method(method_id);
    let code = match method.code_attribute() {
        Some(code) => code,
        None => {
            return Result::Err(StaticInterpretError::UnknownNativeCall(method_id));
        }
    };
    let instrs = BytecodeIterator::for_code(code);

    let mut state = InterpreterState {
        env,
        heap,
        method_id,
        class,
        instrs,
        stack: InterpreterStack::new(),
        locals: vec![]
    };

    if verbose {
        eprintln!("  Interpreting {}.{}{}", class.meta.name, method.name, method.descriptor);
    };

    for _ in 0..(code.max_locals as usize) {
        state.locals.push(Value::Ref(None));
    };

    while let Some((_, instr)) = state.instrs.next() {
        let instr = instr.unwrap();
        match instr {
            BytecodeInstruction::Ldc(idx) | BytecodeInstruction::Ldc2(idx) => {
                state.stack.push(constant_from_cpe(&state.class.constant_pool[idx as usize], heap));
            },
            BytecodeInstruction::AConstNull => {
                state.stack.push(Value::Ref(None));
            },
            BytecodeInstruction::IConst(val) => {
                state.stack.push(Value::Int(val));
            },
            BytecodeInstruction::LConst(val) => {
                state.stack.push(Value::Long(val));
            },
            BytecodeInstruction::FConst(val) => {
                state.stack.push(Value::Float(val));
            },
            BytecodeInstruction::DConst(val) => {
                state.stack.push(Value::Double(val));
            },
            BytecodeInstruction::Dup => {
                let val = state.stack.peek().clone();
                state.stack.push(val);
            },
            BytecodeInstruction::DupX1 => {
                let val1 = state.stack.pop_slot();
                let val2 = state.stack.pop_slot();

                state.stack.push_slot(val1.clone());
                state.stack.push_slot(val2);
                state.stack.push_slot(val1);
            },
            BytecodeInstruction::Dup2 => {
                let val1 = state.stack.read(0).clone();
                let val2 = state.stack.read(1).clone();

                state.stack.push_slot(val2);
                state.stack.push_slot(val1);
            },
            BytecodeInstruction::Pop => {
                state.stack.pop_slot();
            },
            BytecodeInstruction::ALoad(idx) | BytecodeInstruction::DLoad(idx) | BytecodeInstruction::FLoad(idx) |
            BytecodeInstruction::ILoad(idx) | BytecodeInstruction::LLoad(idx) => {
                let val = state.get_local(idx as usize).clone();
                state.stack.push(val);
            },
            BytecodeInstruction::AStore(idx) | BytecodeInstruction::DStore(idx) | BytecodeInstruction::FStore(idx) |
            BytecodeInstruction::IStore(idx) | BytecodeInstruction::LStore(idx) => {
                let val = state.stack.pop();
                state.set_local(idx as usize, val);
            },
            BytecodeInstruction::IInc(idx, inc) => {
                let val = state.get_local(idx as usize).as_int().unwrap() + inc as i32;
                state.set_local(idx as usize, Value::Int(val));
            },
            BytecodeInstruction::I2B => {
                let val = state.stack.pop().as_int().unwrap();
                state.stack.push(Value::Int(val as i8 as i32));
            },
            BytecodeInstruction::I2C => {
                let val = state.stack.pop().as_int().unwrap();
                state.stack.push(Value::Int(val & 0xffff));
            },
            BytecodeInstruction::I2S => {
                let val = state.stack.pop().as_int().unwrap();
                state.stack.push(Value::Int(val as i16 as i32));
            },
            BytecodeInstruction::I2L => {
                let val = state.stack.pop().as_int().unwrap();
                state.stack.push(Value::Long(val as i64));
            },
            BytecodeInstruction::L2I => {
                let val = state.stack.pop().as_long().unwrap();
                state.stack.push(Value::Int(val as i32));
            },
            BytecodeInstruction::I2F => {
                let val = state.stack.pop().as_int().unwrap();
                state.stack.push(Value::Float((val as f32).to_bits()));
            },
            BytecodeInstruction::F2I => {
                let val = f32::from_bits(state.stack.pop().as_float().unwrap());

                state.stack.push(Value::Int(
                    if val.is_nan() {
                        0
                    } else if val <= (i32::min_value() as f32) {
                        i32::min_value()
                    } else if val >= (i32::max_value() as f32) {
                        i32::max_value()
                    } else {
                        val as i32
                    }
                ));
            },
            BytecodeInstruction::I2D => {
                let val = state.stack.pop().as_int().unwrap();
                state.stack.push(Value::Double((val as f64).to_bits()));
            },
            BytecodeInstruction::D2I => {
                let val = f64::from_bits(state.stack.pop().as_double().unwrap());

                state.stack.push(Value::Int(
                    if val.is_nan() {
                        0
                    } else if val <= (i32::min_value() as f64) {
                        i32::min_value()
                    } else if val >= (i32::max_value() as f64) {
                        i32::max_value()
                    } else {
                        val as i32
                    }
                ));
            },
            BytecodeInstruction::L2F => {
                let val = state.stack.pop().as_long().unwrap();
                state.stack.push(Value::Float((val as f32).to_bits()));
            },
            BytecodeInstruction::F2L => {
                let val = f32::from_bits(state.stack.pop().as_float().unwrap());

                state.stack.push(Value::Long(
                    if val.is_nan() {
                        0
                    } else if val <= (i64::min_value() as f32) {
                        i64::min_value()
                    } else if val >= (i64::max_value() as f32) {
                        i64::max_value()
                    } else {
                        val as i64
                    }
                ));
            },
            BytecodeInstruction::L2D => {
                let val = state.stack.pop().as_long().unwrap();
                state.stack.push(Value::Double((val as f64).to_bits()));
            },
            BytecodeInstruction::D2L => {
                let val = f64::from_bits(state.stack.pop().as_double().unwrap());

                state.stack.push(Value::Long(
                    if val.is_nan() {
                        0
                    } else if val <= (i64::min_value() as f64) {
                        i64::min_value()
                    } else if val >= (i64::max_value() as f64) {
                        i64::max_value()
                    } else {
                        val as i64
                    }
                ));
            },
            BytecodeInstruction::F2D => {
                let val = f32::from_bits(state.stack.pop().as_float().unwrap());
                state.stack.push(Value::Double((val as f64).to_bits()));
            },
            BytecodeInstruction::D2F => {
                let val = f64::from_bits(state.stack.pop().as_double().unwrap());
                state.stack.push(Value::Float((val as f32).to_bits()));
            },
            BytecodeInstruction::INeg => {
                let val = state.stack.pop().as_int().unwrap();
                state.stack.push(Value::Int(val.wrapping_neg()));
            },
            BytecodeInstruction::IAdd => {
                let o2 = state.stack.pop().as_int().unwrap();
                let o1 = state.stack.pop().as_int().unwrap();
                state.stack.push(Value::Int(o1.wrapping_add(o2)));
            },
            BytecodeInstruction::ISub => {
                let o2 = state.stack.pop().as_int().unwrap();
                let o1 = state.stack.pop().as_int().unwrap();
                state.stack.push(Value::Int(o1.wrapping_sub(o2)));
            },
            BytecodeInstruction::IMul => {
                let o2 = state.stack.pop().as_int().unwrap();
                let o1 = state.stack.pop().as_int().unwrap();
                state.stack.push(Value::Int(o1.wrapping_mul(o2)));
            },
            BytecodeInstruction::IDiv => {
                let o2 = state.stack.pop().as_int().unwrap();
                let o1 = state.stack.pop().as_int().unwrap();

                if o2 == 0 {
                    return Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT));
                };

                state.stack.push(Value::Int(o1.wrapping_div(o2)));
            },
            BytecodeInstruction::IShr => {
                let o2 = state.stack.pop().as_int().unwrap();
                let o1 = state.stack.pop().as_int().unwrap();
                state.stack.push(Value::Int(o1 >> (o2 & 0x1f)));
            },
            BytecodeInstruction::IUShr => {
                let o2 = state.stack.pop().as_int().unwrap();
                let o1 = state.stack.pop().as_int().unwrap();
                state.stack.push(Value::Int(((o1 as u32) >> ((o2 & 0x1f) as u32)) as i32));
            },
            BytecodeInstruction::IShl => {
                let o2 = state.stack.pop().as_int().unwrap();
                let o1 = state.stack.pop().as_int().unwrap();
                state.stack.push(Value::Int(o1 << (o2 & 0x1f)));
            },
            BytecodeInstruction::IAnd => {
                let o2 = state.stack.pop().as_int().unwrap();
                let o1 = state.stack.pop().as_int().unwrap();
                state.stack.push(Value::Int(o1 & o2));
            },
            BytecodeInstruction::IOr => {
                let o2 = state.stack.pop().as_int().unwrap();
                let o1 = state.stack.pop().as_int().unwrap();
                state.stack.push(Value::Int(o1 | o2));
            },
            BytecodeInstruction::IXor => {
                let o2 = state.stack.pop().as_int().unwrap();
                let o1 = state.stack.pop().as_int().unwrap();
                state.stack.push(Value::Int(o1 ^ o2));
            },
            BytecodeInstruction::LAdd => {
                let o2 = state.stack.pop().as_long().unwrap();
                let o1 = state.stack.pop().as_long().unwrap();
                state.stack.push(Value::Long(o1.wrapping_add(o2)));
            },
            BytecodeInstruction::LMul => {
                let o2 = state.stack.pop().as_long().unwrap();
                let o1 = state.stack.pop().as_long().unwrap();
                state.stack.push(Value::Long(o1.wrapping_mul(o2)));
            },
            BytecodeInstruction::LAnd => {
                let o2 = state.stack.pop().as_long().unwrap();
                let o1 = state.stack.pop().as_long().unwrap();
                state.stack.push(Value::Long(o1 & o2));
            },
            BytecodeInstruction::LShr => {
                let o2 = state.stack.pop().as_int().unwrap();
                let o1 = state.stack.pop().as_long().unwrap();
                state.stack.push(Value::Long(o1 >> (o2 as i64)));
            },
            BytecodeInstruction::LUShr => {
                let o2 = state.stack.pop().as_int().unwrap();
                let o1 = state.stack.pop().as_long().unwrap();
                state.stack.push(Value::Long(((o1 as u64) >> (o2 as u64)) as i64));
            },
            BytecodeInstruction::LShl => {
                let o2 = state.stack.pop().as_int().unwrap();
                let o1 = state.stack.pop().as_long().unwrap();
                state.stack.push(Value::Long(o1 << (o2 as i64)));
            },
            BytecodeInstruction::FCmpG => {
                let o2 = f32::from_bits(state.stack.pop().as_float().unwrap());
                let o1 = f32::from_bits(state.stack.pop().as_float().unwrap());
                state.stack.push(Value::Int(
                    match o1.partial_cmp(&o2) {
                        Some(std::cmp::Ordering::Equal) => 0,
                        Some(std::cmp::Ordering::Greater) => 1,
                        Some(std::cmp::Ordering::Less) => -1,
                        None => 1
                    }
                ));
            },
            BytecodeInstruction::FCmpL => {
                let o2 = f32::from_bits(state.stack.pop().as_float().unwrap());
                let o1 = f32::from_bits(state.stack.pop().as_float().unwrap());
                state.stack.push(Value::Int(
                    match o1.partial_cmp(&o2) {
                        Some(std::cmp::Ordering::Equal) => 0,
                        Some(std::cmp::Ordering::Greater) => 1,
                        Some(std::cmp::Ordering::Less) => -1,
                        None => -1
                    }
                ));
            },
            BytecodeInstruction::FMul => {
                let o2 = f32::from_bits(state.stack.pop().as_float().unwrap());
                let o1 = f32::from_bits(state.stack.pop().as_float().unwrap());
                state.stack.push(Value::Float((o1 * o2).to_bits()));
            },
            BytecodeInstruction::MonitorEnter => {
                state.stack.pop();
            },
            BytecodeInstruction::MonitorExit => {
                state.stack.pop();
            },
            BytecodeInstruction::New(idx) => {
                match state.class.constant_pool[idx as usize] {
                    ConstantPoolEntry::Class(ref cpe) => {
                        try_run_clinit_without_checkpoint(env, heap, cpe.class_id, verbose)?;

                        let obj_ref = state.heap.allocate_object(cpe.class_id)?;
                        state.stack.push(Value::Ref(Some(obj_ref)));
                    },
                    _ => unreachable!()
                };
            },
            BytecodeInstruction::NewArray(t) => {
                let len = state.stack.pop().as_int().unwrap();

                if len < 0 {
                    return Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT));
                };

                let arr_ref = state.heap.allocate_array(ClassId::for_primitive_type_array(t), len as u32)?;

                state.stack.push(Value::Ref(Some(arr_ref)));
            },
            BytecodeInstruction::ANewArray(idx) => {
                match state.class.constant_pool[idx as usize] {
                    ConstantPoolEntry::Class(ref cpe) => {
                        let len = state.stack.pop().as_int().unwrap();

                        if len < 0 {
                            return Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT));
                        };

                        let arr_ref = state.heap.allocate_array(cpe.array_class_id, len as u32)?;

                        state.stack.push(Value::Ref(Some(arr_ref)));
                    },
                    _ => unreachable!()
                };
            },
            BytecodeInstruction::AALoad | BytecodeInstruction::BALoad | BytecodeInstruction::CALoad | BytecodeInstruction::DALoad |
            BytecodeInstruction::FALoad | BytecodeInstruction::IALoad | BytecodeInstruction::LALoad | BytecodeInstruction::SALoad => {
                let idx = state.stack.pop().as_int().unwrap();
                let arr_ref = state.stack.pop().into_ref().unwrap();

                if let Some(arr_ref) = arr_ref {
                    let val = arr_ref.read_array_element(idx);
                    state.stack.push(val);
                } else {
                    return Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT));
                };
            },
            BytecodeInstruction::AAStore | BytecodeInstruction::BAStore | BytecodeInstruction::CAStore | BytecodeInstruction::DAStore |
            BytecodeInstruction::FAStore | BytecodeInstruction::IAStore | BytecodeInstruction::LAStore | BytecodeInstruction::SAStore => {
                let val = state.stack.pop();
                let idx = state.stack.pop().as_int().unwrap();
                let arr_ref = state.stack.pop().into_ref().unwrap();

                if let Some(arr_ref) = arr_ref {
                    // TODO Check class of element stored
                    arr_ref.write_array_element(idx, val);
                } else {
                    return Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT));
                };
            },
            BytecodeInstruction::ArrayLength => {
                let arr_ref = state.stack.pop().into_ref().unwrap();

                if let Some(arr_ref) = arr_ref {
                    state.stack.push(Value::Int(arr_ref.read_array_length()));
                } else {
                    return Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT));
                };
            },
            BytecodeInstruction::GetField(idx) => {
                match state.class.constant_pool[idx as usize] {
                    ConstantPoolEntry::Fieldref(ref cpe) => {
                        let obj_ref = state.stack.pop().into_ref().unwrap();

                        if let Some(obj_ref) = obj_ref {
                            let value = obj_ref.read_field(cpe.field_id);
                            state.stack.push(value);
                        } else {
                            return Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT));
                        };
                    },
                    _ => unreachable!()
                };
            },
            BytecodeInstruction::PutField(idx) => {
                match state.class.constant_pool[idx as usize] {
                    ConstantPoolEntry::Fieldref(ref cpe) => {
                        let value = state.stack.pop();
                        let obj_ref = state.stack.pop().into_ref().unwrap();

                        if let Some(obj_ref) = obj_ref {
                            obj_ref.write_field(cpe.field_id, value);
                        } else {
                            return Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT));
                        };
                    },
                    _ => unreachable!()
                };
            },
            BytecodeInstruction::GetStatic(idx) => {
                match state.class.constant_pool[idx as usize] {
                    ConstantPoolEntry::Fieldref(ref cpe) => {
                        try_run_clinit_without_checkpoint(env, heap, cpe.field_id.0, verbose)?;

                        let class_obj = state.heap.get_class_object(cpe.field_id.0);
                        let value = class_obj.read_field(cpe.field_id);
                        state.stack.push(value);
                    },
                    _ => unreachable!()
                };
            },
            BytecodeInstruction::PutStatic(idx) => {
                match state.class.constant_pool[idx as usize] {
                    ConstantPoolEntry::Fieldref(ref cpe) => {
                        try_run_clinit_without_checkpoint(env, heap, cpe.field_id.0, verbose)?;

                        let class_obj = state.heap.get_class_object(cpe.field_id.0);
                        let value = state.stack.pop();
                        class_obj.write_field(cpe.field_id, value);
                    },
                    _ => unreachable!()
                };
            },
            BytecodeInstruction::LCmp => {
                let rhs = state.stack.pop().as_long().unwrap();
                let lhs = state.stack.pop().as_long().unwrap();

                state.stack.push(Value::Int(
                    match lhs.cmp(&rhs) {
                        std::cmp::Ordering::Equal => 0,
                        std::cmp::Ordering::Greater => 1,
                        std::cmp::Ordering::Less => -1
                    }
                ));
            },
            BytecodeInstruction::If(cond, dest) => {
                let operand = state.stack.pop().as_int().unwrap();
                if do_compare_int(operand, 0, cond) {
                    state.instrs.1 = dest;
                };
            },
            BytecodeInstruction::IfICmp(cond, dest) => {
                let o2 = state.stack.pop().as_int().unwrap();
                let o1 = state.stack.pop().as_int().unwrap();
                if do_compare_int(o1, o2, cond) {
                    state.instrs.1 = dest;
                };
            },
            BytecodeInstruction::IfACmp(cond, dest) => {
                let o2 = state.stack.pop().into_ref().unwrap();
                let o1 = state.stack.pop().into_ref().unwrap();
                if do_compare_ref(o1, o2, cond) {
                    state.instrs.1 = dest;
                };
            },
            BytecodeInstruction::IfNonNull(dest) => {
                let operand = state.stack.pop().into_ref().unwrap();
                if operand.is_some() {
                    state.instrs.1 = dest;
                };
            },
            BytecodeInstruction::IfNull(dest) => {
                let operand = state.stack.pop().into_ref().unwrap();
                if operand.is_none() {
                    state.instrs.1 = dest;
                };
            },
            BytecodeInstruction::Goto(dest) => {
                state.instrs.1 = dest;
            },
            BytecodeInstruction::LookupSwitch(default_dest, ref cases) => {
                let selector = state.stack.pop().as_int().unwrap();
                state.instrs.1 = cases.iter().find(|&&(key, _)| key == selector).map_or(default_dest, |&(_, dest)| dest);
            },
            BytecodeInstruction::TableSwitch(lo, default_dest, ref table) => {
                let index = state.stack.pop().as_int().unwrap().wrapping_sub(lo);
                state.instrs.1 = table.get(index as usize).map_or(default_dest, |&dest| dest);
            },
            BytecodeInstruction::InstanceOf(idx) => {
                let val = state.stack.pop().into_ref().unwrap();
                let class_id = match state.class.constant_pool[idx as usize] {
                    ConstantPoolEntry::Class(ref cpe) => cpe.class_id,
                    _ => unreachable!()
                };

                state.stack.push(Value::Int(if let Some(val) = val {
                    if state.env.can_convert(val.class_id(), class_id) {
                        1
                    } else {
                        0
                    }
                } else {
                    0
                }));
            },
            BytecodeInstruction::CheckCast(idx) => {
                let class_id = match state.class.constant_pool[idx as usize] {
                    ConstantPoolEntry::Class(ref cpe) => cpe.class_id,
                    _ => unreachable!()
                };

                if let Some(val) = state.stack.peek().as_ref().unwrap() {
                    if !state.env.can_convert(val.class_id(), class_id) {
                        return Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT));
                    };
                };
            },
            BytecodeInstruction::InvokeStatic(idx) | BytecodeInstruction::InvokeSpecial(idx) => {
                match state.class.constant_pool[idx as usize] {
                    ConstantPoolEntry::Methodref(ref cpe) => {
                        state.enter_method(cpe.method_id, verbose)?;
                    },
                    ConstantPoolEntry::InterfaceMethodref(ref cpe) => {
                        state.enter_method(cpe.method_id, verbose)?;
                    },
                    _ => unreachable!()
                };
            },
            BytecodeInstruction::InvokeVirtual(idx) => {
                match state.class.constant_pool[idx as usize] {
                    ConstantPoolEntry::Methodref(ref cpe) => {
                        if let Some(receiver) = state.find_receiver(cpe.method_id, verbose)? {
                            let receiver = receiver.clone();
                            let real_method_id = state.find_virtual_target(cpe.method_id, receiver, verbose)?;
                            state.enter_method(real_method_id, verbose)?;
                        } else {
                            return Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT));
                        };
                    },
                    _ => unreachable!()
                };
            },
            BytecodeInstruction::InvokeInterface(idx, _) => {
                match state.class.constant_pool[idx as usize] {
                    ConstantPoolEntry::InterfaceMethodref(ref cpe) => {
                        if let Some(receiver) = state.find_receiver(cpe.method_id, verbose)? {
                            let receiver = receiver.clone();
                            let real_method_id = state.find_interface_target(cpe.method_id, receiver, verbose)?;
                            state.enter_method(real_method_id, verbose)?;
                        } else {
                            return Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT));
                        };
                    },
                    _ => unreachable!()
                };
            },
            BytecodeInstruction::AThrow => {
                let exception = state.stack.pop().into_ref().unwrap();
                let exception_class = if let Some(exception) = exception {
                    exception.class_id()
                } else {
                    ClassId::JAVA_LANG_OBJECT
                };

                return Result::Err(StaticInterpretError::WouldThrowException(exception_class));
            },
            BytecodeInstruction::AReturn | BytecodeInstruction::DReturn | BytecodeInstruction::FReturn | BytecodeInstruction::IReturn | BytecodeInstruction::LReturn => {
                let result = state.stack.pop();
                assert!(!state.exit_method(verbose)?);
                state.stack.push(result);
            },
            BytecodeInstruction::Return => {
                if state.exit_method(verbose)? {
                    break;
                };
            },
            instr => {
                return Result::Err(StaticInterpretError::UnimplementedBytecode(method_id, instr));
            }
        };
    };

    if verbose {
        eprintln!("  Done interpreting {}.{}{}", class.meta.name, method.name, method.descriptor);
    };

    Result::Ok(())
}

fn try_run_clinit_without_checkpoint(env: &ClassEnvironment, heap: &JavaStaticHeap, class_id: ClassId, verbose: bool) -> Result<(), StaticInterpretError> {
    let class_obj = unsafe { heap.get_class_object_untracked(class_id) };
    let skip = class_obj.flags().intersects(ObjectFlags::CLINIT_DONE | ObjectFlags::CLINIT_RUNNING);

    if skip {
        return Result::Ok(());
    };

    unsafe {
        class_obj.set_flags(ObjectFlags::CLINIT_RUNNING);
    };

    do_pre_clinit(env, heap, class_id, verbose)?;

    match **env.get(class_id) {
        ResolvedClass::User(ref class) => {
            if &*class.meta.name != "java/nio/charset/Charset$ExtendedProviderHolder" {
                for (i, m) in class.methods.iter().enumerate() {
                    if &*m.name == "<clinit>" {
                        try_interpret(env, heap, MethodId(class_id, i as u16), verbose)?;
                    };
                };
            };
        },
        _ => {}
    };

    unsafe {
        class_obj.set_flags(ObjectFlags::CLINIT_DONE);
    };

    Result::Ok(())
}

pub fn try_run_clinit(env: &ClassEnvironment, heap: &JavaStaticHeap, class: ClassId, verbose: bool) -> bool {
    heap.commit();

    if verbose {
        eprintln!("Attempting to statically run <clinit> for {}...", env.get(class).name(env));
    };

    match try_run_clinit_without_checkpoint(env, heap, class, verbose) {
        Result::Ok(()) => true,
        Result::Err(err) => {
            eprint!("WARNING: Failed to statically run <clinit> for {}: ", env.get(class).name(env));
            match err {
                StaticInterpretError::UnimplementedBytecode(_, bc) => {
                    eprintln!("Unimplemented bytecode {:?}", bc);
                },
                StaticInterpretError::UnknownNativeCall(method_id) => {
                    let (class, method) = env.get_method(method_id);
                    eprintln!("Unknown native call to {}.{}{}", class.meta.name, method.name, method.descriptor);
                },
                StaticInterpretError::OutOfMemory => {
                    eprintln!("Ran out of memory in static heap");
                },
                StaticInterpretError::WouldThrowException(exception_class_id) => {
                    eprintln!("Threw exception of type {}", env.get(exception_class_id).name(env));
                },
                StaticInterpretError::ExcludedClinit(class_id) => {
                    eprintln!("The <clinit> method of {} is explicitly disabled", env.get(class_id).name(env));
                }
            };
            heap.rollback();
            false
        }
    }
}
