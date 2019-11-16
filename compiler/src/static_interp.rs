use std::alloc::AllocErr;
use std::collections::HashMap;

use lazy_static::lazy_static;

use crate::bytecode::{BytecodeCondition, BytecodeInstruction, BytecodeIterator};
use crate::classfile::{AttributeData, Class, ConstantPoolEntry, FieldFlags, FlatTypeDescriptor, MethodFlags, MethodSummary, PrimitiveType};
use crate::resolve::{ClassEnvironment, ClassId, FieldId, MethodId, ResolvedClass};
use crate::static_heap::{ObjectFlags, JavaStaticHeap, JavaStaticRef};

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

pub fn summarize_bytecode(instrs: BytecodeIterator, cp: &[ConstantPoolEntry]) -> MethodSummary {
    let mut summary = MethodSummary {
        may_virtual_call: vec![],
        may_special_call: vec![],
        may_construct: vec![],
        may_clinit: vec![]
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
            BytecodeInstruction::Ldc(cpe) | BytecodeInstruction::Ldc2(cpe) => {
                match cp[cpe as usize] {
                    ConstantPoolEntry::Class(ref cpe) => {
                        add_may_clinit(&mut summary, cpe.class_id);
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
                eprintln!("WARNING: Bad primitive type {}", name);
                return Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT));
            }
        };

        state.stack.push(Value::Ref(Some(result)));
        Result::Ok(())
    } else {
        Result::Err(StaticInterpretError::WouldThrowException(ClassId::JAVA_LANG_OBJECT))
    }
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

type StaticNative = fn (&mut InterpreterState) -> Result<(), StaticInterpretError>;

lazy_static! {
    static ref KNOWN_NATIVES: HashMap<&'static str, StaticNative> = {
        let mut known_natives = HashMap::new();

        known_natives.insert("java/lang/Object.registerNatives()V", native_nop as StaticNative);
        known_natives.insert("java/lang/Thread.registerNatives()V", native_nop as StaticNative);
        known_natives.insert("java/lang/System.registerNatives()V", native_nop as StaticNative);

        known_natives.insert(
            "java/lang/Class.getPrimitiveClass(Ljava/lang/String;)Ljava/lang/Class;",
            native_get_primitive_class as StaticNative
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
            BytecodeInstruction::Dup => {
                let val = state.stack.peek().clone();
                state.stack.push(val);
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
            BytecodeInstruction::I2L => {
                let val = state.stack.pop().as_int().unwrap();
                state.stack.push(Value::Long(val as i64));
            },
            BytecodeInstruction::ISub => {
                let o2 = state.stack.pop().as_int().unwrap();
                let o1 = state.stack.pop().as_int().unwrap();
                state.stack.push(Value::Int(o1.wrapping_sub(o2)));
            },
            BytecodeInstruction::LAdd => {
                let o2 = state.stack.pop().as_long().unwrap();
                let o1 = state.stack.pop().as_long().unwrap();
                state.stack.push(Value::Long(o1.wrapping_add(o2)));
            },
            BytecodeInstruction::LAnd => {
                let o2 = state.stack.pop().as_long().unwrap();
                let o1 = state.stack.pop().as_long().unwrap();
                state.stack.push(Value::Long(o1 & o2));
            },
            BytecodeInstruction::LShl => {
                let o2 = state.stack.pop().as_int().unwrap();
                let o1 = state.stack.pop().as_long().unwrap();
                state.stack.push(Value::Long(o1 << (o2 as i64)));
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
            BytecodeInstruction::InstanceOf(idx) => {
                // TODO Perform the actual check
                state.stack.pop();
                state.stack.push(Value::Int(1));
            },
            BytecodeInstruction::CheckCast(idx) => {
                // TODO Perform the actual check
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
            if &*class.meta.name == "java/lang/System" {
                return Result::Err(StaticInterpretError::ExcludedClinit(class_id));
            };

            for (i, m) in class.methods.iter().enumerate() {
                if &*m.name == "<clinit>" {
                    try_interpret(env, heap, MethodId(class_id, i as u16), verbose)?;
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
