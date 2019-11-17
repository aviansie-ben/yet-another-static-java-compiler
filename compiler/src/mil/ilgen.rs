use std::collections::HashMap;
use std::convert::TryInto;

use super::il::*;
use crate::bytecode::{BytecodeInstruction, BytecodeIterator};
use crate::classfile::{ConstantPoolEntry, FlatTypeDescriptor, Method, MethodFlags, PrimitiveType, TypeDescriptor};
use crate::resolve::{ClassEnvironment, ClassId, MethodId};

pub struct MilBuilder {
    func: MilFunction,
    current_block: MilBlock
}

impl MilBuilder {
    pub fn new(id: MethodId) -> MilBuilder {
        MilBuilder {
            func: MilFunction::new(id),
            current_block: MilBlock::new()
        }
    }

    pub fn allocate_reg(&mut self, ty: MilType) -> MilRegister {
        if ty == MilType::Void {
            MilRegister::VOID
        } else {
            let r = self.func.reg_alloc.allocate_one();
            self.func.reg_map.info.insert(r, MilRegisterInfo { ty });
            r
        }
    }

    pub fn append_instruction(&mut self, kind: MilInstructionKind, bc: u32) {
        self.current_block.instrs.push(MilInstruction {
            kind,
            bytecode: (!0, bc)
        });
    }

    pub fn append_end_instruction(&mut self, kind: MilEndInstructionKind, bc: u32) -> MilBlockId {
        self.current_block.end_instr = MilEndInstruction {
            kind,
            bytecode: (!0, bc)
        };
        self.end_block()
    }

    pub fn end_block(&mut self) -> MilBlockId {
        let id = self.func.block_alloc.allocate_one();
        self.current_block.id = id;
        self.func.blocks.insert(id, std::mem::replace(&mut self.current_block, MilBlock::new()));
        self.func.block_order.push(id);
        id
    }

    pub fn get_block_mut(&mut self, id: MilBlockId) -> &mut MilBlock {
        self.func.blocks.get_mut(&id).unwrap()
    }

    pub fn finish(self) -> MilFunction {
        if !self.current_block.instrs.is_empty() {
            panic!("Attempt to finish MilBuilder while last block incomplete");
        };

        self.func
    }
}

struct MilLocals {
    locals: Vec<[MilRegister; 5]>
}

impl MilLocals {
    fn new(num_locals: u16) -> MilLocals {
        MilLocals {
            locals: vec![[MilRegister::VOID; 5]; num_locals as usize]
        }
    }

    fn type_index(ty: MilType) -> usize {
        match ty {
            MilType::Ref => 0,
            MilType::Int => 1,
            MilType::Long => 2,
            MilType::Float => 3,
            MilType::Double => 4,
            _ => panic!("Unsupported type {:?} for locals", ty)
        }
    }

    fn get(&self, local: u16, ty: MilType) -> MilRegister {
        self.locals[local as usize][MilLocals::type_index(ty)]
    }

    fn set(&mut self, local: u16, ty: MilType, val: MilRegister) {
        self.locals[local as usize][MilLocals::type_index(ty)] = val;
    }
}

fn collect_targets(instrs: BytecodeIterator) -> HashMap<usize, (MilBlockId, Option<Vec<MilRegister>>)> {
    let mut map = HashMap::new();

    for (off, instr) in instrs {
        match instr.unwrap() {
            BytecodeInstruction::Goto(dest) => {
                map.insert(dest, (MilBlockId::ENTRY, None));
            },
            BytecodeInstruction::IfACmp(_, dest) => {
                map.insert(dest, (MilBlockId::ENTRY, None));
            },
            BytecodeInstruction::IfICmp(_, dest) => {
                map.insert(dest, (MilBlockId::ENTRY, None));
            },
            BytecodeInstruction::If(_, dest) => {
                map.insert(dest, (MilBlockId::ENTRY, None));
            },
            BytecodeInstruction::IfNonNull(dest) => {
                map.insert(dest, (MilBlockId::ENTRY, None));
            },
            BytecodeInstruction::IfNull(dest) => {
                map.insert(dest, (MilBlockId::ENTRY, None));
            },
            BytecodeInstruction::JSR(_) => unimplemented!(),
            BytecodeInstruction::Ret(_) => unimplemented!(),
            BytecodeInstruction::TableSwitch(_, _, _) => unimplemented!(),
            BytecodeInstruction::LookupSwitch(_, _) => unimplemented!(),
            _ => {}
        };
    };

    map
}

fn get_mil_type_for_descriptor(ty: &TypeDescriptor) -> MilType {
    if ty.array_dims > 0 {
        MilType::Ref
    } else {
        match ty.flat {
            FlatTypeDescriptor::Primitive(PrimitiveType::Byte) => MilType::Int,
            FlatTypeDescriptor::Primitive(PrimitiveType::Char) => MilType::Int,
            FlatTypeDescriptor::Primitive(PrimitiveType::Double) => MilType::Double,
            FlatTypeDescriptor::Primitive(PrimitiveType::Float) => MilType::Float,
            FlatTypeDescriptor::Primitive(PrimitiveType::Int) => MilType::Int,
            FlatTypeDescriptor::Primitive(PrimitiveType::Long) => MilType::Long,
            FlatTypeDescriptor::Primitive(PrimitiveType::Short) => MilType::Int,
            FlatTypeDescriptor::Primitive(PrimitiveType::Boolean) => MilType::Int,
            FlatTypeDescriptor::Reference(_) => MilType::Ref
        }
    }
}

fn pop_args(stack: &mut Vec<MilRegister>, n: usize) -> Vec<MilOperand> {
    let mut args = vec![];

    for _ in 0..n {
        args.push(MilOperand::Register(stack.pop().unwrap()));
    };

    args.reverse();
    args
}

fn constant_from_cpe(cpe: &ConstantPoolEntry, known_objects: &MilKnownObjectRefs) -> MilOperand {
    match *cpe {
        ConstantPoolEntry::Class(ref cpe) => MilOperand::KnownObject(
            known_objects.classes[&cpe.class_id],
            ClassId::JAVA_LANG_CLASS
        ),
        ConstantPoolEntry::String(ref cpe) => MilOperand::KnownObject(
            known_objects.strings[cpe.index],
            ClassId::JAVA_LANG_STRING
        ),
        ConstantPoolEntry::Integer(val) => MilOperand::Int(val),
        ConstantPoolEntry::Float(val) => MilOperand::Float(val),
        ConstantPoolEntry::Long(val) => MilOperand::Long(val),
        ConstantPoolEntry::Double(val) => MilOperand::Double(val),
        _ => unreachable!()
    }
}

fn read_param(builder: &mut MilBuilder, i: u16, param_type: ClassId) -> MilRegister {
    let reg = builder.allocate_reg(MilType::for_class(param_type));

    builder.append_instruction(
        if param_type != ClassId::UNRESOLVED {
            MilInstructionKind::GetParam(i.try_into().unwrap(), param_type, reg)
        } else {
            MilInstructionKind::Copy(reg, MilOperand::Null)
        },
        0
    );

    reg
}

fn get_params(builder: &mut MilBuilder, locals: &mut MilLocals, method: &Method) {
    let mut next_param_local = 0;

    for (i, param_type) in method.param_types.iter().cloned().enumerate() {
        locals.set(
            next_param_local,
            MilType::for_class(param_type),
            read_param(builder, i.try_into().unwrap(), param_type)
        );

        next_param_local += 1;
        if param_type.needs_dual_slot() {
            next_param_local += 1;
        };
    };
}

fn generate_native_thunk(env: &ClassEnvironment, name: String, method: &Method, method_id: MethodId, known_objects: &MilKnownObjectRefs) -> MilFunction {
    let mut builder = MilBuilder::new(method_id);

    let mut args = vec![];

    if method.flags.contains(MethodFlags::STATIC) {
        args.push(MilOperand::KnownObject(known_objects.classes[&method_id.0], ClassId::JAVA_LANG_CLASS));
    };

    args.extend(
        method.param_types.iter().cloned().enumerate()
            .map(|(i, param_type)| MilOperand::Register(
                read_param(&mut builder, i.try_into().unwrap(), param_type)
            ))
    );

    let reg = builder.allocate_reg(MilType::for_class(method.return_type));

    builder.append_end_instruction(
        MilEndInstructionKind::CallNative(method.return_type, name, reg, args),
        0
    );
    builder.append_end_instruction(MilEndInstructionKind::Return(MilOperand::Register(reg)), 0);

    builder.finish()
}

pub fn generate_il_for_method(env: &ClassEnvironment, method_id: MethodId, known_objects: &MilKnownObjectRefs, verbose: bool) -> Option<MilFunction> {
    let (class, method) = env.get_method(method_id);

    if method.flags.contains(MethodFlags::NATIVE) {
        let name = format!("{}_{}", class.meta.name.replace('/', "_"), method.name);
        return Some(generate_native_thunk(env, name, method, method_id, known_objects));
    } else if method.flags.contains(MethodFlags::ABSTRACT) {
        return None;
    };

    if verbose {
        eprintln!("===== MIL Generation for {}.{}{} =====\n", class.meta.name, method.name, method.descriptor);
    };

    let code = method.code_attribute().unwrap();
    let instrs = BytecodeIterator::for_code(code);

    let mut locals = MilLocals::new(code.max_locals);
    let mut stack = vec![];

    let mut targets = collect_targets(instrs);
    let mut builder = MilBuilder::new(method_id);

    get_params(&mut builder, &mut locals, method);

    for (bc, instr) in instrs {
        let bc = bc as u32;
        let instr = instr.unwrap();

        if verbose {
            eprintln!("  {:?}", instr);
        };

        match instr {
            BytecodeInstruction::Ldc(idx) | BytecodeInstruction::Ldc2(idx) => {
                let val = constant_from_cpe(&class.constant_pool[idx as usize], known_objects);
                let reg = builder.allocate_reg(val.get_const_type().unwrap());
                builder.append_instruction(
                    MilInstructionKind::Copy(reg, val),
                    bc
                );
                stack.push(reg);
            },
            BytecodeInstruction::AConstNull => {
                let reg = builder.allocate_reg(MilType::Ref);
                builder.append_instruction(
                    MilInstructionKind::Copy(reg, MilOperand::Null),
                    bc
                );
                stack.push(reg);
            },
            BytecodeInstruction::IConst(val) => {
                let reg = builder.allocate_reg(MilType::Int);
                builder.append_instruction(
                    MilInstructionKind::Copy(reg, MilOperand::Int(val)),
                    bc
                );
                stack.push(reg);
            },
            BytecodeInstruction::Dup => {
                stack.push(*stack.last().unwrap());
            },
            BytecodeInstruction::ALoad(idx) => {
                stack.push(locals.get(idx, MilType::Ref));
            },
            BytecodeInstruction::New(idx) => {
                let cpe = match class.constant_pool[idx as usize] {
                    ConstantPoolEntry::Class(ref cpe) => cpe,
                    _ => unreachable!()
                };

                let reg = builder.allocate_reg(MilType::Ref);
                builder.append_instruction(
                    MilInstructionKind::AllocObj(cpe.class_id, reg),
                    bc
                );
                stack.push(reg);
            },
            BytecodeInstruction::ANewArray(idx) => {
                let cpe = match class.constant_pool[idx as usize] {
                    ConstantPoolEntry::Class(ref cpe) => cpe,
                    _ => unreachable!()
                };

                let reg = builder.allocate_reg(MilType::Ref);
                let len = MilOperand::Register(stack.pop().unwrap());
                builder.append_instruction(
                    MilInstructionKind::AllocArray(cpe.array_class_id, reg, len),
                    bc
                );
                stack.push(reg);
            },
            BytecodeInstruction::InvokeStatic(idx) => {
                let cpe = match class.constant_pool[idx as usize] {
                    ConstantPoolEntry::Methodref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                let ret_class = env.get_method(cpe.method_id).1.return_type;
                let reg = builder.allocate_reg(MilType::for_class(ret_class));
                let args = pop_args(&mut stack, cpe.descriptor.param_types.len());
                builder.append_end_instruction(
                    MilEndInstructionKind::Call(
                        ret_class,
                        cpe.method_id,
                        reg,
                        args
                    ),
                    bc
                );
                if ret_class != ClassId::PRIMITIVE_VOID {
                    stack.push(reg);
                };
            },
            BytecodeInstruction::InvokeSpecial(idx) => {
                let cpe = match class.constant_pool[idx as usize] {
                    ConstantPoolEntry::Methodref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                let ret_class = env.get_method(cpe.method_id).1.return_type;
                let reg = builder.allocate_reg(MilType::for_class(ret_class));
                let args = pop_args(&mut stack, cpe.descriptor.param_types.len() + 1);
                builder.append_end_instruction(
                    MilEndInstructionKind::Call(
                        ret_class,
                        cpe.method_id,
                        reg,
                        args
                    ),
                    bc
                );
                if ret_class != ClassId::PRIMITIVE_VOID {
                    stack.push(reg);
                };
            },
            BytecodeInstruction::GetField(idx) => {
                let cpe = match class.constant_pool[idx as usize] {
                    ConstantPoolEntry::Fieldref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                let obj = MilOperand::Register(stack.pop().unwrap());
                let ty = get_mil_type_for_descriptor(&cpe.descriptor);
                let reg = builder.allocate_reg(ty);
                builder.append_instruction(
                    MilInstructionKind::GetField(cpe.field_id, cpe.type_id, reg, obj),
                    bc
                );
                stack.push(reg);
            },
            BytecodeInstruction::PutField(idx) => {
                let cpe = match class.constant_pool[idx as usize] {
                    ConstantPoolEntry::Fieldref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                let val = MilOperand::Register(stack.pop().unwrap());
                let obj = MilOperand::Register(stack.pop().unwrap());
                builder.append_instruction(
                    MilInstructionKind::PutField(cpe.field_id, cpe.type_id, obj, val),
                    bc
                );
            },
            BytecodeInstruction::GetStatic(idx) => {
                let cpe = match class.constant_pool[idx as usize] {
                    ConstantPoolEntry::Fieldref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                let ty = get_mil_type_for_descriptor(&cpe.descriptor);
                let reg = builder.allocate_reg(ty);
                builder.append_instruction(
                    MilInstructionKind::GetStatic(cpe.field_id, cpe.type_id, reg),
                    bc
                );
                stack.push(reg);
            },
            BytecodeInstruction::PutStatic(idx) => {
                let cpe = match class.constant_pool[idx as usize] {
                    ConstantPoolEntry::Fieldref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                let val = MilOperand::Register(stack.pop().unwrap());
                builder.append_instruction(
                    MilInstructionKind::PutStatic(cpe.field_id, cpe.type_id, val),
                    bc
                );
            },
            BytecodeInstruction::AReturn | BytecodeInstruction::DReturn | BytecodeInstruction::FReturn | BytecodeInstruction::IReturn | BytecodeInstruction::LReturn => {
                let val = MilOperand::Register(stack.pop().unwrap());
                builder.append_end_instruction(
                    MilEndInstructionKind::Return(val),
                    bc
                );
            },
            BytecodeInstruction::Return => {
                builder.append_end_instruction(
                    MilEndInstructionKind::Return(MilOperand::Register(MilRegister::VOID)),
                    bc
                );
            },
            instr => {
                panic!("Unsupported bytecode {:?} in ilgen", instr);
            }
        };
    };

    let func = builder.finish();

    if verbose {
        eprintln!("{}", func.pretty(env));
    };

    Some(func)
}
