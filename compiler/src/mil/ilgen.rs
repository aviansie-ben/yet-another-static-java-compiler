use std::collections::{HashMap, HashSet, VecDeque};
use std::convert::TryInto;

use itertools::Itertools;
use smallvec::*;

use super::flow_graph::*;
use super::il::*;
use super::transform;
use crate::bytecode::{BytecodeInstruction, BytecodeIterator};
use crate::classfile::{AttributeCode, ConstantPoolEntry, FlatTypeDescriptor, Method, MethodFlags, PrimitiveType, TypeDescriptor};
use crate::liveness::LivenessInfo;
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

    pub fn append_phi_node(&mut self, srcs: impl IntoIterator<Item=(MilRegister, MilBlockId)>, bc: u32) -> MilRegister {
        assert!(self.current_block.instrs.is_empty());

        let mut ty = None;
        let srcs = srcs.into_iter().map(|(reg, block)| {
            if let Some(ty) = ty {
                assert_eq!(self.func.reg_map.info[&reg].ty, ty);
            } else {
                ty = Some(self.func.reg_map.info[&reg].ty);
            };

            (reg, block)
        }).collect();

        let reg = self.allocate_reg(ty.unwrap());
        self.current_block.phi_nodes.push(MilPhiNode {
            target: reg,
            sources: srcs
        });

        reg
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

    pub fn insert_end_instruction(&mut self, block: MilBlockId, kind: MilEndInstructionKind, bc: u32) {
        let block = self.func.blocks.get_mut(&block).unwrap();

        block.end_instr = MilEndInstruction {
            kind,
            bytecode: (!0, bc)
        };
    }

    pub fn end_block(&mut self) -> MilBlockId {
        let id = self.func.block_alloc.allocate_one();
        self.current_block.id = id;
        self.func.blocks.insert(id, std::mem::replace(&mut self.current_block, MilBlock::new()));
        self.func.block_order.push(id);
        id
    }

    pub fn end_ordered_blocks(&mut self) -> Vec<MilBlockId> {
        assert!(self.current_block.phi_nodes.is_empty());
        assert!(self.current_block.instrs.is_empty());

        std::mem::replace(&mut self.func.block_order, vec![])
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
    locals: Vec<[Option<MilLocalId>; 5]>,
    next_local: MilLocalId
}

impl MilLocals {
    fn new(num_locals: u16) -> MilLocals {
        MilLocals {
            locals: vec![[None; 5]; num_locals as usize],
            next_local: MilLocalId(0)
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

    fn get_or_add(&mut self, local: u16, ty: MilType, map: &mut MilRegisterMap) -> MilLocalId {
        if let Some(id) = self.locals[local as usize][MilLocals::type_index(ty)] {
            id
        } else {
            let id = self.next_local;
            self.next_local.0 += 1;

            self.locals[local as usize][MilLocals::type_index(ty)] = Some(id);
            map.local_info.insert(id, MilLocalInfo {
                java_local: local,
                ty
            });

            id
        }
    }
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
        let local_id = locals.get_or_add(
            next_param_local,
            MilType::for_class(param_type),
            &mut builder.func.reg_map
        );
        let reg = read_param(builder, i.try_into().unwrap(), param_type);
        builder.append_instruction(
            MilInstructionKind::SetLocal(local_id, MilOperand::Register(reg)),
            0
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

struct GenBlockInfo {
    preds: SmallVec<[usize; 2]>,
    succs: SmallVec<[usize; 2]>,
    blocks: Vec<MilBlockId>,
    end_stack: Vec<MilRegister>
}

impl GenBlockInfo {
    pub fn new() -> GenBlockInfo {
        GenBlockInfo {
            preds: smallvec![],
            succs: smallvec![],
            blocks: vec![],
            end_stack: vec![]
        }
    }
}

fn scan_for_block_starts(instrs: BytecodeIterator) -> HashSet<usize> {
    let mut block_starts = HashSet::new();
    let mut next_starts_block = true;

    for (bc, instr) in instrs {
        if next_starts_block {
            next_starts_block = false;
            block_starts.insert(bc);
        };

        match instr.unwrap() {
            BytecodeInstruction::Goto(dest) => {
                block_starts.insert(dest);
                next_starts_block = true;
            },
            BytecodeInstruction::IfACmp(_, dest) => {
                block_starts.insert(dest);
                next_starts_block = true;
            },
            BytecodeInstruction::IfICmp(_, dest) => {
                block_starts.insert(dest);
                next_starts_block = true;
            },
            BytecodeInstruction::If(_, dest) => {
                block_starts.insert(dest);
                next_starts_block = true;
            },
            BytecodeInstruction::IfNonNull(dest) => {
                block_starts.insert(dest);
                next_starts_block = true;
            },
            BytecodeInstruction::IfNull(dest) => {
                block_starts.insert(dest);
                next_starts_block = true;
            },
            BytecodeInstruction::AThrow => {
                next_starts_block = true;
            },
            BytecodeInstruction::Return | BytecodeInstruction::AReturn | BytecodeInstruction::DReturn | BytecodeInstruction::FReturn | BytecodeInstruction::IReturn | BytecodeInstruction::LReturn => {
                next_starts_block = true;
            },
            BytecodeInstruction::JSR(_) => unimplemented!(),
            BytecodeInstruction::Ret(_) => unimplemented!(),
            BytecodeInstruction::TableSwitch(_, default_dest, ref table) => {
                block_starts.insert(default_dest);
                for dest in table.iter().copied() {
                    block_starts.insert(dest);
                };
                next_starts_block = true;
            },
            BytecodeInstruction::LookupSwitch(default_dest, ref cases) => {
                block_starts.insert(default_dest);
                for (_, dest) in cases.iter().copied() {
                    block_starts.insert(dest);
                };
                next_starts_block = true;
            },
            _ => {}
        };
    };

    block_starts
}

fn scan_blocks(instrs: BytecodeIterator) -> HashMap<usize, GenBlockInfo> {
    let mut current_block = 0;
    let mut can_fall_through = false;

    let mut edges = vec![];
    let mut blocks: HashMap<_, _> = scan_for_block_starts(instrs).into_iter()
        .map(|bc| (bc, GenBlockInfo::new()))
        .collect();

    for (bc, instr) in instrs {
        if blocks.contains_key(&bc) {
            if can_fall_through {
                edges.push((current_block, bc));
            } else {
                can_fall_through = true;
            };
            current_block = bc;
        };

        match instr.unwrap() {
            BytecodeInstruction::Goto(dest) => {
                edges.push((current_block, dest));
                can_fall_through = false;
            },
            BytecodeInstruction::IfACmp(_, dest) => {
                edges.push((current_block, dest));
            },
            BytecodeInstruction::IfICmp(_, dest) => {
                edges.push((current_block, dest));
            },
            BytecodeInstruction::If(_, dest) => {
                edges.push((current_block, dest));
            },
            BytecodeInstruction::IfNonNull(dest) => {
                edges.push((current_block, dest));
            },
            BytecodeInstruction::IfNull(dest) => {
                edges.push((current_block, dest));
            },
            BytecodeInstruction::AThrow => {
                can_fall_through = false;
            },
            BytecodeInstruction::Return | BytecodeInstruction::AReturn | BytecodeInstruction::DReturn | BytecodeInstruction::FReturn | BytecodeInstruction::IReturn | BytecodeInstruction::LReturn => {
                can_fall_through = false;
            },
            BytecodeInstruction::JSR(_) => unimplemented!(),
            BytecodeInstruction::Ret(_) => unimplemented!(),
            BytecodeInstruction::TableSwitch(_, default_dest, ref table) => {
                can_fall_through = false;
                edges.push((current_block, default_dest));
                for dest in table.iter().copied() {
                    edges.push((current_block, dest));
                };
            },
            BytecodeInstruction::LookupSwitch(default_dest, ref cases) => {
                can_fall_through = false;
                edges.push((current_block, default_dest));
                for (_, dest) in cases.iter().copied() {
                    edges.push((current_block, dest));
                };
            },
            _ => {}
        };
    };

    for (from, to) in edges.into_iter().sorted().dedup() {
        blocks.get_mut(&from).unwrap().succs.push(to);
        blocks.get_mut(&to).unwrap().preds.push(from);
    };

    blocks
}

fn emit_npe_throw(builder: &mut MilBuilder, bc: u32) {
    // TODO Create an actual exception
    builder.append_end_instruction(
        MilEndInstructionKind::Throw(MilOperand::Null),
        bc
    );
}

fn emit_null_check(builder: &mut MilBuilder, val: MilOperand, bc: u32) {
    let check_block = builder.end_block();
    emit_npe_throw(builder, bc);

    let not_null_block = builder.end_block();
    builder.insert_end_instruction(
        check_block,
        MilEndInstructionKind::JumpIf(MilComparison::Ne, not_null_block, val, MilOperand::Null),
        bc
    );
}

fn generate_un_op(builder: &mut MilBuilder, stack: &mut Vec<MilRegister>, bc: u32, op: MilUnOp, result_ty: MilType) {
    let reg = builder.allocate_reg(result_ty);
    let val = MilOperand::Register(stack.pop().unwrap());

    builder.append_instruction(
        MilInstructionKind::UnOp(op, reg, val),
        bc
    );
    stack.push(reg);
}

fn generate_bin_op(builder: &mut MilBuilder, stack: &mut Vec<MilRegister>, bc: u32, op: MilBinOp, result_ty: MilType) {
    let reg = builder.allocate_reg(result_ty);
    let rhs = MilOperand::Register(stack.pop().unwrap());
    let lhs = MilOperand::Register(stack.pop().unwrap());

    builder.append_instruction(
        MilInstructionKind::BinOp(op, reg, lhs, rhs),
        bc
    );
    stack.push(reg);
}

fn generate_array_load(builder: &mut MilBuilder, stack: &mut Vec<MilRegister>, bc: u32, elem_class: ClassId, result_ty: MilType) {
    let reg = builder.allocate_reg(result_ty);
    let idx = MilOperand::Register(stack.pop().unwrap());
    let obj = MilOperand::Register(stack.pop().unwrap());

    builder.append_instruction(
        MilInstructionKind::GetArrayElement(elem_class, reg, obj, idx),
        bc
    );
    stack.push(reg);
}

fn generate_array_store(builder: &mut MilBuilder, stack: &mut Vec<MilRegister>, bc: u32, elem_class: ClassId) {
    let val = MilOperand::Register(stack.pop().unwrap());
    let idx = MilOperand::Register(stack.pop().unwrap());
    let obj = MilOperand::Register(stack.pop().unwrap());

    builder.append_instruction(
        MilInstructionKind::PutArrayElement(elem_class, obj, idx, val),
        bc
    );
}

fn generate_il_for_block(env: &ClassEnvironment, builder: &mut MilBuilder, code: &AttributeCode, off: usize, cp: &[ConstantPoolEntry], blocks: &mut HashMap<usize, GenBlockInfo>, block_worklist: &mut Vec<usize>, locals: &mut MilLocals, fixups: &mut Vec<Box<dyn FnMut (&mut MilBuilder, &HashMap<usize, GenBlockInfo>) -> ()>>, known_objects: &MilKnownObjectRefs, verbose: bool) {
    let incoming_stacks = blocks.get(&off).unwrap().preds.iter().filter_map(|pred| {
        let pred = blocks.get(&pred).unwrap();

        if let Some(&pred_end_block) = pred.blocks.last() {
            Some((pred_end_block, &pred.end_stack))
        } else {
            None
        }
    }).collect_vec();

    let mut stack = if !incoming_stacks.is_empty() {
        assert!(incoming_stacks.iter().skip(1).all(|(_, s)| s.len() == incoming_stacks[0].1.len()));
        (0..incoming_stacks[0].1.len()).map(|i| {
            builder.append_phi_node(incoming_stacks.iter().map(|&(b, s)| (s[i], b)), off as u32)
        }).collect_vec()
    } else {
        vec![]
    };

    let start_block = builder.end_block();
    let mut end_block = None;

    for (bc, instr) in BytecodeIterator(&code.code, off).take_while(|&(bc, _)| bc == off || !blocks.contains_key(&bc)) {
        let bc = bc as u32;
        let instr = instr.unwrap();

        if verbose {
            eprintln!("  {}: {:?} {:?}", bc, instr, stack);
        };

        assert!(end_block.is_none());

        match instr {
            BytecodeInstruction::Ldc(idx) | BytecodeInstruction::Ldc2(idx) => {
                let val = constant_from_cpe(&cp[idx as usize], known_objects);
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
            BytecodeInstruction::DupX1 => {
                let val1 = stack.pop().unwrap();
                let val2 = stack.pop().unwrap();

                stack.push(val1);
                stack.push(val2);
                stack.push(val1);
            },
            BytecodeInstruction::Pop => {
                stack.pop();
            },
            BytecodeInstruction::ALoad(idx) => {
                let local_id = locals.get_or_add(idx, MilType::Ref, &mut builder.func.reg_map);
                let reg = builder.allocate_reg(MilType::Ref);
                builder.append_instruction(
                    MilInstructionKind::GetLocal(local_id, reg),
                    bc
                );
                stack.push(reg);
            },
            BytecodeInstruction::ILoad(idx) => {
                let local_id = locals.get_or_add(idx, MilType::Int, &mut builder.func.reg_map);
                let reg = builder.allocate_reg(MilType::Int);
                builder.append_instruction(
                    MilInstructionKind::GetLocal(local_id, reg),
                    bc
                );
                stack.push(reg);
            },
            BytecodeInstruction::AStore(idx) => {
                let local_id = locals.get_or_add(idx, MilType::Ref, &mut builder.func.reg_map);
                let val = stack.pop().unwrap();
                builder.append_instruction(
                    MilInstructionKind::SetLocal(local_id, MilOperand::Register(val)),
                    bc
                );
            },
            BytecodeInstruction::IStore(idx) => {
                let local_id = locals.get_or_add(idx, MilType::Int, &mut builder.func.reg_map);
                let val = stack.pop().unwrap();
                builder.append_instruction(
                    MilInstructionKind::SetLocal(local_id, MilOperand::Register(val)),
                    bc
                );
            },
            BytecodeInstruction::IInc(idx, val) => {
                let local_id = locals.get_or_add(idx, MilType::Int, &mut builder.func.reg_map);

                let load_reg = builder.allocate_reg(MilType::Int);
                builder.append_instruction(
                    MilInstructionKind::GetLocal(local_id, load_reg),
                    bc
                );

                let result_reg = builder.allocate_reg(MilType::Int);
                builder.append_instruction(
                    MilInstructionKind::BinOp(
                        MilBinOp::IAdd,
                        result_reg,
                        MilOperand::Register(load_reg),
                        MilOperand::Int(val as i32)
                    ),
                    bc
                );

                builder.append_instruction(
                    MilInstructionKind::SetLocal(local_id, MilOperand::Register(result_reg)),
                    bc
                );
            },
            BytecodeInstruction::I2C => {
                let val = stack.pop().unwrap();
                let result_reg = builder.allocate_reg(MilType::Int);
                builder.append_instruction(
                    MilInstructionKind::BinOp(
                        MilBinOp::IAnd,
                        result_reg,
                        MilOperand::Register(val),
                        MilOperand::Int(0xffff)
                    ),
                    bc
                );

                stack.push(result_reg);
            },
            BytecodeInstruction::INeg => {
                generate_un_op(builder, &mut stack, bc, MilUnOp::INeg, MilType::Int);
            },
            BytecodeInstruction::IAdd => {
                generate_bin_op(builder, &mut stack, bc, MilBinOp::IAdd, MilType::Int);
            },
            BytecodeInstruction::ISub => {
                generate_bin_op(builder, &mut stack, bc, MilBinOp::ISub, MilType::Int);
            },
            BytecodeInstruction::IMul => {
                generate_bin_op(builder, &mut stack, bc, MilBinOp::IMul, MilType::Int);
            },
            BytecodeInstruction::IDiv => {
                generate_bin_op(builder, &mut stack, bc, MilBinOp::IDivS, MilType::Int);
            },
            BytecodeInstruction::IShr => {
                generate_bin_op(builder, &mut stack, bc, MilBinOp::IShrS, MilType::Int);
            },
            BytecodeInstruction::IUShr => {
                generate_bin_op(builder, &mut stack, bc, MilBinOp::IShrU, MilType::Int);
            },
            BytecodeInstruction::IShl => {
                generate_bin_op(builder, &mut stack, bc, MilBinOp::IShl, MilType::Int);
            },
            BytecodeInstruction::IAnd => {
                generate_bin_op(builder, &mut stack, bc, MilBinOp::IAnd, MilType::Int);
            },
            BytecodeInstruction::IOr => {
                generate_bin_op(builder, &mut stack, bc, MilBinOp::IOr, MilType::Int);
            },
            BytecodeInstruction::IXor => {
                generate_bin_op(builder, &mut stack, bc, MilBinOp::IXor, MilType::Int);
            },
            BytecodeInstruction::New(idx) => {
                let cpe = match cp[idx as usize] {
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
                let cpe = match cp[idx as usize] {
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
            BytecodeInstruction::NewArray(ty) => {
                let reg = builder.allocate_reg(MilType::Ref);
                let len = MilOperand::Register(stack.pop().unwrap());
                builder.append_instruction(
                    MilInstructionKind::AllocArray(ClassId::for_primitive_type_array(ty), reg, len),
                    bc
                );
                stack.push(reg);
            },
            BytecodeInstruction::BALoad => {
                generate_array_load(builder, &mut stack, bc, ClassId::PRIMITIVE_BYTE, MilType::Int);
            },
            BytecodeInstruction::SALoad => {
                generate_array_load(builder, &mut stack, bc, ClassId::PRIMITIVE_SHORT, MilType::Int);
            },
            BytecodeInstruction::CALoad => {
                generate_array_load(builder, &mut stack, bc, ClassId::PRIMITIVE_CHAR, MilType::Int);
            },
            BytecodeInstruction::IALoad => {
                generate_array_load(builder, &mut stack, bc, ClassId::PRIMITIVE_INT, MilType::Int);
            },
            BytecodeInstruction::LALoad => {
                generate_array_load(builder, &mut stack, bc, ClassId::PRIMITIVE_LONG, MilType::Long);
            },
            BytecodeInstruction::AALoad => {
                generate_array_load(builder, &mut stack, bc, ClassId::JAVA_LANG_OBJECT, MilType::Ref);
            }
            BytecodeInstruction::BAStore => {
                generate_array_store(builder, &mut stack, bc, ClassId::PRIMITIVE_BYTE);
            },
            BytecodeInstruction::SAStore => {
                generate_array_store(builder, &mut stack, bc, ClassId::PRIMITIVE_SHORT);
            },
            BytecodeInstruction::CAStore => {
                generate_array_store(builder, &mut stack, bc, ClassId::PRIMITIVE_CHAR);
            },
            BytecodeInstruction::IAStore => {
                generate_array_store(builder, &mut stack, bc, ClassId::PRIMITIVE_INT);
            },
            BytecodeInstruction::LAStore => {
                generate_array_store(builder, &mut stack, bc, ClassId::PRIMITIVE_LONG);
            },
            BytecodeInstruction::AAStore => {
                generate_array_store(builder, &mut stack, bc, ClassId::JAVA_LANG_OBJECT);
            },
            BytecodeInstruction::ArrayLength => {
                let reg = builder.allocate_reg(MilType::Int);
                let obj = MilOperand::Register(stack.pop().unwrap());
                builder.append_instruction(
                    MilInstructionKind::GetArrayLength(reg, obj),
                    bc
                );
                stack.push(reg);
            },
            BytecodeInstruction::InvokeStatic(idx) => {
                let cpe = match cp[idx as usize] {
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
                let cpe = match cp[idx as usize] {
                    ConstantPoolEntry::Methodref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                let ret_class = env.get_method(cpe.method_id).1.return_type;
                let reg = builder.allocate_reg(MilType::for_class(ret_class));
                let args = pop_args(&mut stack, cpe.descriptor.param_types.len() + 1);

                emit_null_check(builder, args[0].clone(), bc);
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
            BytecodeInstruction::InvokeVirtual(idx) => {
                let cpe = match cp[idx as usize] {
                    ConstantPoolEntry::Methodref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                let (class, method) = env.get_method(cpe.method_id);

                let ret_class = method.return_type;
                let reg = builder.allocate_reg(MilType::for_class(ret_class));
                let args = pop_args(&mut stack, cpe.descriptor.param_types.len() + 1);

                emit_null_check(builder, args[0].clone(), bc);
                builder.append_end_instruction(
                    MilEndInstructionKind::CallVirtual(
                        ret_class,
                        cpe.method_id,
                        reg,
                        args[0].clone(),
                        args
                    ),
                    bc
                );
                if ret_class != ClassId::PRIMITIVE_VOID {
                    stack.push(reg);
                };
            },
            BytecodeInstruction::InvokeInterface(idx, _) => {
                let cpe = match cp[idx as usize] {
                    ConstantPoolEntry::InterfaceMethodref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                let (class, method) = env.get_method(cpe.method_id);

                let ret_class = method.return_type;
                let reg = builder.allocate_reg(MilType::for_class(ret_class));
                let args = pop_args(&mut stack, cpe.descriptor.param_types.len() + 1);

                emit_null_check(builder, args[0].clone(), bc);
                builder.append_end_instruction(
                    MilEndInstructionKind::CallInterface(
                        ret_class,
                        cpe.method_id,
                        reg,
                        args[0].clone(),
                        args
                    ),
                    bc
                );
                if ret_class != ClassId::PRIMITIVE_VOID {
                    stack.push(reg);
                };
            },
            BytecodeInstruction::GetField(idx) => {
                let cpe = match cp[idx as usize] {
                    ConstantPoolEntry::Fieldref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                let obj = MilOperand::Register(stack.pop().unwrap());
                let ty = get_mil_type_for_descriptor(&cpe.descriptor);
                let reg = builder.allocate_reg(ty);

                emit_null_check(builder, obj.clone(), bc);
                builder.append_instruction(
                    MilInstructionKind::GetField(cpe.field_id, cpe.type_id, reg, obj),
                    bc
                );

                stack.push(reg);
            },
            BytecodeInstruction::PutField(idx) => {
                let cpe = match cp[idx as usize] {
                    ConstantPoolEntry::Fieldref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                let val = MilOperand::Register(stack.pop().unwrap());
                let obj = MilOperand::Register(stack.pop().unwrap());

                emit_null_check(builder, obj.clone(), bc);
                builder.append_instruction(
                    MilInstructionKind::PutField(cpe.field_id, cpe.type_id, obj, val),
                    bc
                );
            },
            BytecodeInstruction::GetStatic(idx) => {
                let cpe = match cp[idx as usize] {
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
                let cpe = match cp[idx as usize] {
                    ConstantPoolEntry::Fieldref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                let val = MilOperand::Register(stack.pop().unwrap());
                builder.append_instruction(
                    MilInstructionKind::PutStatic(cpe.field_id, cpe.type_id, val),
                    bc
                );
            },
            BytecodeInstruction::If(cond, target) => {
                let val = stack.pop().unwrap();
                let block = builder.append_end_instruction(
                    MilEndInstructionKind::JumpIf(
                        MilComparison::from_bytecode(cond),
                        MilBlockId::ENTRY,
                        MilOperand::Register(val),
                        MilOperand::Int(0)
                    ),
                    bc
                );

                fixups.push(Box::new(move |builder, blocks| {
                    match builder.func.blocks.get_mut(&block).unwrap().end_instr.kind {
                        MilEndInstructionKind::JumpIf(_, ref mut block, _, _) => {
                            *block = blocks[&target].blocks[0];
                        },
                        _ => unreachable!()
                    };
                }));
                end_block = Some(block);
            },
            BytecodeInstruction::IfICmp(cond, target) | BytecodeInstruction::IfACmp(cond, target) => {
                let rhs = stack.pop().unwrap();
                let lhs = stack.pop().unwrap();
                let block = builder.append_end_instruction(
                    MilEndInstructionKind::JumpIf(
                        MilComparison::from_bytecode(cond),
                        MilBlockId::ENTRY,
                        MilOperand::Register(lhs),
                        MilOperand::Register(rhs)
                    ),
                    bc
                );

                fixups.push(Box::new(move |builder, blocks| {
                    match builder.func.blocks.get_mut(&block).unwrap().end_instr.kind {
                        MilEndInstructionKind::JumpIf(_, ref mut block, _, _) => {
                            *block = blocks[&target].blocks[0];
                        },
                        _ => unreachable!()
                    };
                }));
                end_block = Some(block);
            },
            BytecodeInstruction::IfNonNull(target) => {
                let block = builder.append_end_instruction(
                    MilEndInstructionKind::JumpIf(
                        MilComparison::Ne,
                        MilBlockId::ENTRY,
                        MilOperand::Register(stack.pop().unwrap()),
                        MilOperand::Null
                    ),
                    bc
                );

                fixups.push(Box::new(move |builder, blocks| {
                    match builder.func.blocks.get_mut(&block).unwrap().end_instr.kind {
                        MilEndInstructionKind::JumpIf(_, ref mut block, _, _) => {
                            *block = blocks[&target].blocks[0];
                        },
                        _ => unreachable!()
                    };
                }));
                end_block = Some(block);
            },
            BytecodeInstruction::IfNull(target) => {
                let block = builder.append_end_instruction(
                    MilEndInstructionKind::JumpIf(
                        MilComparison::Eq,
                        MilBlockId::ENTRY,
                        MilOperand::Register(stack.pop().unwrap()),
                        MilOperand::Null
                    ),
                    bc
                );

                fixups.push(Box::new(move |builder, blocks| {
                    match builder.func.blocks.get_mut(&block).unwrap().end_instr.kind {
                        MilEndInstructionKind::JumpIf(_, ref mut block, _, _) => {
                            *block = blocks[&target].blocks[0];
                        },
                        _ => unreachable!()
                    };
                }));
                end_block = Some(block);
            },
            BytecodeInstruction::Goto(target) => {
                let block = builder.append_end_instruction(
                    MilEndInstructionKind::Jump(MilBlockId::ENTRY),
                    bc
                );

                fixups.push(Box::new(move |builder, blocks| {
                    match builder.func.blocks.get_mut(&block).unwrap().end_instr.kind {
                        MilEndInstructionKind::Jump(ref mut block) => {
                            *block = blocks[&target].blocks[0];
                        },
                        _ => unreachable!()
                    };
                }));
                end_block = Some(block);
            },
            BytecodeInstruction::AThrow => {
                let val = MilOperand::Register(stack.pop().unwrap());
                end_block = Some(builder.append_end_instruction(
                    MilEndInstructionKind::Throw(val),
                    bc
                ));
            },
            BytecodeInstruction::AReturn | BytecodeInstruction::DReturn | BytecodeInstruction::FReturn | BytecodeInstruction::IReturn | BytecodeInstruction::LReturn => {
                let val = MilOperand::Register(stack.pop().unwrap());
                end_block = Some(builder.append_end_instruction(
                    MilEndInstructionKind::Return(val),
                    bc
                ));
            },
            BytecodeInstruction::Return => {
                end_block = Some(builder.append_end_instruction(
                    MilEndInstructionKind::Return(MilOperand::Register(MilRegister::VOID)),
                    bc
                ));
            },
            BytecodeInstruction::TableSwitch(_, _, _) | BytecodeInstruction::LookupSwitch(_, _) => {
                eprintln!("{}: UNIMPLEMENTED {:?}", MethodName(builder.func.id, env), instr);
                stack.pop();
                end_block = Some(builder.append_end_instruction(
                    MilEndInstructionKind::Throw(MilOperand::Null),
                    bc
                ));
            },
            instr => {
                panic!("Unsupported bytecode {:?} in ilgen", instr);
            }
        };
    };

    let end_block = if let Some(end_block) = end_block {
        end_block
    } else {
        builder.end_block()
    };

    let block_info = blocks.get_mut(&off).unwrap();
    block_info.blocks = builder.end_ordered_blocks();
    block_info.end_stack = stack.clone();

    for succ_bc in blocks.get(&off).unwrap().succs.iter().cloned() {
        let succ = blocks.get(&succ_bc).unwrap();

        if let Some(succ_start_block) = succ.blocks.first() {
            let start_block = builder.func.blocks.get_mut(succ_start_block).unwrap();

            assert!(stack.len() == start_block.phi_nodes.len());

            for (stack_elem, phi) in stack.iter().cloned().zip(start_block.phi_nodes.iter_mut()) {
                assert!(builder.func.reg_map.info[&stack_elem].ty == builder.func.reg_map.info[&phi.sources[0].0].ty);
                phi.sources.push((stack_elem, end_block));
            };
        } else if !block_worklist.contains(&succ_bc) {
            block_worklist.push(succ_bc);
        };
    };
}

fn lower_il_in_method(env: &ClassEnvironment, func: &mut MilFunction, liveness: &LivenessInfo) {
    let cfg = FlowGraph::for_function(func);
    let mut truncated_blocks = vec![];

    for block_id in func.block_order.iter().cloned() {
        let block = func.blocks.get_mut(&block_id).unwrap();

        for (i, instr) in block.instrs.iter().enumerate() {
            let bc = instr.bytecode;

            match instr.kind {
                MilInstructionKind::GetField(field_id, _, _, _) | MilInstructionKind::PutField(field_id, _, _, _)
                    if !liveness.may_construct.contains(&field_id.0) => {
                    block.instrs.truncate(i);
                    block.end_instr = MilEndInstruction {
                        kind: MilEndInstructionKind::Unreachable,
                        bytecode: bc
                    };

                    break;
                },
                _ => {}
            };
        };

        match block.end_instr.kind {
            MilEndInstructionKind::CallVirtual(return_ty, method_id, result_reg, _, ref mut args) => {
                let (_, method) = env.get_method(method_id);

                if !liveness.may_construct.contains(&method_id.0) {
                    block.end_instr.kind = MilEndInstructionKind::Unreachable;
                    truncated_blocks.push(block_id);
                } else if method.virtual_slot == !0 {
                    let args = std::mem::replace(args, vec![]);
                    block.end_instr.kind = MilEndInstructionKind::Call(return_ty, method_id, result_reg, args);
                };
            },
            MilEndInstructionKind::CallInterface(return_ty, method_id, result_reg, _, _) => {
                if !liveness.may_construct.contains(&method_id.0) {
                    block.end_instr.kind = MilEndInstructionKind::Unreachable;
                    truncated_blocks.push(block_id);
                };
            },
            _ => {}
        };

        match block.end_instr.kind {
            MilEndInstructionKind::Call(_, method_id, _, _) => {
                let (_, method) = env.get_method(method_id);

                if method.flags.contains(MethodFlags::ABSTRACT) {
                    block.end_instr.kind = MilEndInstructionKind::Unreachable;
                    truncated_blocks.push(block_id);
                };
            },
            _ => {}
        }
    };

    for block_id in truncated_blocks {
        for succ in cfg.get(block_id).outgoing.iter().copied() {
            transform::remove_incoming_phis(func.blocks.get_mut(&succ).unwrap(), block_id);
        };
    };
}

pub fn generate_il_for_method(env: &ClassEnvironment, method_id: MethodId, known_objects: &MilKnownObjectRefs, liveness: &LivenessInfo, verbose: bool) -> Option<MilFunction> {
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
    let mut fixups = vec![];

    let mut blocks = scan_blocks(instrs);
    let mut block_worklist = vec![0];
    let mut builder = MilBuilder::new(method_id);

    get_params(&mut builder, &mut locals, method);

    while let Some(next_block) = block_worklist.pop() {
        generate_il_for_block(env, &mut builder, code, next_block, &class.constant_pool, &mut blocks, &mut block_worklist, &mut locals, &mut fixups, known_objects, verbose);
    };

    for mut fixup in fixups {
        fixup(&mut builder, &blocks);
    };

    for (_, block_info) in blocks.into_iter().sorted_by_key(|&(bc, _)| bc) {
        builder.func.block_order.extend(block_info.blocks);
    }

    let mut func = builder.finish();
    lower_il_in_method(env, &mut func, liveness);

    if verbose {
        eprintln!("{}", func.pretty(env));
    };

    Some(func)
}
