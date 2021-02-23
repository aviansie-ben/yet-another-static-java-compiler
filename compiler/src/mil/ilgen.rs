use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::fmt;
use std::sync::Arc;

use itertools::Itertools;
use smallvec::*;

use super::flow_graph::*;
use super::il::*;
use super::transform;
use crate::bytecode::{BytecodeInstruction, BytecodeIterator};
use crate::classfile::{AttributeCode, Class, ClassFlags, ConstantPoolEntry, FlatTypeDescriptor, LocalVariableTableEntry, Method, MethodBody, MethodFlags, PrimitiveType, TypeDescriptor};
use crate::liveness::LivenessInfo;
use crate::resolve::{ClassEnvironment, ClassId, MethodId};

pub struct MilBuilder<'a> {
    func: MilFunction,
    current_block: MilBlock,
    bc: u32,
    verbose: bool,
    env: &'a ClassEnvironment
}

impl <'a> MilBuilder<'a> {
    pub fn new(id: MethodId, verbose: bool, env: &'a ClassEnvironment) -> MilBuilder {
        let func = MilFunction::new(id);

        if verbose {
            eprintln!("  {}:", func.block_alloc.next());
        };

        MilBuilder {
            func,
            current_block: MilBlock::new(),
            bc: !0,
            verbose,
            env
        }
    }

    pub fn set_bytecode(&mut self, bc: u32) {
        self.bc = bc;
    }

    pub fn append_phi_node(&mut self, srcs: impl IntoIterator<Item=(MilOperand, MilBlockId)>) -> MilOperand {
        assert!(self.current_block.instrs.is_empty());

        let mut ty = None;
        let srcs = srcs.into_iter().map(|(val, block)| {
            let new_ty = val.get_type();
            if let Some(ty) = ty {
                assert_eq!(new_ty, ty);
            } else {
                ty = Some(new_ty);
            };

            (val, block)
        }).collect();

        let ty = ty.unwrap();
        let reg = self.allocate_reg();
        let phi_node = MilPhiNode {
            target: reg,
            ty,
            sources: srcs,
            bytecode: (!0, self.bc)
        };

        if self.verbose {
            eprintln!("    {}", phi_node.pretty(self.env));
        };

        self.current_block.phi_nodes.push(phi_node);

        MilOperand::Register(ty, reg)
    }

    pub fn allocate_reg(&mut self) -> MilRegister {
        self.func.reg_alloc.allocate_one()
    }

    pub fn append_instruction(&mut self, kind: MilInstructionKind) {
        let instr = MilInstruction {
            kind,
            bytecode: (!0, self.bc)
        };

        if self.verbose {
            eprintln!("    {}", instr.pretty(self.env));
        };

        self.current_block.instrs.push(instr);
    }

    pub fn append_end_instruction(&mut self, kind: MilEndInstructionKind) -> MilBlockId {
        let instr = MilEndInstruction {
            kind,
            bytecode: (!0, self.bc)
        };

        if self.verbose && &instr.kind != &MilEndInstructionKind::Nop {
            eprintln!("    {}", instr.pretty(self.env));
        };

        self.current_block.end_instr = instr;
        self.end_block_impl()
    }

    pub fn insert_end_instruction(&mut self, block: MilBlockId, kind: MilEndInstructionKind) {
        let block = self.func.blocks.get_mut(&block).unwrap();

        block.end_instr = MilEndInstruction {
            kind,
            bytecode: (!0, self.bc)
        };
    }

    fn end_block_impl(&mut self) -> MilBlockId {
        let id = self.func.block_alloc.allocate_one();
        self.current_block.id = id;
        self.func.blocks.insert(id, std::mem::replace(&mut self.current_block, MilBlock::new()));
        self.func.block_order.push(id);

        if self.verbose {
            eprintln!("  {}:", self.func.block_alloc.next());
        };

        id
    }

    pub fn end_block(&mut self) -> MilBlockId {
        self.append_end_instruction(MilEndInstructionKind::Nop)
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

    fn get(&self, local: u16, ty: MilType) -> Option<MilLocalId> {
        self.locals[local as usize][MilLocals::type_index(ty)]
    }

    fn get_or_add(&mut self, local: u16, ty: MilType, local_info: &mut HashMap<MilLocalId, MilLocalInfo>) -> MilLocalId {
        if let Some(id) = self.locals[local as usize][MilLocals::type_index(ty)] {
            id
        } else {
            let id = self.next_local;
            self.next_local.0 += 1;

            self.locals[local as usize][MilLocals::type_index(ty)] = Some(id);
            local_info.insert(id, MilLocalInfo {
                java_local: local,
                ty
            });

            id
        }
    }
}

struct MilVirtualStack {
    stack: Vec<Option<MilOperand>>
}

impl MilVirtualStack {
    fn new(stack: Vec<Option<MilOperand>>) -> MilVirtualStack {
        MilVirtualStack { stack }
    }

    fn push(&mut self, val: MilOperand) {
        let ty = val.get_type();

        self.push_slot(Some(val));
        if ty == MilType::Double || ty == MilType::Long {
            self.push_slot(None);
        };
    }

    fn push_slot(&mut self, val: Option<MilOperand>) {
        self.stack.push(val);
    }

    fn pop(&mut self, ty: MilType) -> MilOperand {
        if ty == MilType::Double || ty == MilType::Long {
            assert_eq!(self.pop_slot(), None);
        };

        let val = self.pop_slot().unwrap();
        assert_eq!(val.get_type(), ty);
        val
    }

    fn pop_any(&mut self) -> MilOperand {
        while self.peek().is_none() {
            self.pop_slot();
        };

        self.pop_slot().unwrap()
    }

    fn pop_slot(&mut self) -> Option<MilOperand> {
        self.stack.pop().unwrap()
    }

    fn peek(&self) -> &Option<MilOperand> {
        self.read(0)
    }

    fn read(&self, depth: usize) -> &Option<MilOperand> {
        &self.stack[self.stack.len() - 1 - depth]
    }

    fn as_slice(&self) -> &[Option<MilOperand>] {
        &self.stack
    }

    fn non_empty_slots<'a>(&'a self) -> impl Iterator<Item=&MilOperand> + 'a {
        self.stack.iter().filter_map(|val| val.as_ref())
    }

    fn pretty<'a>(&'a self, env: &'a ClassEnvironment) -> impl fmt::Display + 'a {
        PrettyMilVirtualStack(self, env)
    }
}

struct PrettyMilVirtualStack<'a>(&'a MilVirtualStack, &'a ClassEnvironment);

impl <'a> fmt::Display for PrettyMilVirtualStack<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let PrettyMilVirtualStack(stack, env) = *self;
        write!(f, "[ ")?;
        for val in stack.as_slice().iter() {
            if let Some(val) = val {
                write!(f, "{} ", val.pretty(env))?;
            } else {
                write!(f, "(empty) ")?;
            };
        };
        write!(f, "]")
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

fn pop_args(stack: &mut MilVirtualStack, tys: &[TypeDescriptor], has_receiver: bool) -> Vec<MilOperand> {
    let mut args = vec![];

    for ty in tys.iter().rev() {
        args.push(stack.pop(get_mil_type_for_descriptor(ty)));
    };

    if has_receiver {
        args.push(stack.pop(MilType::Ref));
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

fn read_param(builder: &mut MilBuilder, i: u16, param_constraint: MilClassConstraint) -> MilRegister {
    let reg = builder.allocate_reg();

    builder.append_instruction(
        if param_constraint.class_id() != ClassId::UNRESOLVED {
            MilInstructionKind::GetParam(i.try_into().unwrap(), param_constraint, reg)
        } else {
            MilInstructionKind::Copy(reg, MilOperand::RefNull)
        }
    );

    reg
}

fn get_params(builder: &mut MilBuilder, locals: &mut MilLocals, method: &Method) {
    let mut next_param_local = 0;

    for (i, param_type) in method.param_types.iter().cloned().enumerate() {
        let local_id = locals.get_or_add(
            next_param_local,
            MilType::for_class(param_type),
            &mut builder.func.local_info
        );
        let param_constraint = if i == 0 && !method.flags.contains(MethodFlags::STATIC) {
            MilClassConstraint::for_class(param_type).not_null()
        } else {
            MilClassConstraint::for_class(param_type)
        };
        let reg = read_param(builder, i.try_into().unwrap(), param_constraint);
        builder.append_instruction(MilInstructionKind::SetLocal(local_id, MilOperand::Register(MilType::for_class(param_type), reg)));

        next_param_local += 1;
        if param_type.needs_dual_slot() {
            next_param_local += 1;
        };
    };
}

fn generate_native_thunk(name: String, method: &Method, method_id: MethodId, known_objects: &MilKnownObjectRefs, env: &ClassEnvironment) -> MilFunction {
    let mut builder = MilBuilder::new(method_id, false, env);

    let mut args = vec![];

    if method.flags.contains(MethodFlags::STATIC) {
        args.push(MilOperand::KnownObject(known_objects.classes[&method_id.0], ClassId::JAVA_LANG_CLASS));
    };

    args.extend(
        method.param_types.iter().cloned().enumerate()
            .map(|(i, param_type)| MilOperand::Register(
                MilType::for_class(param_type),
                read_param(&mut builder, i.try_into().unwrap(), MilClassConstraint::for_class(param_type))
            ))
    );

    let reg = if method.return_type == ClassId::PRIMITIVE_VOID {
        MilRegister::VOID
    } else {
        builder.allocate_reg()
    };

    builder.append_end_instruction(MilEndInstructionKind::CallNative(method.return_type, name, reg, args));
    builder.append_end_instruction(MilEndInstructionKind::Return(if reg != MilRegister::VOID {
        Some(MilOperand::Register(MilType::for_class(method.return_type), reg))
    } else {
        None
    }));

    let mut func = builder.finish();
    func.source_file = (String::from(""), String::from("<native thunk>"));

    func
}

struct GenBlockInfo {
    preds: SmallVec<[usize; 2]>,
    succs: SmallVec<[usize; 2]>,
    blocks: Vec<MilBlockId>,
    end_stack: Vec<Option<MilOperand>>,
    fallthrough_bc: usize
}

impl GenBlockInfo {
    pub fn new() -> GenBlockInfo {
        GenBlockInfo {
            preds: smallvec![],
            succs: smallvec![],
            blocks: vec![],
            end_stack: vec![],
            fallthrough_bc: !0
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
                blocks.get_mut(&current_block).unwrap().fallthrough_bc = bc;
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

fn emit_npe_throw(builder: &mut MilBuilder) -> MilBlockId {
    // TODO Create an actual exception
    builder.append_end_instruction(MilEndInstructionKind::Throw(MilOperand::RefNull))
}

fn emit_null_check(builder: &mut MilBuilder, val: MilOperand) {
    let check_cond = builder.allocate_reg();
    builder.append_instruction(
        MilInstructionKind::BinOp(MilBinOp::RCmp(MilRefComparison::Ne), check_cond, val, MilOperand::RefNull)
    );

    let check_block = builder.end_block();
    let throw_block = emit_npe_throw(builder);

    let not_null_block = builder.end_block();
    builder.insert_end_instruction(
        check_block,
        MilEndInstructionKind::JumpIf(not_null_block, throw_block, MilOperand::Register(MilType::Bool, check_cond))
    );
}

fn generate_local_load(builder: &mut MilBuilder, stack: &mut MilVirtualStack, locals: &mut MilLocals, idx: u16, ty: MilType) {
    let local_id = locals.get_or_add(idx, ty, &mut builder.func.local_info);
    let reg = builder.allocate_reg();
    builder.append_instruction(MilInstructionKind::GetLocal(local_id, reg));
    stack.push(MilOperand::Register(ty, reg));
}

fn generate_local_store(builder: &mut MilBuilder, stack: &mut MilVirtualStack, locals: &mut MilLocals, idx: u16, ty: MilType) {
    let local_id = locals.get_or_add(idx, ty, &mut builder.func.local_info);
    let val = stack.pop(ty);
    builder.append_instruction(MilInstructionKind::SetLocal(local_id, val));
}

fn generate_un_op(builder: &mut MilBuilder, stack: &mut MilVirtualStack, op: MilUnOp, op_ty: MilType, result_ty: MilType) {
    let reg = builder.allocate_reg();
    let val = stack.pop(op_ty);

    builder.append_instruction(MilInstructionKind::UnOp(op, reg, val));
    stack.push(MilOperand::Register(result_ty, reg));
}

fn generate_bin_op(builder: &mut MilBuilder, stack: &mut MilVirtualStack, op: MilBinOp, op1_ty: MilType, op2_ty: MilType, result_ty: MilType) {
    let reg = builder.allocate_reg();
    let rhs = stack.pop(op2_ty);
    let lhs = stack.pop(op1_ty);

    builder.append_instruction(MilInstructionKind::BinOp(op, reg, lhs, rhs));
    stack.push(MilOperand::Register(result_ty, reg));
}

fn generate_array_load(builder: &mut MilBuilder, stack: &mut MilVirtualStack, elem_class: ClassId, elem_ty: MilType) {
    let reg = builder.allocate_reg();
    let idx = stack.pop(MilType::Int);
    let obj = stack.pop(MilType::Ref);

    builder.append_instruction(MilInstructionKind::GetArrayElement(elem_class, reg, obj, idx));
    stack.push(MilOperand::Register(elem_ty, reg));
}

fn generate_array_store(builder: &mut MilBuilder, stack: &mut MilVirtualStack, elem_class: ClassId, elem_ty: MilType) {
    let val = stack.pop(elem_ty);
    let idx = stack.pop(MilType::Int);
    let obj = stack.pop(MilType::Ref);

    builder.append_instruction(MilInstructionKind::PutArrayElement(elem_class, obj, idx, val));
}

fn generate_il_for_block(env: &ClassEnvironment, builder: &mut MilBuilder, code: &AttributeCode, off: usize, cp: &[ConstantPoolEntry], blocks: &mut HashMap<usize, GenBlockInfo>, block_worklist: &mut Vec<usize>, locals: &mut MilLocals, fixups: &mut Vec<Box<dyn FnMut (&mut MilBuilder, &HashMap<usize, GenBlockInfo>) -> ()>>, known_objects: &MilKnownObjectRefs, verbose: bool) {
    if verbose {
        eprintln!("(Start of bytecode basic block at {})", off);
    };

    let block_info = &blocks[&off];
    let fallthrough_bc = block_info.fallthrough_bc;
    let incoming_stacks = block_info.preds.iter().filter_map(|pred| {
        let pred = blocks.get(&pred).unwrap();

        if let Some(&pred_end_block) = pred.blocks.last() {
            Some((pred_end_block, &pred.end_stack))
        } else {
            None
        }
    }).collect_vec();

    builder.set_bytecode(off as u32);
    let mut stack = MilVirtualStack::new(if !incoming_stacks.is_empty() {
        assert!(incoming_stacks.iter().skip(1).all(|(_, s)| s.len() == incoming_stacks[0].1.len()));
        (0..incoming_stacks[0].1.len()).map(|i| {
            if incoming_stacks[0].1[i].is_some() {
                Some(builder.append_phi_node(incoming_stacks.iter().map(|&(b, s)| (s[i].clone().unwrap(), b))))
            } else {
                None
            }
        }).collect_vec()
    } else {
        vec![]
    });

    builder.end_block();
    let mut end_block = None;

    for (bc, instr) in BytecodeIterator(&code.code, off).take_while(|&(bc, _)| bc == off || !blocks.contains_key(&bc)) {
        let instr = instr.unwrap();
        builder.set_bytecode(bc as u32);

        if verbose {
            eprintln!("{}: {} {}", bc, instr.pretty(cp), stack.pretty(env));
        };

        assert!(end_block.is_none());

        match instr {
            BytecodeInstruction::Ldc(idx) | BytecodeInstruction::Ldc2(idx) => {
                let val = constant_from_cpe(&cp[idx as usize], known_objects);
                stack.push(val);
            },
            BytecodeInstruction::AConstNull => {
                stack.push(MilOperand::RefNull);
            },
            BytecodeInstruction::IConst(val) => {
                stack.push(MilOperand::Int(val));
            },
            BytecodeInstruction::LConst(val) => {
                stack.push(MilOperand::Long(val));
            },
            BytecodeInstruction::FConst(val) => {
                stack.push(MilOperand::Float(val));
            },
            BytecodeInstruction::DConst(val) => {
                stack.push(MilOperand::Double(val));
            },
            BytecodeInstruction::Dup => {
                stack.push_slot(stack.peek().clone());
            },
            BytecodeInstruction::DupX1 => {
                let val1 = stack.pop_slot();
                let val2 = stack.pop_slot();

                stack.push_slot(val1.clone());
                stack.push_slot(val2);
                stack.push_slot(val1);
            },
            BytecodeInstruction::DupX2 => {
                let val1 = stack.pop_slot();
                let val2 = stack.pop_slot();
                let val3 = stack.pop_slot();

                stack.push_slot(val1.clone());
                stack.push_slot(val3);
                stack.push_slot(val2);
                stack.push_slot(val1);
            },
            BytecodeInstruction::Dup2 => {
                stack.push_slot(stack.read(1).clone());
                stack.push_slot(stack.read(1).clone());
            },
            BytecodeInstruction::Dup2X1 => {
                let val1 = stack.pop_slot();
                let val2 = stack.pop_slot();
                let val3 = stack.pop_slot();

                stack.push_slot(val2.clone());
                stack.push_slot(val1.clone());
                stack.push_slot(val3);
                stack.push_slot(val2);
                stack.push_slot(val1);
            },
            BytecodeInstruction::Dup2X2 => {
                let val1 = stack.pop_slot();
                let val2 = stack.pop_slot();
                let val3 = stack.pop_slot();
                let val4 = stack.pop_slot();

                stack.push_slot(val2.clone());
                stack.push_slot(val1.clone());
                stack.push_slot(val4);
                stack.push_slot(val3);
                stack.push_slot(val2);
                stack.push_slot(val1);
            },
            BytecodeInstruction::Pop => {
                stack.pop_slot();
            },
            BytecodeInstruction::Pop2 => {
                stack.pop_slot();
                stack.pop_slot();
            },
            BytecodeInstruction::ALoad(idx) => {
                generate_local_load(builder, &mut stack, locals, idx, MilType::Ref);
            },
            BytecodeInstruction::ILoad(idx) => {
                generate_local_load(builder, &mut stack, locals, idx, MilType::Int);
            },
            BytecodeInstruction::LLoad(idx) => {
                generate_local_load(builder, &mut stack, locals, idx, MilType::Long);
            },
            BytecodeInstruction::FLoad(idx) => {
                generate_local_load(builder, &mut stack, locals, idx, MilType::Float);
            },
            BytecodeInstruction::DLoad(idx) => {
                generate_local_load(builder, &mut stack, locals, idx, MilType::Double);
            },
            BytecodeInstruction::AStore(idx) => {
                generate_local_store(builder, &mut stack, locals, idx, MilType::Ref);
            },
            BytecodeInstruction::IStore(idx) => {
                generate_local_store(builder, &mut stack, locals, idx, MilType::Int);
            },
            BytecodeInstruction::LStore(idx) => {
                generate_local_store(builder, &mut stack, locals, idx, MilType::Long);
            },
            BytecodeInstruction::FStore(idx) => {
                generate_local_store(builder, &mut stack, locals, idx, MilType::Float);
            },
            BytecodeInstruction::DStore(idx) => {
                generate_local_store(builder, &mut stack, locals, idx, MilType::Double);
            },
            BytecodeInstruction::IInc(idx, val) => {
                let local_id = locals.get_or_add(idx, MilType::Int, &mut builder.func.local_info);

                let load_reg = builder.allocate_reg();
                builder.append_instruction(MilInstructionKind::GetLocal(local_id, load_reg));

                let result_reg = builder.allocate_reg();
                builder.append_instruction(MilInstructionKind::BinOp(
                    MilBinOp::IAdd,
                    result_reg,
                    MilOperand::Register(MilType::Int, load_reg),
                    MilOperand::Int(val as i32)
                ));

                builder.append_instruction(MilInstructionKind::SetLocal(local_id, MilOperand::Register(MilType::Int, result_reg)));
            },
            BytecodeInstruction::I2B => {
                generate_un_op(builder, &mut stack, MilUnOp::IExtB, MilType::Int, MilType::Int);
            },
            BytecodeInstruction::I2S => {
                generate_un_op(builder, &mut stack, MilUnOp::IExtS, MilType::Int, MilType::Int);
            }
            BytecodeInstruction::I2C => {
                let val = stack.pop(MilType::Int);
                let result_reg = builder.allocate_reg();
                builder.append_instruction(MilInstructionKind::BinOp(MilBinOp::IAnd, result_reg, val, MilOperand::Int(0xffff)));

                stack.push(MilOperand::Register(MilType::Int, result_reg));
            },
            BytecodeInstruction::I2L => {
                generate_un_op(builder, &mut stack, MilUnOp::I2L, MilType::Int, MilType::Long);
            },
            BytecodeInstruction::I2F => {
                generate_un_op(builder, &mut stack, MilUnOp::I2F, MilType::Int, MilType::Float);
            },
            BytecodeInstruction::I2D => {
                generate_un_op(builder, &mut stack, MilUnOp::I2D, MilType::Int, MilType::Double);
            },
            BytecodeInstruction::INeg => {
                generate_un_op(builder, &mut stack, MilUnOp::INeg, MilType::Int, MilType::Int);
            },
            BytecodeInstruction::IAdd => {
                generate_bin_op(builder, &mut stack, MilBinOp::IAdd, MilType::Int, MilType::Int, MilType::Int);
            },
            BytecodeInstruction::ISub => {
                generate_bin_op(builder, &mut stack, MilBinOp::ISub, MilType::Int, MilType::Int, MilType::Int);
            },
            BytecodeInstruction::IMul => {
                generate_bin_op(builder, &mut stack, MilBinOp::IMul, MilType::Int, MilType::Int, MilType::Int);
            },
            BytecodeInstruction::IDiv => {
                generate_bin_op(builder, &mut stack, MilBinOp::IDivS, MilType::Int, MilType::Int, MilType::Int);
            },
            BytecodeInstruction::IRem => {
                generate_bin_op(builder, &mut stack, MilBinOp::IRemS, MilType::Int, MilType::Int, MilType::Int);
            },
            BytecodeInstruction::IShr => {
                generate_bin_op(builder, &mut stack, MilBinOp::IShrS, MilType::Int, MilType::Int, MilType::Int);
            },
            BytecodeInstruction::IUShr => {
                generate_bin_op(builder, &mut stack, MilBinOp::IShrU, MilType::Int, MilType::Int, MilType::Int);
            },
            BytecodeInstruction::IShl => {
                generate_bin_op(builder, &mut stack, MilBinOp::IShl, MilType::Int, MilType::Int, MilType::Int);
            },
            BytecodeInstruction::IAnd => {
                generate_bin_op(builder, &mut stack, MilBinOp::IAnd, MilType::Int, MilType::Int, MilType::Int);
            },
            BytecodeInstruction::IOr => {
                generate_bin_op(builder, &mut stack, MilBinOp::IOr, MilType::Int, MilType::Int, MilType::Int);
            },
            BytecodeInstruction::IXor => {
                generate_bin_op(builder, &mut stack, MilBinOp::IXor, MilType::Int, MilType::Int, MilType::Int);
            },
            BytecodeInstruction::L2I => {
                generate_un_op(builder, &mut stack, MilUnOp::L2I, MilType::Long, MilType::Int);
            },
            BytecodeInstruction::L2F => {
                generate_un_op(builder, &mut stack, MilUnOp::L2F, MilType::Long, MilType::Float);
            },
            BytecodeInstruction::L2D => {
                generate_un_op(builder, &mut stack, MilUnOp::L2D, MilType::Long, MilType::Double);
            },
            BytecodeInstruction::LNeg => {
                generate_un_op(builder, &mut stack, MilUnOp::LNeg, MilType::Long, MilType::Long);
            },
            BytecodeInstruction::LAdd => {
                generate_bin_op(builder, &mut stack, MilBinOp::LAdd, MilType::Long, MilType::Long, MilType::Long);
            },
            BytecodeInstruction::LSub => {
                generate_bin_op(builder, &mut stack, MilBinOp::LSub, MilType::Long, MilType::Long, MilType::Long);
            },
            BytecodeInstruction::LMul => {
                generate_bin_op(builder, &mut stack, MilBinOp::LMul, MilType::Long, MilType::Long, MilType::Long);
            },
            BytecodeInstruction::LDiv => {
                generate_bin_op(builder, &mut stack, MilBinOp::LDivS, MilType::Long, MilType::Long, MilType::Long);
            },
            BytecodeInstruction::LRem => {
                generate_bin_op(builder, &mut stack, MilBinOp::LRemS, MilType::Long, MilType::Long, MilType::Long);
            },
            BytecodeInstruction::LShr => {
                generate_bin_op(builder, &mut stack, MilBinOp::LShrS, MilType::Long, MilType::Int, MilType::Long);
            },
            BytecodeInstruction::LUShr => {
                generate_bin_op(builder, &mut stack, MilBinOp::LShrU, MilType::Long, MilType::Int, MilType::Long);
            },
            BytecodeInstruction::LShl => {
                generate_bin_op(builder, &mut stack, MilBinOp::LShl, MilType::Long, MilType::Int, MilType::Long);
            },
            BytecodeInstruction::LAnd => {
                generate_bin_op(builder, &mut stack, MilBinOp::LAnd, MilType::Long, MilType::Long, MilType::Long);
            },
            BytecodeInstruction::LOr => {
                generate_bin_op(builder, &mut stack, MilBinOp::LOr, MilType::Long, MilType::Long, MilType::Long);
            },
            BytecodeInstruction::LXor => {
                generate_bin_op(builder, &mut stack, MilBinOp::LXor, MilType::Long, MilType::Long, MilType::Long);
            },
            BytecodeInstruction::F2I => {
                generate_un_op(builder, &mut stack, MilUnOp::F2I, MilType::Float, MilType::Int);
            },
            BytecodeInstruction::F2L => {
                generate_un_op(builder, &mut stack, MilUnOp::F2L, MilType::Float, MilType::Long);
            },
            BytecodeInstruction::F2D => {
                generate_un_op(builder, &mut stack, MilUnOp::F2D, MilType::Float, MilType::Double);
            },
            BytecodeInstruction::FNeg => {
                generate_un_op(builder, &mut stack, MilUnOp::FNeg, MilType::Float, MilType::Float);
            },
            BytecodeInstruction::FAdd => {
                generate_bin_op(builder, &mut stack, MilBinOp::FAdd, MilType::Float, MilType::Float, MilType::Float);
            },
            BytecodeInstruction::FSub => {
                generate_bin_op(builder, &mut stack, MilBinOp::FSub, MilType::Float, MilType::Float, MilType::Float);
            },
            BytecodeInstruction::FMul => {
                generate_bin_op(builder, &mut stack, MilBinOp::FMul, MilType::Float, MilType::Float, MilType::Float);
            },
            BytecodeInstruction::FDiv => {
                generate_bin_op(builder, &mut stack, MilBinOp::FDiv, MilType::Float, MilType::Float, MilType::Float);
            },
            BytecodeInstruction::FRem => {
                generate_bin_op(builder, &mut stack, MilBinOp::FRem, MilType::Float, MilType::Float, MilType::Float);
            },
            BytecodeInstruction::D2I => {
                generate_un_op(builder, &mut stack, MilUnOp::D2I, MilType::Double, MilType::Int);
            },
            BytecodeInstruction::D2L => {
                generate_un_op(builder, &mut stack, MilUnOp::D2L, MilType::Double, MilType::Long);
            },
            BytecodeInstruction::D2F => {
                generate_un_op(builder, &mut stack, MilUnOp::D2F, MilType::Double, MilType::Float);
            },
            BytecodeInstruction::DNeg => {
                generate_un_op(builder, &mut stack, MilUnOp::DNeg, MilType::Double, MilType::Double);
            },
            BytecodeInstruction::DAdd => {
                generate_bin_op(builder, &mut stack, MilBinOp::DAdd, MilType::Double, MilType::Double, MilType::Double);
            },
            BytecodeInstruction::DSub => {
                generate_bin_op(builder, &mut stack, MilBinOp::DSub, MilType::Double, MilType::Double, MilType::Double);
            },
            BytecodeInstruction::DMul => {
                generate_bin_op(builder, &mut stack, MilBinOp::DMul, MilType::Double, MilType::Double, MilType::Double);
            },
            BytecodeInstruction::DDiv => {
                generate_bin_op(builder, &mut stack, MilBinOp::DDiv, MilType::Double, MilType::Double, MilType::Double);
            },
            BytecodeInstruction::DRem => {
                generate_bin_op(builder, &mut stack, MilBinOp::DRem, MilType::Double, MilType::Double, MilType::Double);
            },
            // TODO Support multithreading
            BytecodeInstruction::MonitorEnter => {
                stack.pop(MilType::Ref);
            },
            BytecodeInstruction::MonitorExit => {
                stack.pop(MilType::Ref);
            },
            BytecodeInstruction::New(idx) => {
                let cpe = match cp[idx as usize] {
                    ConstantPoolEntry::Class(ref cpe) => cpe,
                    _ => unreachable!()
                };

                let reg = builder.allocate_reg();
                builder.append_instruction(MilInstructionKind::AllocObj(cpe.class_id, reg));
                stack.push(MilOperand::Register(MilType::Ref, reg));
            },
            BytecodeInstruction::ANewArray(idx) => {
                let cpe = match cp[idx as usize] {
                    ConstantPoolEntry::Class(ref cpe) => cpe,
                    _ => unreachable!()
                };

                let reg = builder.allocate_reg();
                let len = stack.pop(MilType::Int);
                builder.append_instruction(MilInstructionKind::AllocArray(cpe.array_class_id, reg, len));
                stack.push(MilOperand::Register(MilType::Ref, reg));
            },
            BytecodeInstruction::NewArray(ty) => {
                let reg = builder.allocate_reg();
                let len = stack.pop(MilType::Int);
                builder.append_instruction(MilInstructionKind::AllocArray(ClassId::for_primitive_type_array(ty), reg, len));
                stack.push(MilOperand::Register(MilType::Ref, reg));
            },
            BytecodeInstruction::BALoad => {
                generate_array_load(builder, &mut stack, ClassId::PRIMITIVE_BYTE, MilType::Int);
            },
            BytecodeInstruction::SALoad => {
                generate_array_load(builder, &mut stack, ClassId::PRIMITIVE_SHORT, MilType::Int);
            },
            BytecodeInstruction::CALoad => {
                generate_array_load(builder, &mut stack, ClassId::PRIMITIVE_CHAR, MilType::Int);
            },
            BytecodeInstruction::IALoad => {
                generate_array_load(builder, &mut stack, ClassId::PRIMITIVE_INT, MilType::Int);
            },
            BytecodeInstruction::LALoad => {
                generate_array_load(builder, &mut stack, ClassId::PRIMITIVE_LONG, MilType::Long);
            },
            BytecodeInstruction::AALoad => {
                generate_array_load(builder, &mut stack, ClassId::JAVA_LANG_OBJECT, MilType::Ref);
            },
            BytecodeInstruction::FALoad => {
                generate_array_load(builder, &mut stack, ClassId::PRIMITIVE_FLOAT, MilType::Float);
            },
            BytecodeInstruction::DALoad => {
                generate_array_load(builder, &mut stack, ClassId::PRIMITIVE_DOUBLE, MilType::Double);
            },
            BytecodeInstruction::BAStore => {
                generate_array_store(builder, &mut stack, ClassId::PRIMITIVE_BYTE, MilType::Int);
            },
            BytecodeInstruction::SAStore => {
                generate_array_store(builder, &mut stack, ClassId::PRIMITIVE_SHORT, MilType::Int);
            },
            BytecodeInstruction::CAStore => {
                generate_array_store(builder, &mut stack, ClassId::PRIMITIVE_CHAR, MilType::Int);
            },
            BytecodeInstruction::IAStore => {
                generate_array_store(builder, &mut stack, ClassId::PRIMITIVE_INT, MilType::Int);
            },
            BytecodeInstruction::LAStore => {
                generate_array_store(builder, &mut stack, ClassId::PRIMITIVE_LONG, MilType::Long);
            },
            BytecodeInstruction::AAStore => {
                generate_array_store(builder, &mut stack, ClassId::JAVA_LANG_OBJECT, MilType::Ref);
            },
            BytecodeInstruction::FAStore => {
                generate_array_store(builder, &mut stack, ClassId::PRIMITIVE_FLOAT, MilType::Float);
            },
            BytecodeInstruction::DAStore => {
                generate_array_store(builder, &mut stack, ClassId::PRIMITIVE_DOUBLE, MilType::Double);
            },
            BytecodeInstruction::ArrayLength => {
                let reg = builder.allocate_reg();
                let obj = stack.pop(MilType::Ref);
                builder.append_instruction(MilInstructionKind::GetArrayLength(reg, obj));
                stack.push(MilOperand::Register(MilType::Int, reg));
            },
            BytecodeInstruction::InvokeStatic(idx) => {
                let cpe = match cp[idx as usize] {
                    ConstantPoolEntry::Methodref(ref cpe) => cpe,
                    ConstantPoolEntry::InterfaceMethodref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                let ret_class = env.get_method(cpe.method_id).1.return_type;
                let reg = if ret_class == ClassId::PRIMITIVE_VOID {
                    MilRegister::VOID
                } else {
                    builder.allocate_reg()
                };
                let args = pop_args(&mut stack, &cpe.descriptor.param_types, false);
                builder.append_end_instruction(MilEndInstructionKind::Call(
                    ret_class,
                    cpe.method_id,
                    reg,
                    args
                ));
                if ret_class != ClassId::PRIMITIVE_VOID {
                    stack.push(MilOperand::Register(MilType::for_class(ret_class), reg));
                };
            },
            BytecodeInstruction::InvokeSpecial(idx) => {
                let cpe = match cp[idx as usize] {
                    ConstantPoolEntry::Methodref(ref cpe) => cpe,
                    ConstantPoolEntry::InterfaceMethodref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                let ret_class = env.get_method(cpe.method_id).1.return_type;
                let reg = if ret_class == ClassId::PRIMITIVE_VOID {
                    MilRegister::VOID
                } else {
                    builder.allocate_reg()
                };
                let args = pop_args(&mut stack, &cpe.descriptor.param_types, true);

                emit_null_check(builder, args[0].clone());
                builder.append_end_instruction(MilEndInstructionKind::Call(
                    ret_class,
                    cpe.method_id,
                    reg,
                    args
                ));
                if ret_class != ClassId::PRIMITIVE_VOID {
                    stack.push(MilOperand::Register(MilType::for_class(ret_class), reg));
                };
            },
            BytecodeInstruction::InvokeVirtual(idx) | BytecodeInstruction::InvokeInterface(idx, _) => {
                let cpe = match cp[idx as usize] {
                    ConstantPoolEntry::Methodref(ref cpe) => cpe,
                    ConstantPoolEntry::InterfaceMethodref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                let (class, method) = env.get_method(cpe.method_id);

                let ret_class = method.return_type;
                let reg = if ret_class == ClassId::PRIMITIVE_VOID {
                    MilRegister::VOID
                } else {
                    builder.allocate_reg()
                };
                let args = pop_args(&mut stack, &cpe.descriptor.param_types, true);

                emit_null_check(builder, args[0].clone());

                let vtable = builder.allocate_reg();
                builder.append_instruction(MilInstructionKind::UnOp(MilUnOp::GetVTable, vtable, args[0].clone()));

                if class.flags.contains(ClassFlags::INTERFACE) {
                    builder.append_end_instruction(MilEndInstructionKind::CallInterface(
                        ret_class,
                        cpe.method_id,
                        reg,
                        MilOperand::Register(MilType::Addr, vtable),
                        args
                    ));
                } else {
                    builder.append_end_instruction(MilEndInstructionKind::CallVirtual(
                        ret_class,
                        cpe.method_id,
                        reg,
                        MilOperand::Register(MilType::Addr, vtable),
                        args
                    ));
                };

                if ret_class != ClassId::PRIMITIVE_VOID {
                    stack.push(MilOperand::Register(MilType::for_class(ret_class), reg));
                };
            },
            BytecodeInstruction::GetField(idx) => {
                let cpe = match cp[idx as usize] {
                    ConstantPoolEntry::Fieldref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                let ty = get_mil_type_for_descriptor(&cpe.descriptor);
                let obj = stack.pop(MilType::Ref);
                let reg = builder.allocate_reg();

                emit_null_check(builder, obj.clone());
                builder.append_instruction(MilInstructionKind::GetField(cpe.field_id, cpe.type_id, reg, obj));

                stack.push(MilOperand::Register(ty, reg));
            },
            BytecodeInstruction::PutField(idx) => {
                let cpe = match cp[idx as usize] {
                    ConstantPoolEntry::Fieldref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                let ty = get_mil_type_for_descriptor(&cpe.descriptor);
                let val = stack.pop(ty);
                let obj = stack.pop(MilType::Ref);

                emit_null_check(builder, obj.clone());
                builder.append_instruction(MilInstructionKind::PutField(cpe.field_id, cpe.type_id, obj, val));
            },
            BytecodeInstruction::GetStatic(idx) => {
                let cpe = match cp[idx as usize] {
                    ConstantPoolEntry::Fieldref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                let ty = get_mil_type_for_descriptor(&cpe.descriptor);
                let reg = builder.allocate_reg();
                builder.append_instruction(MilInstructionKind::GetStatic(cpe.field_id, cpe.type_id, reg));
                stack.push(MilOperand::Register(ty, reg));
            },
            BytecodeInstruction::PutStatic(idx) => {
                let cpe = match cp[idx as usize] {
                    ConstantPoolEntry::Fieldref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                let ty = get_mil_type_for_descriptor(&cpe.descriptor);
                let val = stack.pop(ty);
                builder.append_instruction(MilInstructionKind::PutStatic(cpe.field_id, cpe.type_id, val));
            },
            BytecodeInstruction::LCmp => {
                generate_bin_op(builder, &mut stack, MilBinOp::LCmp, MilType::Long, MilType::Long, MilType::Int);
            },
            BytecodeInstruction::If(cond, target) => {
                let val = stack.pop(MilType::Int);
                let cond_reg = builder.allocate_reg();

                builder.append_instruction(MilInstructionKind::BinOp(
                    MilBinOp::ICmp(MilIntComparison::from_bytecode(cond)),
                    cond_reg,
                    val,
                    MilOperand::Int(0)
                ));

                let cond_block = builder.append_end_instruction(MilEndInstructionKind::JumpIf(
                    MilBlockId::ENTRY,
                    MilBlockId::ENTRY,
                    MilOperand::Register(MilType::Bool, cond_reg)
                ));

                fixups.push(Box::new(move |builder, blocks| {
                    builder.func.blocks.get_mut(&cond_block).unwrap().end_instr.kind = MilEndInstructionKind::JumpIf(
                        blocks[&target].blocks[0],
                        blocks[&fallthrough_bc].blocks[0],
                        MilOperand::Register(MilType::Bool, cond_reg)
                    );
                }));
                end_block = Some(cond_block);
            },
            BytecodeInstruction::IfICmp(cond, target) => {
                let rhs = stack.pop(MilType::Int);
                let lhs = stack.pop(MilType::Int);
                let cond_reg = builder.allocate_reg();

                builder.append_instruction(MilInstructionKind::BinOp(
                    MilBinOp::ICmp(MilIntComparison::from_bytecode(cond)),
                    cond_reg,
                    lhs,
                    rhs
                ));

                let cond_block = builder.append_end_instruction(MilEndInstructionKind::JumpIf(
                    MilBlockId::ENTRY,
                    MilBlockId::ENTRY,
                    MilOperand::Register(MilType::Bool, cond_reg)
                ));

                fixups.push(Box::new(move |builder, blocks| {
                    builder.func.blocks.get_mut(&cond_block).unwrap().end_instr.kind = MilEndInstructionKind::JumpIf(
                        blocks[&target].blocks[0],
                        blocks[&fallthrough_bc].blocks[0],
                        MilOperand::Register(MilType::Bool, cond_reg)
                    );
                }));
                end_block = Some(cond_block);
            },
            BytecodeInstruction::IfACmp(cond, target) => {
                let rhs = stack.pop(MilType::Ref);
                let lhs = stack.pop(MilType::Ref);
                let cond_reg = builder.allocate_reg();

                builder.append_instruction(MilInstructionKind::BinOp(
                    MilBinOp::RCmp(MilRefComparison::from_bytecode(cond)),
                    cond_reg,
                    rhs,
                    lhs
                ));

                let cond_block = builder.append_end_instruction(MilEndInstructionKind::JumpIf(
                    MilBlockId::ENTRY,
                    MilBlockId::ENTRY,
                    MilOperand::Register(MilType::Bool, cond_reg)
                ));

                fixups.push(Box::new(move |builder, blocks| {
                    builder.func.blocks.get_mut(&cond_block).unwrap().end_instr.kind = MilEndInstructionKind::JumpIf(
                        blocks[&target].blocks[0],
                        blocks[&fallthrough_bc].blocks[0],
                        MilOperand::Register(MilType::Bool, cond_reg)
                    );
                }));
                end_block = Some(cond_block);
            },
            BytecodeInstruction::IfNonNull(target) => {
                let cond_reg = builder.allocate_reg();

                builder.append_instruction(MilInstructionKind::BinOp(
                    MilBinOp::RCmp(MilRefComparison::Ne),
                    cond_reg,
                    stack.pop(MilType::Ref),
                    MilOperand::RefNull
                ));

                let cond_block = builder.append_end_instruction(MilEndInstructionKind::JumpIf(
                    MilBlockId::ENTRY,
                    MilBlockId::ENTRY,
                    MilOperand::Register(MilType::Bool, cond_reg)
                ));

                fixups.push(Box::new(move |builder, blocks| {
                    builder.func.blocks.get_mut(&cond_block).unwrap().end_instr.kind = MilEndInstructionKind::JumpIf(
                        blocks[&target].blocks[0],
                        blocks[&fallthrough_bc].blocks[0],
                        MilOperand::Register(MilType::Bool, cond_reg)
                    );
                }));
                end_block = Some(cond_block);
            },
            BytecodeInstruction::IfNull(target) => {
                let cond_reg = builder.allocate_reg();

                builder.append_instruction(MilInstructionKind::BinOp(
                    MilBinOp::RCmp(MilRefComparison::Eq),
                    cond_reg,
                    stack.pop(MilType::Ref),
                    MilOperand::RefNull
                ));

                let cond_block = builder.append_end_instruction(MilEndInstructionKind::JumpIf(
                    MilBlockId::ENTRY,
                    MilBlockId::ENTRY,
                    MilOperand::Register(MilType::Bool, cond_reg)
                ));

                fixups.push(Box::new(move |builder, blocks| {
                    builder.func.blocks.get_mut(&cond_block).unwrap().end_instr.kind = MilEndInstructionKind::JumpIf(
                        blocks[&target].blocks[0],
                        blocks[&fallthrough_bc].blocks[0],
                        MilOperand::Register(MilType::Bool, cond_reg)
                    );
                }));
                end_block = Some(cond_block);
            },
            BytecodeInstruction::Goto(target) => {
                let block = builder.append_end_instruction(MilEndInstructionKind::Jump(MilBlockId::ENTRY));

                fixups.push(Box::new(move |builder, blocks| {
                    builder.func.blocks.get_mut(&block).unwrap().end_instr.kind = MilEndInstructionKind::Jump(
                        blocks[&target].blocks[0]
                    );
                }));
                end_block = Some(block);
            },
            BytecodeInstruction::CheckCast(idx) => {
                let cpe = match cp[idx as usize] {
                    ConstantPoolEntry::Class(ref cpe) => cpe,
                    _ => unreachable!()
                };

                let obj = stack.pop(MilType::Ref);

                let is_null_reg = builder.allocate_reg();
                builder.append_instruction(MilInstructionKind::BinOp(
                    MilBinOp::RCmp(MilRefComparison::Eq),
                    is_null_reg,
                    obj.clone(),
                    MilOperand::RefNull
                ));

                let null_check_block = builder.end_block();

                let vtable_reg = builder.allocate_reg();
                let is_subclass_reg = builder.allocate_reg();
                builder.append_instruction(MilInstructionKind::UnOp(MilUnOp::GetVTable, vtable_reg, obj.clone()));

                if !env.get(cpe.class_id).is_interface() {
                    builder.append_instruction(MilInstructionKind::IsSubclass(
                        cpe.class_id,
                        is_subclass_reg,
                        MilOperand::Register(MilType::Addr, vtable_reg)
                    ));
                } else {
                    // TODO
                    builder.append_instruction(MilInstructionKind::Copy(is_subclass_reg, MilOperand::Bool(true)));
                };

                let class_check_block = builder.end_block();
                let throw_block = builder.append_end_instruction(MilEndInstructionKind::Throw(MilOperand::RefNull));

                let continue_block = builder.end_block();
                builder.insert_end_instruction(
                    null_check_block,
                    MilEndInstructionKind::JumpIf(continue_block, class_check_block, MilOperand::Register(MilType::Bool, is_null_reg))
                );
                builder.insert_end_instruction(
                    class_check_block,
                    MilEndInstructionKind::JumpIf(continue_block, throw_block, MilOperand::Register(MilType::Bool, is_subclass_reg))
                );

                stack.push(obj);
            },
            BytecodeInstruction::InstanceOf(idx) => {
                let cpe = match cp[idx as usize] {
                    ConstantPoolEntry::Class(ref cpe) => cpe,
                    _ => unreachable!()
                };

                let obj = stack.pop(MilType::Ref);

                let is_null_reg = builder.allocate_reg();
                builder.append_instruction(MilInstructionKind::BinOp(
                    MilBinOp::RCmp(MilRefComparison::Eq),
                    is_null_reg,
                    obj.clone(),
                    MilOperand::RefNull
                ));

                let null_check_block = builder.end_block();

                let vtable_reg = builder.allocate_reg();
                let is_subclass_reg = builder.allocate_reg();
                builder.append_instruction(MilInstructionKind::UnOp(MilUnOp::GetVTable, vtable_reg, obj));

                if !env.get(cpe.class_id).is_interface() {
                    builder.append_instruction(MilInstructionKind::IsSubclass(
                        cpe.class_id,
                        is_subclass_reg,
                        MilOperand::Register(MilType::Addr, vtable_reg)
                    ));
                } else {
                    // TODO
                    builder.append_instruction(MilInstructionKind::Copy(is_subclass_reg, MilOperand::Bool(true)));
                };

                let is_subclass_block = builder.end_block();
                let is_subclass_phi = builder.append_phi_node(vec![
                    (MilOperand::Bool(false), null_check_block),
                    (MilOperand::Register(MilType::Bool, is_subclass_reg), is_subclass_block)
                ]);

                let result_reg = builder.allocate_reg();
                builder.append_instruction(MilInstructionKind::Select(
                    result_reg,
                    is_subclass_phi,
                    MilOperand::Int(1),
                    MilOperand::Int(0)
                ));

                let merge_block = builder.end_block();
                builder.insert_end_instruction(
                    null_check_block,
                    MilEndInstructionKind::JumpIf(merge_block, is_subclass_block, MilOperand::Register(MilType::Bool, is_null_reg))
                );

                stack.push(MilOperand::Register(MilType::Int, result_reg));
            },
            BytecodeInstruction::AThrow => {
                let val = stack.pop(MilType::Ref);
                end_block = Some(builder.append_end_instruction(MilEndInstructionKind::Throw(val)));
            },
            BytecodeInstruction::AReturn | BytecodeInstruction::DReturn | BytecodeInstruction::FReturn | BytecodeInstruction::IReturn | BytecodeInstruction::LReturn => {
                let val = stack.pop_any();
                end_block = Some(builder.append_end_instruction(MilEndInstructionKind::Return(Some(val))));
            },
            BytecodeInstruction::Return => {
                end_block = Some(builder.append_end_instruction(MilEndInstructionKind::Return(None)));
            },
            BytecodeInstruction::FCmpG => {
                generate_bin_op(builder, &mut stack, MilBinOp::FCmp(MilFCmpMode::G), MilType::Float, MilType::Float, MilType::Int);
            },
            BytecodeInstruction::FCmpL => {
                generate_bin_op(builder, &mut stack, MilBinOp::FCmp(MilFCmpMode::L), MilType::Float, MilType::Float, MilType::Int);
            },
            BytecodeInstruction::DCmpG => {
                generate_bin_op(builder, &mut stack, MilBinOp::DCmp(MilFCmpMode::G), MilType::Double, MilType::Double, MilType::Int);
            },
            BytecodeInstruction::DCmpL => {
                generate_bin_op(builder, &mut stack, MilBinOp::DCmp(MilFCmpMode::L), MilType::Double, MilType::Double, MilType::Int);
            },
            BytecodeInstruction::LookupSwitch(default_bc, ref table) => {
                let value = stack.pop(MilType::Int);
                let table = table.clone();

                let switch_block = builder.append_end_instruction(MilEndInstructionKind::ISwitch(
                    value.clone(),
                    vec![],
                    MilBlockId::EXIT
                ));

                fixups.push(Box::new(move |builder, blocks| {
                    builder.func.blocks.get_mut(&switch_block).unwrap().end_instr.kind = MilEndInstructionKind::ISwitch(
                        value.clone(),
                        table.iter().copied().map(|(val, bc)| (val, blocks[&bc].blocks[0])).collect_vec(),
                        blocks[&default_bc].blocks[0]
                    );
                }));

                end_block = Some(switch_block);
            },
            BytecodeInstruction::TableSwitch(low_value, default_bc, ref table) => {
                let value = stack.pop(MilType::Int);
                let table = table.clone();

                let switch_block = builder.append_end_instruction(MilEndInstructionKind::ISwitch(
                    value.clone(),
                    vec![],
                    MilBlockId::EXIT
                ));

                fixups.push(Box::new(move |builder, blocks| {
                    builder.func.blocks.get_mut(&switch_block).unwrap().end_instr.kind = MilEndInstructionKind::ISwitch(
                        value.clone(),
                        table.iter().copied().enumerate().map(|(i, bc)| (low_value.wrapping_add(i as i32), blocks[&bc].blocks[0])).collect_vec(),
                        blocks[&default_bc].blocks[0]
                    );
                }));

                end_block = Some(switch_block);
            },
            BytecodeInstruction::InvokeDynamic(idx) => {
                eprintln!("{}: UNIMPLEMENTED {:?}", MethodName(builder.func.id, env), instr);
                let cpe = match cp[idx as usize] {
                    ConstantPoolEntry::InvokeDynamic(ref cpe) => cpe,
                    _ => unreachable!()
                };

                pop_args(&mut stack, &cpe.descriptor.param_types, false);

                if let Some(ref return_type) = cpe.descriptor.return_type {
                    let ty = match return_type.flat {
                        _ if return_type.array_dims > 0 => MilType::Ref,
                        FlatTypeDescriptor::Reference(_) => MilType::Ref,
                        FlatTypeDescriptor::Primitive(ty) => MilType::for_class(ClassId::for_primitive_type(ty))
                    };
                    let reg = builder.allocate_reg();

                    stack.push(MilOperand::Register(ty, reg));
                };

                builder.append_end_instruction(MilEndInstructionKind::Throw(MilOperand::RefNull));
            },
            instr => {
                panic!("Unsupported bytecode {:?} in ilgen of method {}", instr, MethodName(builder.func.id, env));
            }
        };
    };

    let end_block = if let Some(end_block) = end_block {
        end_block
    } else {
        builder.end_block()
    };

    if verbose {
        eprintln!("(End of bytecode basic block) {}", stack.pretty(env));
    };

    let block_info = blocks.get_mut(&off).unwrap();
    block_info.blocks = builder.end_ordered_blocks();
    block_info.end_stack = stack.as_slice().iter().cloned().collect_vec();

    for succ_bc in blocks.get(&off).unwrap().succs.iter().cloned() {
        let succ = blocks.get(&succ_bc).unwrap();

        if let Some(succ_start_block) = succ.blocks.first() {
            let start_block = builder.func.blocks.get_mut(succ_start_block).unwrap();

            assert_eq!(stack.non_empty_slots().count(), start_block.phi_nodes.len());

            for (stack_elem, phi) in stack.non_empty_slots().zip(start_block.phi_nodes.iter_mut()) {
                assert_eq!(stack_elem.get_type(), phi.sources[0].0.get_type());
                phi.sources.push((stack_elem.clone(), end_block));
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
            MilEndInstructionKind::CallInterface(_, method_id, _, _, _) => {
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

fn is_in_range(bc: u32, range: (u32, u32)) -> bool {
    bc >= range.0 && bc < range.1
}

fn build_local_debug_map(code: &AttributeCode, table: &[LocalVariableTableEntry], locals: &MilLocals) -> Result<MilLocalDebugMap, ()> {
    let mut scope_stack = vec![MilLocalDebugScope::new(0, code.code.len() as u32)];

    for entry in table.iter().filter(|e| e.class_id != ClassId::UNRESOLVED) {
        let local = if let Some(local) = locals.get(entry.slot, MilType::for_class(entry.class_id)) {
            local
        } else {
            continue;
        };

        while !is_in_range(entry.start_pc as u32, scope_stack.last().unwrap().range()) {
            let scope = scope_stack.pop().unwrap();
            scope_stack.last_mut().unwrap().sub_scopes.push(scope);
        };

        let range = (entry.start_pc as u32, entry.start_pc as u32 + entry.len as u32);

        let scope = scope_stack.last_mut().unwrap();
        let scope = if range.1 > scope.range().1 {
            return Err(());
        } else if range == scope.range() {
            scope
        } else {
            scope_stack.push(MilLocalDebugScope::new(range.0, range.1));
            scope_stack.last_mut().unwrap()
        };

        scope.locals.push(MilLocalDebugMapEntry {
            name: Arc::clone(&entry.name),
            ty: entry.class_id,
            local
        });
    };

    Ok(MilLocalDebugMap::new(scope_stack.into_iter().rev().fold1(|sub_scope, mut scope| {
        scope.sub_scopes.push(sub_scope);
        scope
    }).unwrap()))
}

pub fn generate_il_for_code(
    env: &ClassEnvironment,
    method_id: MethodId,
    class: &Class,
    method: &Method,
    code: &AttributeCode,
    known_objects: &MilKnownObjectRefs,
    liveness: &LivenessInfo,
    verbose: bool
) -> MilFunction {
    if verbose {
        eprintln!("===== MIL Generation for {}.{}{} =====\n", class.meta.name, method.name, method.descriptor);
    };

    let instrs = BytecodeIterator::for_code(code);

    let mut locals = MilLocals::new(code.max_locals);
    let mut fixups = vec![];

    let mut blocks = scan_blocks(instrs);
    let mut block_worklist = vec![0];
    let mut builder = MilBuilder::new(method_id, verbose, env);

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

    func.source_file = if let Some(ref source_file) = class.meta.source_file {
        (
            String::from(class.meta.name.rsplit_once('/').map_or("", |(dir, _)| dir)),
            String::from(&source_file[..])
        )
    } else {
        (String::from(""), String::from("<unknown>"))
    };

    func.line_map = MilLineMap::from_table(code.line_table().cloned());
    func.local_map = if let Some(table) = code.local_variable_table() {
        match build_local_debug_map(code, table, &locals) {
            Ok(local_map) => Some(local_map),
            Err(_) => {
                eprintln!("WARNING: Local variable map on {}.{}{} discarded due to not being well-nested", class.meta.name, method.name, method.descriptor);
                None
            }
        }
    } else {
        None
    };

    if verbose {
        eprintln!("\n\n{}", func.pretty(env));
    };

    func
}

pub fn generate_il_for_method(env: &ClassEnvironment, method_id: MethodId, known_objects: &MilKnownObjectRefs, liveness: &LivenessInfo, verbose: bool) -> Option<MilFunction> {
    let (class, method) = env.get_method(method_id);

    match method.body {
        Some(MethodBody::Code(ref code)) => Some(generate_il_for_code(env, method_id, class, method, code, known_objects, liveness, verbose)),
        Some(MethodBody::NativeThunk(ref name)) => Some(generate_native_thunk(String::from(name), method, method_id, known_objects, env)),
        None => None
    }
}
