use std::collections::HashMap;
use std::fmt;

use itertools::Itertools;

use crate::mil::dom::Dominators;
use crate::mil::flow_graph::FlowGraph;
use crate::mil::il::*;
use crate::resolve::ClassEnvironment;

#[derive(Debug, Clone, Copy)]
enum ValidatorLocation {
    Phi(MilBlockId, usize),
    Instruction(MilBlockId, usize),
    EndInstruction(MilBlockId)
}

impl ValidatorLocation {
    fn block(self) -> MilBlockId {
        match self {
            ValidatorLocation::Phi(block, _) => block,
            ValidatorLocation::Instruction(block, _) => block,
            ValidatorLocation::EndInstruction(block) => block
        }
    }
}

impl fmt::Display for ValidatorLocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ValidatorLocation::Phi(block_id, i) => {
                write!(f, "{}.phi_nodes[{}]", block_id, i)
            },
            ValidatorLocation::Instruction(block_id, i) => {
                write!(f, "{}.instrs[{}]", block_id, i)
            },
            ValidatorLocation::EndInstruction(block_id) => {
                write!(f, "{}.end_instr", block_id)
            }
        }
    }
}

#[derive(Debug, Clone)]
enum ValidatorErrorKind {
    PhiEntriesMismatch(Vec<MilBlockId>),
    MultipleRegisterDefinitions(MilRegister, ValidatorLocation),
    ExpectedVoidTarget,
    ExpectedNonVoidTarget,
    OperandUndefinedRegister(MilOperand),
    OperandWrongRegisterType(MilOperand, MilType, Option<ValidatorLocation>),
    OperandRegisterDoesNotDominate(MilOperand, ValidatorLocation),
    FallsThroughLastBlock,
    OperandTypeMismatch(MilOperand, MilType),
    BlockNotFound(MilBlockId),
    InlineSiteNotFound(u32)
}

#[derive(Debug, Clone)]
struct ValidatorError {
    kind: ValidatorErrorKind,
    loc: ValidatorLocation
}

struct ValidatorState<'a> {
    func: &'a MilFunction,
    doms: Dominators,
    regs_seen: Vec<MilRegister>,
    loc: ValidatorLocation,
    reachable: bool,
    regs: HashMap<MilRegister, (ValidatorLocation, MilType)>,
    errors: Vec<ValidatorError>
}

impl ValidatorState<'_> {
    fn push_error(&mut self, err: ValidatorErrorKind) {
        self.errors.push(ValidatorError {
            kind: err,
            loc: self.loc
        });
    }
}

fn validate_operand(op: &MilOperand, expected_ty: MilType, phi_block: Option<MilBlockId>, state: &mut ValidatorState) {
    if op.get_type() != expected_ty {
        state.push_error(ValidatorErrorKind::OperandTypeMismatch(op.clone(), expected_ty));
    };

    let def = match *op {
        MilOperand::Register(ty, reg) => {
            if let Some(&(def_loc, def_ty)) = state.regs.get(&reg) {
                if def_ty != ty {
                    state.push_error(ValidatorErrorKind::OperandWrongRegisterType(op.clone(), def_ty, Some(def_loc)));
                };
                Some((reg, def_loc))
            } else {
                if state.reachable {
                    state.push_error(ValidatorErrorKind::OperandUndefinedRegister(op.clone()));
                };
                None
            }
        },
        _ => None
    };

    if let Some((reg, def_loc)) = def {
        let doms_correctly = if phi_block.is_none() && def_loc.block() == state.loc.block() {
            state.regs_seen.contains(&reg)
        } else {
            state.doms.get(phi_block.unwrap_or(state.loc.block())).get(def_loc.block())
        };

        if !doms_correctly {
            state.push_error(ValidatorErrorKind::OperandRegisterDoesNotDominate(op.clone(), def_loc));
        };
    };
}

fn mark_target(tgt: MilRegister, _: Option<MilType>, state: &mut ValidatorState) {
    if tgt != MilRegister::VOID {
        state.regs_seen.push(tgt);
    };
}

fn validate_target(tgt: MilRegister, expected_ty: Option<MilType>, state: &mut ValidatorState) {
    if let Some(expected_ty) = expected_ty {
        if tgt == MilRegister::VOID {
            state.push_error(ValidatorErrorKind::ExpectedNonVoidTarget);
        } else if let Some((old_loc, _)) = state.regs.insert(tgt, (state.loc, expected_ty)) {
            state.push_error(ValidatorErrorKind::MultipleRegisterDefinitions(tgt, old_loc));
        };
    } else if tgt != MilRegister::VOID {
        state.push_error(ValidatorErrorKind::ExpectedVoidTarget);
    };
}

fn validate_bytecode(bytecode: (u32, u32), state: &mut ValidatorState) {
    if bytecode.0 != !0 && bytecode.0 as usize >= state.func.inline_sites.len() {
        state.push_error(ValidatorErrorKind::InlineSiteNotFound(bytecode.0));
    };
}

fn validate_block_target(tgt: MilBlockId, state: &mut ValidatorState) {
    if !state.func.blocks.contains_key(&tgt) {
        state.push_error(ValidatorErrorKind::BlockNotFound(tgt));
    };
}

fn validate_instr(
    mut validate_target: impl FnMut (MilRegister, Option<MilType>, &mut ValidatorState),
    mut validate_operand: impl FnMut (&MilOperand, MilType, &mut ValidatorState),
    instr: &MilInstruction,
    state: &mut ValidatorState
) {
    match instr.kind {
        MilInstructionKind::Nop => {},
        MilInstructionKind::Copy(tgt, ref val) => {
            validate_operand(val, val.get_type(), state);
            validate_target(tgt, Some(val.get_type()), state);
        },
        MilInstructionKind::Select(tgt, ref cond, ref true_val, ref false_val) => {
            let ty = true_val.get_type();

            validate_operand(cond, MilType::Bool, state);
            validate_operand(true_val, ty, state);
            validate_operand(false_val, ty, state);
            validate_target(tgt, Some(ty), state);
        },
        MilInstructionKind::UnOp(op, tgt, ref val) => {
            let (tgt_ty, val_ty) = op.type_sig();

            validate_operand(val, val_ty, state);
            validate_target(tgt, Some(tgt_ty), state);
        },
        MilInstructionKind::BinOp(op, tgt, ref lhs, ref rhs) => {
            let (tgt_ty, lhs_ty, rhs_ty) = op.type_sig();

            validate_operand(lhs, lhs_ty, state);
            validate_operand(rhs, rhs_ty, state);
            validate_target(tgt, Some(tgt_ty), state);
        },
        MilInstructionKind::GetParam(_, constraint, tgt) => {
            validate_target(tgt, Some(MilType::for_class(constraint.class_id())), state);
        },
        MilInstructionKind::GetLocal(local_id, tgt) => {
            validate_target(tgt, Some(state.func.local_info[&local_id].ty), state);
        },
        MilInstructionKind::SetLocal(local_id, ref val) => {
            validate_operand(val, state.func.local_info[&local_id].ty, state);
        },
        MilInstructionKind::GetField(_, constraint, tgt, ref obj) => {
            validate_operand(obj, MilType::Ref, state);
            validate_target(tgt, Some(MilType::for_class(constraint)), state);
        },
        MilInstructionKind::PutField(_, constraint, ref obj, ref val) => {
            validate_operand(obj, MilType::Ref, state);
            validate_operand(val, MilType::for_class(constraint), state);
        },
        MilInstructionKind::GetArrayElement(constraint, tgt, ref obj, ref idx) => {
            validate_operand(obj, MilType::Ref, state);
            validate_operand(idx, MilType::Int, state);
            validate_target(tgt, Some(MilType::for_class(constraint)), state);
        },
        MilInstructionKind::PutArrayElement(constraint, ref obj, ref idx, ref val) => {
            validate_operand(obj, MilType::Ref, state);
            validate_operand(idx, MilType::Int, state);
            validate_operand(val, MilType::for_class(constraint), state);
        },
        MilInstructionKind::GetStatic(_, constraint, tgt) => {
            validate_target(tgt, Some(MilType::for_class(constraint)), state);
        },
        MilInstructionKind::PutStatic(_, constraint, ref val) => {
            validate_operand(val, MilType::for_class(constraint), state);
        },
        MilInstructionKind::AllocObj(_, tgt) => {
            validate_target(tgt, Some(MilType::Ref), state);
        },
        MilInstructionKind::AllocArray(_, tgt, ref len) => {
            validate_operand(len, MilType::Int, state);
            validate_target(tgt, Some(MilType::Ref), state);
        }
    };
}

fn validate_end_instr(
    mut validate_target: impl FnMut (MilRegister, Option<MilType>, &mut ValidatorState),
    mut validate_operand: impl FnMut (&MilOperand, MilType, &mut ValidatorState),
    mut validate_block_target: impl FnMut(MilBlockId, &mut ValidatorState),
    end_instr: &MilEndInstruction,
    state: &mut ValidatorState
) {
    match end_instr.kind {
        MilEndInstructionKind::Nop => {},
        MilEndInstructionKind::Unreachable => {},
        MilEndInstructionKind::Call(constraint, _, tgt, ref args) => {
            // TODO Typecheck calls
            for arg in args.iter() {
                validate_operand(arg, arg.get_type(), state);
            };
            validate_target(tgt, MilType::for_class_return(constraint), state);
        },
        MilEndInstructionKind::CallVirtual(constraint, _, tgt, ref vtable, ref args) => {
            // TODO Typecheck calls
            validate_operand(vtable, MilType::Addr, state);
            for arg in args.iter() {
                validate_operand(arg, arg.get_type(), state);
            };
            validate_target(tgt, MilType::for_class_return(constraint), state);
        },
        MilEndInstructionKind::CallInterface(constraint, _, tgt, ref vtable, ref args) => {
            // TODO Typecheck calls
            validate_operand(vtable, MilType::Addr, state);
            for arg in args.iter() {
                validate_operand(arg, arg.get_type(), state);
            };
            validate_target(tgt, MilType::for_class_return(constraint), state);
        },
        MilEndInstructionKind::CallNative(constraint, _, tgt, ref args) => {
            // TODO Typecheck calls
            for arg in args.iter() {
                validate_operand(arg, arg.get_type(), state);
            };
            validate_target(tgt, MilType::for_class_return(constraint), state);
        },
        MilEndInstructionKind::Throw(ref val) => {
            validate_operand(val, MilType::Ref, state);
        },
        MilEndInstructionKind::Return(ref val) => {
            // TODO Typecheck returns
            if let Some(ref val) = val {
                validate_operand(val, val.get_type(), state);
            };
        },
        MilEndInstructionKind::Jump(tgt) => {
            validate_block_target(tgt, state);
        },
        MilEndInstructionKind::JumpIf(true_tgt, false_tgt, ref cond) => {
            validate_block_target(true_tgt, state);
            validate_block_target(false_tgt, state);
            validate_operand(cond, MilType::Bool, state);
        },
        MilEndInstructionKind::ISwitch(ref val, ref tgts, default_tgt) => {
            for (_, tgt) in tgts.iter().copied() {
                validate_block_target(tgt, state);
            };
            validate_block_target(default_tgt, state);
            validate_operand(val, MilType::Int, state);
        }
    };
}

fn validate_function_internal(func: &MilFunction, _env: &ClassEnvironment) -> Vec<ValidatorError> {
    let cfg = FlowGraph::for_function(func);
    let reachable = cfg.compute_reachability();
    let mut state = ValidatorState {
        func,
        doms: Dominators::calculate_dominators(func, &cfg),
        regs_seen: vec![],
        loc: ValidatorLocation::EndInstruction(MilBlockId::ENTRY),
        reachable: true,
        regs: HashMap::new(),
        errors: vec![]
    };

    for block_id in func.block_order.iter().copied() {
        let block = &func.blocks[&block_id];
        let preds = cfg.get(block_id).incoming.iter().copied().sorted_by_key(|id| id.0).dedup().collect_vec();

        for (i, phi) in block.phi_nodes.iter().enumerate() {
            let phi_preds = phi.sources.iter().map(|&(_, pred)| pred).sorted_by_key(|id| id.0).collect_vec();

            state.loc = ValidatorLocation::Phi(block_id, i);

            if phi_preds != preds {
                state.push_error(ValidatorErrorKind::PhiEntriesMismatch(preds.clone()));
            };

            validate_target(phi.target, Some(phi.ty), &mut state);
            validate_bytecode(phi.bytecode, &mut state);
        };

        for (i, instr) in block.instrs.iter().enumerate() {
            state.loc = ValidatorLocation::Instruction(block_id, i);
            validate_instr(validate_target, |_, _, _| {}, instr, &mut state);
            validate_bytecode(instr.bytecode, &mut state);
        };

        state.loc = ValidatorLocation::EndInstruction(block_id);
        validate_end_instr(validate_target, |_, _, _| {}, validate_block_target, &block.end_instr, &mut state);
        validate_bytecode(block.end_instr.bytecode, &mut state);
    };

    let last_block_id = func.block_order.last().copied().unwrap();
    if func.blocks[&last_block_id].end_instr.can_fall_through() && reachable.get(last_block_id) {
        state.push_error(ValidatorErrorKind::FallsThroughLastBlock);
    };

    for block_id in func.block_order.iter().copied() {
        let block = &func.blocks[&block_id];

        state.regs_seen.clear();

        for (i, phi) in block.phi_nodes.iter().enumerate() {
            state.loc = ValidatorLocation::Phi(block_id, i);

            for &(ref op, pred) in phi.sources.iter() {
                state.reachable = reachable.get(pred);
                validate_operand(op, phi.ty, Some(pred), &mut state);
            };
        };

        for phi in block.phi_nodes.iter() {
            state.regs_seen.push(phi.target);
        };

        state.reachable = reachable.get(block_id);

        for (i, instr) in block.instrs.iter().enumerate() {
            state.loc = ValidatorLocation::Instruction(block_id, i);
            validate_instr(mark_target, |op, expected_ty, state| validate_operand(op, expected_ty, None, state), instr, &mut state);
        };

        state.loc = ValidatorLocation::EndInstruction(block_id);
        validate_end_instr(|_, _, _| {}, |op, expected_ty, state| validate_operand(op, expected_ty, None, state), |_, _| {}, &block.end_instr, &mut state);
    };

    state.errors
}

fn print_validation_error(func: &MilFunction, env: &ClassEnvironment, err: &ValidatorError) {
    match err.loc {
        ValidatorLocation::Phi(block_id, i) => {
            let phi = &func.blocks[&block_id].phi_nodes[i];
            eprint!("{}\n  At {}.phi_nodes[{}]: ", phi.pretty(env), block_id, i);
        },
        ValidatorLocation::Instruction(block_id, i) => {
            let instr = &func.blocks[&block_id].instrs[i];
            eprint!("{}\n  At {}.instrs[{}]: ", instr.pretty(env), block_id, i);
        },
        ValidatorLocation::EndInstruction(block_id) => {
            let end_instr = &func.blocks[&block_id].end_instr;
            eprint!("{}\n  At {}.end_instr: ", end_instr.pretty(env), block_id);
        }
    };

    match err.kind {
        ValidatorErrorKind::PhiEntriesMismatch(ref expected_preds) => {
            eprint!("Phi predecessor mismatch, expected [ ");

            for pred in expected_preds.iter().copied() {
                eprint!("{} ", pred);
            };

            eprintln!("]");
        },
        ValidatorErrorKind::MultipleRegisterDefinitions(reg, orig_loc) => {
            eprintln!("Register {} already defined at {}", reg, orig_loc);
        },
        ValidatorErrorKind::ExpectedVoidTarget => {
            eprintln!("Instruction expected a void target");
        },
        ValidatorErrorKind::ExpectedNonVoidTarget => {
            eprintln!("Instruction expected a non-void target");
        },
        ValidatorErrorKind::OperandUndefinedRegister(ref op) => {
            eprintln!("Operand {} refers to a register that is never defined", op.pretty(env));
        },
        ValidatorErrorKind::OperandWrongRegisterType(ref op, defined_ty, defined_loc) => {
            eprint!("Operand {} mismatches defined type {} of register", op.pretty(env), defined_ty);

            if let Some(defined_loc) = defined_loc {
                eprintln!(" (defined at {})", defined_loc);
            } else {
                eprintln!();
            };
        },
        ValidatorErrorKind::OperandRegisterDoesNotDominate(ref op, defined_loc) => {
            eprintln!("Operand {}'s use does not dominate its definition at {}", op.pretty(env), defined_loc);
        },
        ValidatorErrorKind::FallsThroughLastBlock => {
            eprintln!("Control falls through last block");
        },
        ValidatorErrorKind::OperandTypeMismatch(ref op, expected_ty) => {
            eprintln!("Expected operand {} to be type {}, was {}", op.pretty(env), expected_ty, op.get_type());
        },
        ValidatorErrorKind::BlockNotFound(block_id) => {
            eprintln!("Block {} was not found", block_id);
        },
        ValidatorErrorKind::InlineSiteNotFound(site_id) => {
            eprintln!("Inline site {} was not found", site_id);
        }
    };
}

pub fn validate_function(func: &MilFunction, env: &ClassEnvironment) {
    let errors = validate_function_internal(func, env);

    if !errors.is_empty() {
        eprintln!("The MIL validator has detected errors in {}:\n\n{}\n", MethodName(func.id, env), func.pretty(env));
        for err in errors {
            print_validation_error(func, env, &err);
        };

        panic!("Encountered errors while validating MIL for {}", MethodName(func.id, env));
    };
}
