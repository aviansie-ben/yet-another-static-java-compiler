use std::collections::HashMap;

use itertools::Itertools;

use crate::mil::flow_graph::FlowGraph;
use crate::mil::il::*;
use crate::resolve::ClassEnvironment;

#[derive(Debug, Clone, Copy)]
enum ValidatorLocation {
    Phi(MilBlockId, usize),
    Instruction(MilBlockId, usize),
    EndInstruction(MilBlockId)
}

#[derive(Debug, Clone)]
enum ValidatorErrorKind {
    PhiEntriesMismatch(Vec<MilBlockId>),
    MultipleRegisterDefinitions(MilRegister, ValidatorLocation),
    FallsThroughLastBlock,
    TargetTypeMismatch(MilRegister, MilType, MilType),
    OperandTypeMismatch(MilOperand, MilType, MilType),
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
    loc: ValidatorLocation,
    dup_defs: HashMap<MilRegister, ValidatorLocation>,
    errors: Vec<ValidatorError>
}

fn validate_operand(op: &MilOperand, expected_ty: MilType, state: &mut ValidatorState) {
    // TODO Check that operand definitions reach uses

    let actual_ty = op.get_type(&state.func.reg_map);
    if actual_ty != expected_ty {
        state.errors.push(ValidatorError {
            kind: ValidatorErrorKind::OperandTypeMismatch(op.clone(), expected_ty, actual_ty),
            loc: state.loc
        });
    };
}

fn validate_target(tgt: MilRegister, expected_ty: MilType, state: &mut ValidatorState) {
    if tgt != MilRegister::VOID {
        if let Some(old_loc) = state.dup_defs.insert(tgt, state.loc) {
            state.errors.push(ValidatorError {
                kind: ValidatorErrorKind::MultipleRegisterDefinitions(tgt, old_loc),
                loc: state.loc
            });
        };
    };

    let actual_ty = state.func.reg_map.get_reg_info(tgt).ty;
    if actual_ty != expected_ty {
        state.errors.push(ValidatorError {
            kind: ValidatorErrorKind::TargetTypeMismatch(tgt, expected_ty, actual_ty),
            loc: state.loc
        });
    };
}

fn validate_bytecode(bytecode: (u32, u32), state: &mut ValidatorState) {
    if bytecode.0 != !0 && bytecode.0 as usize >= state.func.inline_sites.len() {
        state.errors.push(ValidatorError {
            kind: ValidatorErrorKind::InlineSiteNotFound(bytecode.0),
            loc: state.loc
        });
    };
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
            eprint!("Register {} already defined at ", reg);

            match orig_loc {
                ValidatorLocation::Phi(block_id, i) => {
                    eprintln!("{}.phi_nodes[{}]", block_id, i);
                },
                ValidatorLocation::Instruction(block_id, i) => {
                    eprintln!("{}.instrs[{}]", block_id, i);
                },
                ValidatorLocation::EndInstruction(block_id) => {
                    eprintln!("{}.end_instr", block_id);
                }
            }
        },
        ValidatorErrorKind::FallsThroughLastBlock => {
            eprintln!("Control falls through last block");
        },
        ValidatorErrorKind::TargetTypeMismatch(tgt, expected_ty, actual_ty) => {
            eprintln!("Expected target {} to be type {}, was {}", tgt, expected_ty, actual_ty);
        },
        ValidatorErrorKind::OperandTypeMismatch(ref op, expected_ty, actual_ty) => {
            eprintln!("Expected operand {} to be type {}, was {}", op.pretty(env), expected_ty, actual_ty);
        },
        ValidatorErrorKind::BlockNotFound(block_id) => {
            eprintln!("Block {} was not found", block_id);
        },
        ValidatorErrorKind::InlineSiteNotFound(site_id) => {
            eprintln!("Inline site {} was not found", site_id);
        }
    };
}

fn validate_function_internal(func: &MilFunction, _env: &ClassEnvironment) -> Vec<ValidatorError> {
    let cfg = FlowGraph::for_function(func);
    let mut state = ValidatorState {
        func,
        loc: ValidatorLocation::EndInstruction(MilBlockId::ENTRY),
        dup_defs: HashMap::new(),
        errors: vec![]
    };

    for block_id in func.block_order.iter().copied() {
        let block = &func.blocks[&block_id];
        let preds = cfg.get(block_id).incoming.iter().copied().sorted_by_key(|id| id.0).dedup().collect_vec();

        for (i, phi) in block.phi_nodes.iter().enumerate() {
            let phi_preds = phi.sources.iter().map(|&(_, pred)| pred).sorted_by_key(|id| id.0).dedup().collect_vec();

            state.loc = ValidatorLocation::Phi(block_id, i);

            if phi_preds != preds {
                state.errors.push(ValidatorError {
                    kind: ValidatorErrorKind::PhiEntriesMismatch(preds.clone()),
                    loc: state.loc
                });
            };

            let ty = func.reg_map.get_reg_info(phi.target).ty;

            for &(ref op, _) in phi.sources.iter() {
                if op != &MilOperand::Register(MilRegister::VOID) {
                    validate_operand(op, ty, &mut state);
                };
            };

            validate_target(phi.target, ty, &mut state);
            validate_bytecode(phi.bytecode, &mut state);
        };

        for (i, instr) in block.instrs.iter().enumerate() {
            state.loc = ValidatorLocation::Instruction(block_id, i);

            match instr.kind {
                MilInstructionKind::Nop => {},
                MilInstructionKind::Copy(tgt, ref val) => {
                    let ty = func.reg_map.get_reg_info(tgt).ty;

                    validate_operand(val, ty, &mut state);
                    validate_target(tgt, ty, &mut state);
                },
                MilInstructionKind::Select(tgt, ref cond, ref true_val, ref false_val) => {
                    let ty = func.reg_map.get_reg_info(tgt).ty;

                    validate_operand(cond, MilType::Bool, &mut state);
                    validate_operand(true_val, ty, &mut state);
                    validate_operand(false_val, ty, &mut state);
                    validate_target(tgt, ty, &mut state);
                },
                MilInstructionKind::UnOp(op, tgt, ref val) => {
                    let (tgt_ty, val_ty) = op.type_sig();

                    validate_operand(val, val_ty, &mut state);
                    validate_target(tgt, tgt_ty, &mut state);
                },
                MilInstructionKind::BinOp(op, tgt, ref lhs, ref rhs) => {
                    let (tgt_ty, lhs_ty, rhs_ty) = op.type_sig();

                    validate_operand(lhs, lhs_ty, &mut state);
                    validate_operand(rhs, rhs_ty, &mut state);
                    validate_target(tgt, tgt_ty, &mut state);
                },
                MilInstructionKind::GetParam(_, _, tgt) => {
                    // TODO Validate param type
                    validate_target(tgt, func.reg_map.get_reg_info(tgt).ty, &mut state);
                },
                MilInstructionKind::GetLocal(_, tgt) => {
                    // TODO Validate local type
                    validate_target(tgt, func.reg_map.get_reg_info(tgt).ty, &mut state);
                },
                MilInstructionKind::SetLocal(_, ref val) => {
                    // TODO Validate local type
                    validate_operand(val, val.get_type(&func.reg_map), &mut state);
                },
                MilInstructionKind::GetField(_, _, tgt, ref obj) => {
                    // TODO Validate field type
                    validate_operand(obj, MilType::Ref, &mut state);
                    validate_target(tgt, func.reg_map.get_reg_info(tgt).ty, &mut state);
                },
                MilInstructionKind::PutField(_, _, ref obj, ref val) => {
                    // TODO Validate field type
                    validate_operand(obj, MilType::Ref, &mut state);
                    validate_operand(val, val.get_type(&func.reg_map), &mut state);
                },
                MilInstructionKind::GetArrayLength(tgt, ref obj) => {
                    validate_operand(obj, MilType::Ref, &mut state);
                    validate_target(tgt, MilType::Int, &mut state);
                },
                MilInstructionKind::GetArrayElement(_, tgt, ref obj, ref idx) => {
                    // TODO Validate element type
                    validate_operand(obj, MilType::Ref, &mut state);
                    validate_operand(idx, MilType::Int, &mut state);
                    validate_target(tgt, func.reg_map.get_reg_info(tgt).ty, &mut state);
                },
                MilInstructionKind::PutArrayElement(_, ref obj, ref idx, ref val) => {
                    // TODO Validate element type
                    validate_operand(obj, MilType::Ref, &mut state);
                    validate_operand(idx, MilType::Int, &mut state);
                    validate_operand(val, val.get_type(&func.reg_map), &mut state);
                },
                MilInstructionKind::GetStatic(_, _, tgt) => {
                    // TODO Validate field type
                    validate_target(tgt, func.reg_map.get_reg_info(tgt).ty, &mut state);
                },
                MilInstructionKind::PutStatic(_, _, ref val) => {
                    // TODO Validate field type
                    validate_operand(val, val.get_type(&func.reg_map), &mut state);
                },
                MilInstructionKind::AllocObj(_, tgt) => {
                    validate_target(tgt, MilType::Ref, &mut state);
                },
                MilInstructionKind::AllocArray(_, tgt, ref len) => {
                    validate_operand(len, MilType::Int, &mut state);
                    validate_target(tgt, MilType::Ref, &mut state);
                },
                MilInstructionKind::GetVTable(tgt, ref obj) => {
                    validate_operand(obj, MilType::Ref, &mut state);
                    validate_target(tgt, MilType::Addr, &mut state);
                }
            };

            validate_bytecode(instr.bytecode, &mut state);
        };

        state.loc = ValidatorLocation::EndInstruction(block_id);

        match block.end_instr.kind {
            MilEndInstructionKind::Nop => {},
            MilEndInstructionKind::Unreachable => {},
            MilEndInstructionKind::Call(_, _, tgt, ref args) => {
                // TODO Typecheck calls
                for arg in args.iter() {
                    validate_operand(arg, arg.get_type(&func.reg_map), &mut state);
                };
                validate_target(tgt, func.reg_map.get_reg_info(tgt).ty, &mut state);
            },
            MilEndInstructionKind::CallVirtual(_, _, tgt, ref vtable, ref args) => {
                // TODO Typecheck calls
                validate_operand(vtable, MilType::Addr, &mut state);
                for arg in args.iter() {
                    validate_operand(arg, arg.get_type(&func.reg_map), &mut state);
                };
                validate_target(tgt, func.reg_map.get_reg_info(tgt).ty, &mut state);
            },
            MilEndInstructionKind::CallInterface(_, _, tgt, ref vtable, ref args) => {
                // TODO Typecheck calls
                validate_operand(vtable, MilType::Addr, &mut state);
                for arg in args.iter() {
                    validate_operand(arg, arg.get_type(&func.reg_map), &mut state);
                };
                validate_target(tgt, func.reg_map.get_reg_info(tgt).ty, &mut state);
            },
            MilEndInstructionKind::CallNative(_, _, tgt, ref args) => {
                // TODO Typecheck calls
                for arg in args.iter() {
                    validate_operand(arg, arg.get_type(&func.reg_map), &mut state);
                };
                validate_target(tgt, func.reg_map.get_reg_info(tgt).ty, &mut state);
            },
            MilEndInstructionKind::Throw(ref val) => {
                validate_operand(val, MilType::Ref, &mut state);
            },
            MilEndInstructionKind::Return(ref val) => {
                // TODO Typecheck returns
                validate_operand(val, val.get_type(&func.reg_map), &mut state);
            },
            MilEndInstructionKind::Jump(tgt) => {
                if !func.blocks.contains_key(&tgt) {
                    state.errors.push(ValidatorError {
                        kind: ValidatorErrorKind::BlockNotFound(tgt),
                        loc: state.loc
                    });
                };
            },
            MilEndInstructionKind::JumpIf(tgt, ref cond) => {
                if !func.blocks.contains_key(&tgt) {
                    state.errors.push(ValidatorError {
                        kind: ValidatorErrorKind::BlockNotFound(tgt),
                        loc: state.loc
                    });
                };

                validate_operand(cond, MilType::Bool, &mut state);
            }
        };

        validate_bytecode(block.end_instr.bytecode, &mut state);
    };

    if func.blocks[func.block_order.last().unwrap()].end_instr.can_fall_through() {
        state.errors.push(ValidatorError {
            kind: ValidatorErrorKind::FallsThroughLastBlock,
            loc: ValidatorLocation::EndInstruction(*func.block_order.last().unwrap())
        });
    };

    state.errors
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
