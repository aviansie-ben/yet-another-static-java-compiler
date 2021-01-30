use itertools::Itertools;

use crate::log_writeln;
use crate::log::Log;
use crate::mil::flow_graph::FlowGraph;
use crate::mil::il::*;
use crate::resolve::ClassEnvironment;
use crate::static_heap::JavaStaticHeap;

pub mod basic_control_flow;
pub mod class_constraints;
pub mod value_prop;

pub struct OptimizationEnvironment<'a, 'b, 'c> {
    pub env: &'a ClassEnvironment,
    pub heap: &'a JavaStaticHeap<'b>,
    pub known_objects: &'a MilKnownObjectMap<'b>,
    pub log: &'a Log<'c>
}

fn run_block_cleanup_group(func: &mut MilFunction, cfg: &mut FlowGraph<MilBlockId>, env: &OptimizationEnvironment) {
    basic_control_flow::eliminate_dead_blocks(func, cfg, env.env, env.log);
    basic_control_flow::simplify_phis(func, env.env, env.log);
    basic_control_flow::merge_blocks(func, cfg, env.env, env.log);
    while basic_control_flow::remove_redundant_jumps(func, cfg, env.env, env.log) != 0 {
        basic_control_flow::merge_blocks(func, cfg, env.env, env.log);
    };
}

fn optimize_function(func: &mut MilFunction, env: &OptimizationEnvironment) {
    let mut cfg = FlowGraph::for_function(func);

    log_writeln!(env.log, "\n===== OPTIMIZING {} =====\n\n{}\n{:#?}\n", MethodName(func.id, env.env), func.pretty(env.env), cfg);

    // Start by cleaning up known messiness left by the IL generator
    value_prop::fold_constant_exprs(func, env.env, env.known_objects, env.log);
    basic_control_flow::fold_constant_jumps(func, &mut cfg, env.env, env.log);
    value_prop::eliminate_dead_stores(func, env.env, env.log);
    run_block_cleanup_group(func, &mut cfg, env);

    // Now turn local slots into phi nodes and perform some basic cleanup to deal with the results
    if !func.reg_map.local_info.is_empty() {
        value_prop::transform_locals_into_phis(func, &cfg, env.env, env.log);
        basic_control_flow::simplify_phis(func, env.env, env.log);
        value_prop::fold_constant_exprs(func, env.env, env.known_objects, env.log);
        basic_control_flow::fold_constant_jumps(func, &mut cfg, env.env, env.log);
        value_prop::eliminate_dead_stores(func, env.env, env.log);
        run_block_cleanup_group(func, &mut cfg, env);
    };

    // Perform class constraint analysis and related cleanups
    class_constraints::perform_class_constraint_analysis(func, &cfg, env.env, env.log);
    basic_control_flow::fold_constant_jumps(func, &mut cfg, env.env, env.log);
    value_prop::eliminate_dead_stores(func, env.env, env.log);
    run_block_cleanup_group(func, &mut cfg, env);

    // Get things ready for inlining wherever possible
    basic_control_flow::devirtualize_nonoverriden_calls(func, env.env, env.log);
}

pub fn optimize_program(program: &mut MilProgram, env: &ClassEnvironment, heap: &JavaStaticHeap, log: &Log) {
    for (_, func) in program.funcs.iter_mut().sorted_by_key(|&(&id, _)| ((id.0).0, id.1)) {
        optimize_function(func, &OptimizationEnvironment { env, heap, known_objects: &program.known_objects, log })
    };
}
