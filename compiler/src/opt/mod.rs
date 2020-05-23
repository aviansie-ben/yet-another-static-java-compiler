use itertools::Itertools;

use crate::mil::flow_graph::FlowGraph;
use crate::mil::il::*;
use crate::resolve::ClassEnvironment;
use crate::static_heap::JavaStaticHeap;

pub mod basic_control_flow;
pub mod value_prop;

pub struct OptimizationEnvironment<'a, 'b> {
    env: &'a ClassEnvironment,
    heap: &'a JavaStaticHeap<'b>,
    known_objects: &'a MilKnownObjectMap<'b>
}

fn optimize_function(func: &mut MilFunction, env: &OptimizationEnvironment) {
    let mut cfg = FlowGraph::for_function(func);

    eprintln!("===== OPTIMIZING {} =====\n\n{}\n{:#?}\n", MethodName(func.id, env.env), func.pretty(env.env), cfg);

    // Start by cleaning up known mesiness left by the IL generator
    value_prop::fold_constant_exprs(func, env.env, env.known_objects);
    basic_control_flow::simplify_phis(func, &mut cfg, env.env);
    basic_control_flow::merge_blocks(func, &mut cfg, env.env);
    basic_control_flow::eliminate_dead_blocks(func, &mut cfg, env.env);

    // Now turn local slots into phi nodes and perform some basic cleanup to deal with the results
    value_prop::transform_locals_into_phis(func, &cfg, env.env);
    basic_control_flow::simplify_phis(func, &mut cfg, env.env);
    value_prop::fold_constant_exprs(func, env.env, env.known_objects);
    basic_control_flow::fold_constant_jumps(func, &mut cfg, env.env);
}

pub fn optimize_program(program: &mut MilProgram, env: &ClassEnvironment, heap: &JavaStaticHeap) {
    for (_, func) in program.funcs.iter_mut().sorted_by_key(|&(&id, _)| ((id.0).0, id.1)) {
        optimize_function(func, &OptimizationEnvironment { env, heap, known_objects: &program.known_objects })
    };
}
