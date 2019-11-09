use std::collections::HashSet;

use crate::classfile::Method;
use crate::resolve::{ClassEnvironment, ClassId, MethodId, ResolvedClass};

pub struct LivenessInfo {
    pub needs_clinit: HashSet<ClassId>,
    pub may_construct: HashSet<ClassId>,
    pub may_virtual_call: HashSet<MethodId>,
    pub may_call: HashSet<MethodId>
}

struct Indent(u32);

impl std::fmt::Display for Indent {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for _ in 0..self.0 {
            write!(f, " ")?;
        };
        Result::Ok(())
    }
}

fn ensure_clinit(env: &ClassEnvironment, liveness: &mut LivenessInfo, class_id: ClassId, indent: &mut Indent, verbose: bool) {
    if class_id == ClassId::UNRESOLVED || !liveness.needs_clinit.insert(class_id) {
        return;
    };

    let class = if let ResolvedClass::User(ref class) = **env.get(class_id) {
        class
    } else {
        return;
    };

    if verbose {
        eprintln!("{}Can clinit {}", indent, class.meta.name);
    };
    indent.0 += 1;

    if let Some(clinit_method) = class.meta.clinit_method {
        analyze_method(env, liveness, MethodId(class_id, clinit_method), false, indent, verbose);
    };

    indent.0 -= 1;
}

fn may_call_parent_virtual(env: &ClassEnvironment, liveness: &LivenessInfo, method: &Method) -> bool {
    itertools::repeat_n(method.overrides.overrides_virtual, 1).chain(
        method.overrides.overrides_interface.iter().cloned()
    ).any(|parent_id| liveness.may_virtual_call.contains(&parent_id))
}

fn ensure_constructed(env: &ClassEnvironment, liveness: &mut LivenessInfo, class_id: ClassId, indent: &mut Indent, verbose: bool) {
    ensure_clinit(env, liveness, class_id, indent, verbose);
    if class_id == ClassId::UNRESOLVED || !liveness.may_construct.insert(class_id) {
        return;
    };

    let class = if let ResolvedClass::User(ref class) = **env.get(class_id) {
        class
    } else {
        return;
    };

    if verbose {
        eprintln!("{}Can construct {}", indent, class.meta.name);
    };
    indent.0 += 1;

    ensure_constructed(env, liveness, class.meta.super_id, indent, verbose);
    for interface in class.meta.interface_ids.iter().cloned() {
        ensure_constructed(env, liveness, interface, indent, verbose);
    };

    for (i, m) in class.methods.iter().enumerate() {
        if may_call_parent_virtual(env, liveness, m) {
            analyze_method(env, liveness, MethodId(class_id, i as u16), true, indent, verbose);
        };
    };

    indent.0 -= 1;
}

fn analyze_method(env: &ClassEnvironment, liveness: &mut LivenessInfo, method_id: MethodId, virtual_call: bool, indent: &mut Indent, verbose: bool) {
    if method_id == MethodId::UNRESOLVED {
        return;
    };

    let class = env.get(method_id.0).as_user_class();
    let method = &class.methods[method_id.1 as usize];

    if liveness.may_call.insert(method_id) {
        if verbose {
            eprintln!("{}Can call method {}.{}{}", indent, class.meta.name, method.name, method.descriptor);
        };
        indent.0 += 1;

        for may_clinit in method.summary.may_clinit.iter().cloned() {
            ensure_clinit(env, liveness, may_clinit, indent, verbose);
        };

        for may_construct in method.summary.may_construct.iter().cloned() {
            ensure_constructed(env, liveness, may_construct, indent, verbose);
        };

        for may_virtual_call in method.summary.may_virtual_call.iter().cloned() {
            analyze_method(env, liveness, may_virtual_call, true, indent, verbose);
        };

        for may_special_call in method.summary.may_special_call.iter().cloned() {
            analyze_method(env, liveness, may_special_call, false, indent, verbose);
        };

        indent.0 -= 1;
    };

    if virtual_call && liveness.may_virtual_call.insert(method_id) {
        if verbose {
            eprintln!("{}Can vcall method {}.{}{}", indent, class.meta.name, method.name, method.descriptor);
        };
        indent.0 += 1;

        for overrider in method.overrides.overridden_by.iter().cloned() {
            if liveness.may_construct.contains(&overrider.0) {
                analyze_method(env, liveness, overrider, true, indent, verbose);
            };
        };

        indent.0 -= 1;
    };
}

pub fn analyze_all(env: &ClassEnvironment, main_method: MethodId, verbose: bool) -> LivenessInfo {
    let mut liveness = LivenessInfo {
        needs_clinit: HashSet::new(),
        may_construct: HashSet::new(),
        may_virtual_call: HashSet::new(),
        may_call: HashSet::new()
    };

    let mut indent = Indent(0);

    for class_id in ClassId::special_classes() {
        if class_id != ClassId::JAVA_LANG_INVOKE_METHODHANDLE {
            ensure_constructed(env, &mut liveness, class_id, &mut indent, verbose);
        };
    };

    ensure_clinit(env, &mut liveness, main_method.0, &mut indent, verbose);
    analyze_method(env, &mut liveness, main_method, false, &mut indent, verbose);

    liveness
}
