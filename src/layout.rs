use std::collections::HashMap;

use crate::classfile::{Class, FieldFlags, PrimitiveType};
use crate::liveness::LivenessInfo;
use crate::resolve::{ClassEnvironment, ClassId, FieldId, MethodId, ResolvedClass};

#[derive(Debug, Clone)]
pub struct ObjectLayout {
    pub fields: Vec<(FieldId, u32)>,
    pub size: u32,
    pub virtual_slots: Vec<MethodId>,
    pub interface_slots: Vec<ClassId>,
    pub static_fields: Vec<(FieldId, u32)>,
    pub static_size: u32
}

impl ObjectLayout {
    pub fn empty() -> ObjectLayout {
        ObjectLayout {
            fields: vec![],
            size: 8 /* Skip the vtable pointer and lock word */,
            virtual_slots: vec![],
            interface_slots: vec![],
            static_fields: vec![],
            static_size: 24 /* Size of java/lang/Class objects */
        }
    }

    fn align_size(&mut self, align: u32) {
        if self.size % align != 0 {
            self.size += align - (self.size % align);
        };
    }

    fn alloc_field(&mut self, size: u32, align: u32) -> u32 {
        self.align_size(align);

        let offset = self.size;
        self.size += size;
        offset
    }

    fn align_static_size(&mut self, align: u32) {
        if self.static_size % align != 0 {
            self.static_size += align - (self.size % align);
        };
    }

    fn alloc_static_field(&mut self, size: u32, align: u32) -> u32 {
        self.align_static_size(align);

        let offset = self.static_size;
        self.static_size += size;
        offset
    }
}

fn get_field_size_align(env: &ClassEnvironment, class_id: ClassId) -> (u32, u32) {
    if class_id == ClassId::UNRESOLVED {
        return (0, 1);
    };

    match **env.get(class_id) {
        ResolvedClass::User(_) => (8, 8),
        ResolvedClass::Primitive(PrimitiveType::Byte) => (1, 1),
        ResolvedClass::Primitive(PrimitiveType::Char) => (2, 2),
        ResolvedClass::Primitive(PrimitiveType::Double) => (8, 8),
        ResolvedClass::Primitive(PrimitiveType::Float) => (4, 4),
        ResolvedClass::Primitive(PrimitiveType::Int) => (4, 4),
        ResolvedClass::Primitive(PrimitiveType::Long) => (8, 8),
        ResolvedClass::Primitive(PrimitiveType::Short) => (2, 2),
        ResolvedClass::Primitive(PrimitiveType::Boolean) => (1, 1),
        ResolvedClass::Array(_, _) => (8, 8)
    }
}

fn compute_layout(env: &ClassEnvironment, liveness: &LivenessInfo, class_id: ClassId, layouts: &mut HashMap<ClassId, ObjectLayout>, verbose: bool) {
    if class_id == ClassId::UNRESOLVED || !liveness.needs_clinit.contains(&class_id) || layouts.contains_key(&class_id) {
        return;
    };

    let mut layout = ObjectLayout::empty();
    if let ResolvedClass::User(ref class) = **env.get(class_id) {
        let may_construct = liveness.may_construct.contains(&class_id);

        compute_layout(env, liveness, class.meta.super_id, layouts, verbose);
        for interface_id in class.meta.interface_ids.iter().cloned() {
            compute_layout(env, liveness, interface_id, layouts, verbose);
        };

        if verbose {
            eprintln!("Computing layout for {}...", class.meta.name);
        };

        if may_construct {
            if let Some(super_layout) = layouts.get(&class.meta.super_id) {
                layout.fields = super_layout.fields.clone();
                layout.size = super_layout.size;
                layout.virtual_slots = super_layout.virtual_slots.clone();
                layout.interface_slots = super_layout.interface_slots.clone();
            };

            for interface_id in class.meta.interface_ids.iter().cloned() {
                if !layout.interface_slots.contains(&interface_id) && !layouts.get(&interface_id).unwrap().virtual_slots.is_empty() {
                    if verbose {
                        eprintln!("    Interface {} is allocated at islot {}", env.get(interface_id).name(env), layout.interface_slots.len());
                    };

                    layout.interface_slots.push(interface_id);
                };
            };
        };

        for (i, f) in class.fields.iter().enumerate() {
            let (fsize, falign) = get_field_size_align(env, f.class_id);
            if f.flags.contains(FieldFlags::STATIC) {
                let off = layout.alloc_static_field(fsize, falign);

                if verbose {
                    eprintln!("    Static field {} {} is allocated at offset {}", f.name, f.descriptor, off);
                };

                layout.static_fields.push((FieldId(class_id, i as u16), off));
            } else if may_construct {
                let off = layout.alloc_field(fsize, falign);

                if verbose {
                    eprintln!("    Field {} {} is allocated at offset {}", f.name, f.descriptor, off);
                };

                layout.fields.push((FieldId(class_id, i as u16), off));
            };
        };

        if may_construct {
            for (i, m) in class.methods.iter().enumerate() {
                let method_id = MethodId(class_id, i as u16);
                if liveness.may_virtual_call.contains(&method_id) {
                    let old_slot = if m.overrides.overrides_virtual != MethodId::UNRESOLVED {
                        layout.virtual_slots.iter().enumerate()
                            .filter(|(_, mid)| mid == &&m.overrides.overrides_virtual)
                            .next().map(|(i, _)| i)
                    } else if !m.overrides.overridden_by.is_empty() {
                        None
                    } else {
                        continue;
                    };

                    if let Some(old_slot) = old_slot {
                        if verbose {
                            eprintln!("    Virtual method {}{} is allocated at vslot {} (overriding super)", m.name, m.descriptor, old_slot);
                        };

                        layout.virtual_slots[old_slot] = method_id;
                    } else {
                        if verbose {
                            eprintln!("    Virtual method {}{} is allocated at vslot {}", m.name, m.descriptor, layout.virtual_slots.len());
                        };

                        layout.virtual_slots.push(method_id);
                    };
                };
            };
        };
    };

    layouts.insert(class_id, layout);
}

fn commit_layout(class: &mut Class, layout: ObjectLayout) {
    for (FieldId(field_class, i), off) in layout.fields.iter().cloned() {
        if field_class == class.meta.this_id {
            class.fields[i as usize].off = off;
        };
    };

    for (FieldId(_, i), off) in layout.static_fields.iter().cloned() {
        class.fields[i as usize].off = off;
    };

    class.layout = layout;
}

pub fn compute_all_layouts(env: &mut ClassEnvironment, liveness: &LivenessInfo, verbose: bool) {
    let mut layouts = HashMap::new();

    for class_id in env.class_ids() {
        compute_layout(env, liveness, class_id, &mut layouts, verbose);
    };

    for class_id in env.class_ids() {
        if let ResolvedClass::User(ref mut class) = **env.get_mut(class_id) {
            if let Some(layout) = layouts.remove(&class_id) {
                commit_layout(class, layout);
            };
        };
    };
}