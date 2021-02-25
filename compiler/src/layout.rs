use std::collections::HashMap;
use std::sync::Arc;

use crate::classfile::{Class, FieldFlags, PrimitiveType};
use crate::liveness::LivenessInfo;
use crate::resolve::{ClassEnvironment, ClassId, FieldId, MethodId, MethodName, ResolvedClass};

pub const JAVA_OBJECT_HEADER_SIZE: u32 = 8;
pub const JAVA_ARRAY_HEADER_SIZE: u32 = 12;
pub const JAVA_LANG_CLASS_SIZE: u32 = 40;

#[derive(Debug, Clone)]
pub struct ObjectLayout {
    pub fields: Vec<(FieldId, u32)>,
    pub size: u32,
    pub virtual_slots: Vec<MethodId>,
    pub interface_slots: Vec<(ClassId, Arc<Vec<MethodId>>)>,
    pub static_fields: Vec<(FieldId, u32)>,
    pub static_size: u32
}

impl ObjectLayout {
    pub fn empty() -> ObjectLayout {
        ObjectLayout {
            fields: vec![],
            size: JAVA_OBJECT_HEADER_SIZE,
            virtual_slots: vec![],
            interface_slots: vec![],
            static_fields: vec![],
            static_size: JAVA_LANG_CLASS_SIZE
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
            self.static_size += align - (self.static_size % align);
        };
    }

    fn alloc_static_field(&mut self, size: u32, align: u32) -> u32 {
        self.align_static_size(align);

        let offset = self.static_size;
        self.static_size += size;
        offset
    }
}

pub fn get_field_size_align(env: &ClassEnvironment, class_id: ClassId) -> (u32, u32) {
    if class_id == ClassId::UNRESOLVED {
        return (0, 1);
    };

    match **env.get(class_id) {
        ResolvedClass::User(_) => (8, 8),
        ResolvedClass::Primitive(None) => panic!("Cannot have a field of type void"),
        ResolvedClass::Primitive(Some(PrimitiveType::Byte)) => (1, 1),
        ResolvedClass::Primitive(Some(PrimitiveType::Char)) => (2, 2),
        ResolvedClass::Primitive(Some(PrimitiveType::Double)) => (8, 8),
        ResolvedClass::Primitive(Some(PrimitiveType::Float)) => (4, 4),
        ResolvedClass::Primitive(Some(PrimitiveType::Int)) => (4, 4),
        ResolvedClass::Primitive(Some(PrimitiveType::Long)) => (8, 8),
        ResolvedClass::Primitive(Some(PrimitiveType::Short)) => (2, 2),
        ResolvedClass::Primitive(Some(PrimitiveType::Boolean)) => (1, 1),
        ResolvedClass::Array(_) => (8, 8)
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

            for interface_id in class.meta.all_interface_ids.iter().cloned() {
                if !layout.interface_slots.iter().any(|&(id, _)| id == interface_id) && !layouts.get(&interface_id).unwrap().virtual_slots.is_empty() {
                    if verbose {
                        eprintln!("    Interface {} is allocated at islot {}", env.get(interface_id).name(env), layout.interface_slots.len());
                    };

                    layout.interface_slots.push((interface_id, Arc::new(layouts[&interface_id].virtual_slots.clone())));
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
                    } else if !m.overrides.overridden_by.is_empty() || !m.overrides.overrides_interface.is_empty() {
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

                    for interface_method in m.overrides.overrides_interface.iter().cloned() {
                        let islot = layout.interface_slots.iter().enumerate()
                            .filter(|&(_, &(interface_id, _))| interface_id == interface_method.0)
                            .next().map(|(i, _)| i);

                        if let Some(islot) = islot {
                            let vslot = layouts[&interface_method.0].virtual_slots.iter().enumerate()
                                .filter(|&(_, &mid)| mid == interface_method)
                                .next().map(|(i, _)| i);

                            if let Some(vslot) = vslot {
                                if verbose {
                                    eprintln!("        Overriding interface method at islot ({}, {})", islot, vslot);
                                };

                                Arc::make_mut(&mut layout.interface_slots[islot].1)[vslot] = method_id;
                            };
                        };
                    };
                };
            };

            for (interface_method, super_method) in class.meta.extra_interface_overrides.iter().copied() {
                let islot = layout.interface_slots.iter().enumerate()
                    .filter(|&(_, &(interface_id, _))| interface_id == interface_method.0)
                    .next().map(|(i, _)| i);

                if let Some(islot) = islot {
                    let vslot = layouts[&interface_method.0].virtual_slots.iter().enumerate()
                        .filter(|&(_, &mid)| mid == interface_method)
                        .next().map(|(i, _)| i);

                    if let Some(vslot) = vslot {
                        if verbose {
                            eprintln!("    Superclass method {} overriding interface method at islot ({}, {})", MethodName(super_method, env), islot, vslot);
                        };

                        Arc::make_mut(&mut layout.interface_slots[islot].1)[vslot] = super_method;
                    };
                };
            };
        };
    };

    if class_id == ClassId::JAVA_LANG_CLASS {
        assert_eq!(layout.size, JAVA_LANG_CLASS_SIZE);
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

    for (slot, MethodId(method_class, i)) in layout.virtual_slots.iter().cloned().enumerate() {
        if method_class == class.meta.this_id {
            class.methods[i as usize].virtual_slot = slot as u32;
        };
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

pub fn get_array_header_size(elem_align: u32) -> u32 {
    (JAVA_ARRAY_HEADER_SIZE + elem_align - 1) / elem_align * elem_align
}
