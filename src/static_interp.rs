use crate::bytecode::{BytecodeInstruction, BytecodeIterator};
use crate::classfile::{ConstantPoolEntry, MethodSummary};
use crate::resolve::{ClassId, FieldId, MethodId};

fn add_may_virtual_call(summary: &mut MethodSummary, method_id: MethodId) {
    if method_id.0 != ClassId::UNRESOLVED && !summary.may_virtual_call.contains(&method_id) {
        summary.may_virtual_call.push(method_id);
    };
}

fn add_may_special_call(summary: &mut MethodSummary, method_id: MethodId) {
    if method_id.0 != ClassId::UNRESOLVED && !summary.may_special_call.contains(&method_id) {
        summary.may_special_call.push(method_id);
    };
}

fn add_may_construct(summary: &mut MethodSummary, class_id: ClassId) {
    if class_id != ClassId::UNRESOLVED && !summary.may_construct.contains(&class_id) {
        summary.may_construct.push(class_id);
    };
}

fn add_may_clinit(summary: &mut MethodSummary, class_id: ClassId) {
    if class_id != ClassId::UNRESOLVED && !summary.may_clinit.contains(&class_id) {
        summary.may_clinit.push(class_id);
    };
}

pub fn summarize_bytecode(instrs: BytecodeIterator, cp: &[ConstantPoolEntry]) -> MethodSummary {
    let mut summary = MethodSummary {
        may_virtual_call: vec![],
        may_special_call: vec![],
        may_construct: vec![],
        may_clinit: vec![]
    };

    for instr in instrs {
        match instr.unwrap() {
            BytecodeInstruction::ANewArray(_) => {
                // TODO
            },
            BytecodeInstruction::GetField(cpe) => {
                // TODO
            },
            BytecodeInstruction::GetStatic(cpe) => {
                let cpe = match cp[cpe as usize] {
                    ConstantPoolEntry::Fieldref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                add_may_clinit(&mut summary, cpe.field_id.0);
            },
            BytecodeInstruction::InvokeDynamic(_) => {
                // TODO
            },
            BytecodeInstruction::InvokeInterface(cpe, _) => {
                let cpe = match cp[cpe as usize] {
                    ConstantPoolEntry::InterfaceMethodref(ref cpe) => cpe,
                    ConstantPoolEntry::Methodref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                add_may_virtual_call(&mut summary, cpe.method_id);
            },
            BytecodeInstruction::InvokeSpecial(cpe) => {
                let cpe = match cp[cpe as usize] {
                    ConstantPoolEntry::InterfaceMethodref(ref cpe) => cpe,
                    ConstantPoolEntry::Methodref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                add_may_special_call(&mut summary, cpe.method_id);
            },
            BytecodeInstruction::InvokeStatic(cpe) => {
                let cpe = match cp[cpe as usize] {
                    ConstantPoolEntry::InterfaceMethodref(ref cpe) => cpe,
                    ConstantPoolEntry::Methodref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                add_may_clinit(&mut summary, cpe.method_id.0);
                add_may_special_call(&mut summary, cpe.method_id);
            },
            BytecodeInstruction::InvokeVirtual(cpe) => {
                let cpe = match cp[cpe as usize] {
                    ConstantPoolEntry::InterfaceMethodref(ref cpe) => cpe,
                    ConstantPoolEntry::Methodref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                add_may_virtual_call(&mut summary, cpe.method_id);
            },
            BytecodeInstruction::MultiANewArray(_, _) => {
                // TODO
            },
            BytecodeInstruction::New(cpe) => {
                let cpe = match cp[cpe as usize] {
                    ConstantPoolEntry::Class(ref cpe) => cpe,
                    _ => unreachable!()
                };

                add_may_clinit(&mut summary, cpe.class_id);
                add_may_construct(&mut summary, cpe.class_id);
            },
            BytecodeInstruction::NewArray(_) => {
                // TODO
            },
            BytecodeInstruction::PutField(cpe) => {
                // TODO
            },
            BytecodeInstruction::PutStatic(cpe) => {
                let cpe = match cp[cpe as usize] {
                    ConstantPoolEntry::Fieldref(ref cpe) => cpe,
                    _ => unreachable!()
                };

                add_may_clinit(&mut summary, cpe.field_id.0);
            },
            _ => {}
        };
    };

    summary
}
