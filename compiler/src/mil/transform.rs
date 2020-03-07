use super::flow_graph::*;
use super::il::*;

pub fn remove_incoming_phis(block: &mut MilBlock, pred: MilBlockId) {
    for phi in block.phi_nodes.iter_mut() {
        phi.sources.remove(
            phi.sources.iter().enumerate()
                .find(|(_, &(_, phi_pred))| phi_pred == pred)
                .map(|(i, _)| i)
                .unwrap()
        );
    };
}
