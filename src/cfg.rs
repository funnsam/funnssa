use crate::*;

pub struct Cfg<'a> {
    bbs: &'a [BasicBlock],
    ipreds: Vec<HashSet<BlockId>>,
}

impl<'a> Cfg<'a> {
    pub(crate) fn new(func: &'a Function<'a>) -> Self {

        Self {
            bbs: &func.blocks,
            ipreds: func.pred(),
        }
    }

    pub fn bb_imm_succs(&self, bb_id: BlockId) -> Vec<BlockId> {
        match &self.bbs[bb_id.0].term {
            Terminator::UncondBranch(t) => vec![t.target],
            Terminator::CondBranch(_, a, b) => vec![a.target, b.target],
            Terminator::Return(_) | Terminator::None => vec![],
        }
    }

    pub fn bb_imm_preds(&'a self, bb_id: BlockId) -> &'a HashSet<BlockId> {
        &self.ipreds[bb_id.0]
    }
}
