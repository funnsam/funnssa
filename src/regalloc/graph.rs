use super::*;
use std::collections::HashMap;

type InnerRange = core::ops::Range<usize>;

pub struct GraphAlloc<R: Register> {
    blocks: Vec<HashMap<VReg<R>, InnerRange>>,
    at_inst: usize,
}

impl<R: Register + 'static> RegAlloc<R> for GraphAlloc<R> {
    fn new_sized(_size: usize) -> Self {
        Self {
            blocks: Vec::new(),
            at_inst: 0,
        }
    }

    fn next_inst(&mut self) {
        self.at_inst += 1;
    }

    fn next_block(&mut self) {
        self.blocks.push(HashMap::new());
        self.at_inst = 0;
    }

    fn next_fn(&mut self) {
        self.blocks.clear();
    }

    fn define(&mut self, vr: VReg<R>) {
        let range = self.at_inst..self.at_inst;
        self.get_block().entry(vr).or_insert(range);
    }

    fn add_use(&mut self, vr: VReg<R>) {
    }

    fn coalesce_move(&mut self, from: VReg<R>, to: VReg<R>) {
    }

    fn alloc_regs(&mut self, alloc: &mut [VReg<R>]) {
    }
}

impl<R: Register> GraphAlloc<R> {
    fn get_block<'a>(&'a mut self) -> &'a mut HashMap<VReg<R>, InnerRange> {
        self.blocks.last_mut().unwrap()
    }
}
