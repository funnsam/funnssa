use super::*;
use std::collections::HashMap;

type InnerRange = core::ops::Range<usize>;

pub struct GraphAlloc<R: Register> {
    first_def: HashMap<VReg<R>, (Option<BlockId>, usize)>,
    last_uses: HashMap<VReg<R>, HashMap<Option<BlockId>, usize>>,

    at_block: Option<BlockId>,
    at_inst: usize,
}

impl<R: Register + 'static + core::fmt::Debug> RegAlloc<R> for GraphAlloc<R> {
    fn new_sized(_size: usize) -> Self {
        Self {
            first_def: HashMap::new(),
            last_uses: HashMap::new(),

            at_block: None,
            at_inst: 0,
        }
    }

    fn next_inst(&mut self) {
        self.at_inst += 1;
    }

    fn next_block(&mut self) {
        if let Some(ab) = &mut self.at_block {
            ab.0 += 1;
        }

        self.at_inst = 0;
    }

    fn next_fn(&mut self) {
        self.first_def.clear();
        self.last_uses.clear();

        self.at_block = None;
        self.at_inst = 0;
    }

    fn prologue_end(&mut self) {
        self.at_block = Some(BlockId(0));
        self.at_inst = 0;
    }

    fn define(&mut self, vr: VReg<R>) {
        self.first_def.insert(vr, (self.at_block, self.at_inst));
    }

    fn add_use(&mut self, vr: VReg<R>) {
        self.last_uses.entry(vr)
            .or_default()
            .insert(self.at_block, self.at_inst);
    }

    fn coalesce_move(&mut self, from: VReg<R>, to: VReg<R>) {
    }

    fn alloc_regs(&mut self, alloc: &mut [VReg<R>], cfg: cfg::Cfg) {
        println!("{:?}", self.first_def);
        println!("{:?}", self.last_uses);
    }
}
