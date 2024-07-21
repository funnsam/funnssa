use crate::*;

pub struct Builder<'a> {
    program: Program<'a>,

    at_fn: Option<FuncId>,
    at_bb: Option<BlockId>,

}

impl<'a> Builder<'a> {
    pub fn new() -> Self {
        Self {
            program: Program {
                functions: vec![],
            },

            at_fn: None,
            at_bb: None,
        }
    }

    pub fn create_function(&mut self, name: &'a str) -> FuncId {
        let id = FuncId(self.program.functions.len());
        self.program.functions.push(Function {
            name,
            blocks: vec![],

            next_var: VarId(0),
            next_ptr: PtrId(0),
        });
        id
    }

    pub fn position_at_function(&mut self, f: FuncId) {
        self.at_fn = Some(f);
        self.at_bb = None;
    }

    pub fn position_at_bb(&mut self, b: BlockId) {
        self.at_bb = Some(b);
    }

    pub fn push_block(&mut self) -> BlockId {
        let f = &mut self.program.functions[self.at_fn.unwrap().0];
        let id = BlockId(f.blocks.len());
        f.blocks.push(BasicBlock {
            args: vec![],
            insts: vec![],
            term: Terminator::None,
        });
        id
    }

    pub fn alloc_var(&mut self) -> VarId {
        self.program.functions[self.at_fn.unwrap().0].alloc_var()
    }

    pub fn alloc_ptr(&mut self) -> PtrId {
        self.program.functions[self.at_fn.unwrap().0].alloc_ptr()
    }

    pub fn push_inst(&mut self, inst: Instruction) {
        self.program.functions[self.at_fn.unwrap().0].blocks[self.at_bb.unwrap().0].insts.push(inst);
    }

    pub fn set_term(&mut self, term: Terminator) {
        self.program.functions[self.at_fn.unwrap().0].blocks[self.at_bb.unwrap().0].term = term;
    }

    pub fn done(mut self) -> Program<'a> {
        destruct(&mut self.program.functions[0]);
        self.program
    }
}
