use crate::*;

pub struct Builder {
    program: Program,

    at_fn: Option<FuncId>,
    at_bb: Option<BlockId>,

    next_var: VarId,
    next_ptr: PtrId,
}

impl Builder {
    pub fn new() -> Self {
        Self {
            program: Program {
                functions: vec![],
            },

            at_fn: None,
            at_bb: None,

            next_var: VarId(0),
            next_ptr: PtrId(0),
        }
    }

    pub fn create_function(&mut self) -> FuncId {
        let id = FuncId(self.program.functions.len());
        self.program.functions.push(Function { blocks: vec![] });
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
        let id = self.next_var;
        self.next_var.0 += 1;
        id
    }

    pub fn alloc_ptr(&mut self) -> PtrId {
        let id = self.next_ptr;
        self.next_ptr.0 += 1;
        id
    }

    pub fn push_inst(&mut self, inst: Instruction) {
        self.program.functions[self.at_fn.unwrap().0].blocks[self.at_bb.unwrap().0].insts.push(inst);
    }

    pub fn set_term(&mut self, term: Terminator) {
        self.program.functions[self.at_fn.unwrap().0].blocks[self.at_bb.unwrap().0].term = term;
    }

    pub fn done(mut self) -> Program {
        destruct(&mut self.program.functions[0]);
        self.program
    }
}
