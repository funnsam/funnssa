use crate::{*, types::*, value::*};

pub struct Builder<'a> {
    program: Program<'a>,

    at_fn: Option<FuncId>,
    at_bb: Option<BlockId>,

}

impl<'a> Default for Builder<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Builder<'a> {
    pub const fn new() -> Self {
        Self {
            program: Program {
                functions: vec![],
            },

            at_fn: None,
            at_bb: None,
        }
    }

    pub fn create_function(
        &mut self,
        linkage: Linkage,
        name: &'a str,
        at: Vec<ValueType>,
        rt: Option<ValueType>,
    ) -> (FuncId, Vec<Value>) {
        let id = FuncId(self.program.functions.len());
        let args = at.iter().copied().enumerate().map(|(id, typ)| Value {
            typ,
            id: ValueId(id),
        }).collect::<Vec<_>>();
        self.program.functions.push(Function {
            linkage,
            name,
            blocks: vec![],
            arguments: at,
            returns: rt,

            val_alloc: ValAlloc(ValueId(args.len())),
        });
        (id, args)
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

    fn alloc_val_id(&mut self) -> ValueId {
        self.program.functions[self.at_fn.unwrap().0].val_alloc.alloc_val()
    }

    fn alloc_val(&mut self, typ: ValueType) -> Value {
        Value {
            typ,
            id: self.alloc_val_id(),
        }
    }

    fn alloc_int(&mut self, size: usize) -> IntValue {
        IntValue {
            size,
            id: self.alloc_val_id(),
        }
    }

    fn alloc_ptr(&mut self) -> PtrValue {
        PtrValue(self.alloc_val_id())
    }

    fn push_inst(&mut self, inst: Instruction) {
        self.program.functions[self.at_fn.unwrap().0].blocks[self.at_bb.unwrap().0].insts.push(inst);
    }

    pub fn push_int_op(&mut self, op: IntOp, a: IntValue, b: IntValue) -> IntValue {
        let d = self.alloc_int(op.result_size(a.size, b.size));
        self.push_inst(Instruction::IntOp(op, d, a, b));
        d
    }

    pub fn push_alloc(&mut self, t: Type) -> PtrValue {
        let d = self.alloc_ptr();
        self.push_inst(Instruction::Alloc(d, t));
        d
    }

    pub fn push_load(&mut self, p: PtrValue, t: ValueType) -> Value {
        let d = self.alloc_val(t);
        self.push_inst(Instruction::Load(d, p));
        d
    }

    pub fn push_store<V: Into<Value>>(&mut self, p: PtrValue, v: V) {
        self.push_inst(Instruction::Store(p, v.into()));
    }

    pub fn push_int_const(&mut self, size: usize, val: u128) -> IntValue {
        let d = self.alloc_int(size);
        self.push_inst(Instruction::Assign(d.into(), val));
        d
    }

    pub fn push_copy<V: Into<Value>>(&mut self, v: V) -> Value {
        let v = v.into();
        let d = self.alloc_val(v.typ);
        self.push_inst(Instruction::Copy(d, v));
        d
    }

    pub fn push_call(&mut self, f: FuncId, a: Vec<Value>) -> Option<Value> {
        let d = self.program.functions[f.0].returns.map(|t| self.alloc_val(t));
        self.push_inst(Instruction::Call(d, f, a));
        d
    }

    fn set_term(&mut self, term: Terminator) {
        self.program.functions[self.at_fn.unwrap().0].blocks[self.at_bb.unwrap().0].term = term;
    }

    pub fn set_cond_br(&mut self, c: IntValue, a: BlockId, b: BlockId) {
        self.set_term(Terminator::CondBranch(c, a.into(), b.into()))
    }

    pub fn set_uncond_br(&mut self, t: BlockId) {
        self.set_term(Terminator::UncondBranch(t.into()))
    }

    pub fn set_ret(&mut self, v: Option<Value>) {
        self.set_term(Terminator::Return(v))
    }

    pub fn done(mut self) -> Program<'a> {
        for f in self.program.functions.iter_mut() {
            f.do_everything();
        }

        self.program
    }
}

impl core::fmt::Display for Builder<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.program.fmt(f)
    }
}
