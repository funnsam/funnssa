use crate::{*, types::*, value::*};

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

            next_val: ValueId(0),
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

    fn alloc_val_id(&mut self) -> ValueId {
        self.program.functions[self.at_fn.unwrap().0].alloc_val()
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
        let d = self.alloc_int(a.size);
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

    pub fn push_store(&mut self, p: PtrValue, v: Value) {
        self.push_inst(Instruction::Store(p, v));
    }

    pub fn int_const(&mut self, size: usize, val: u128) -> IntValue {
        let d = self.alloc_int(size);
        self.push_inst(Instruction::Assign(d.into(), val));
        d
    }

    pub fn set_term(&mut self, term: Terminator) {
        self.program.functions[self.at_fn.unwrap().0].blocks[self.at_bb.unwrap().0].term = term;
    }

    pub fn done(mut self) -> Program<'a> {
        destruct(&mut self.program.functions[0]);
        self.program
    }
}
