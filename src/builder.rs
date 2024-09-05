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

    fn build_inst(&mut self, inst: Instruction) {
        self.program.functions[self.at_fn.unwrap().0].blocks[self.at_bb.unwrap().0].insts.push(inst);
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

macro_rules! build_inst {
    (alloct Value) => { Self::alloc_val };
    (alloct ValueId) => { Self::alloc_val_id };
    (alloct IntValue) => { Self::alloc_int };
    (alloct PtrValue) => { Self::alloc_ptr };

    ($i:ident ($($an:ident : $at:ty),*) -> alloc $t:tt ( $arg:expr ) = $inst:expr) => {
        impl Builder<'_> {
            pub fn $i(&mut self, $($an: $at),*) -> $t {
                let d = build_inst!(alloct $t)(self, $arg);
                self.build_inst($inst(d));
                d
            }
        }
    };
    ($i:ident ($($an:ident : $at:ty),*) -> alloc $t:tt = $inst:expr) => {
        impl Builder<'_> {
            pub fn $i(&mut self, $($an: $at),*) -> $t {
                let d = build_inst!(alloct $t)(self);
                self.build_inst($inst(d));
                d
            }
        }
    };
    ($i:ident ($($an:ident : $at:ty),*) = $inst:expr) => {
        impl Builder<'_> {
            pub fn $i(&mut self, $($an: $at),*) {
                self.build_inst($inst);
            }
        }
    };
}

build_inst!(build_alloc(typ: Type) -> alloc PtrValue = |d| Instruction::Alloc(d, typ));

impl Builder<'_> {
    pub fn build_call(&mut self, f: FuncId, a: Vec<Value>) -> Option<Value> {
        let d = self.program.functions[f.0].returns.map(|t| self.alloc_val(t));
        self.build_inst(Instruction::Call(d, f, a));
        d
    }
}

build_inst!(build_int_const(size: usize, val: u128) -> alloc IntValue(size) = |d| Instruction::Assign(d, val));
build_inst!(build_copy(v: Value) -> alloc Value(v.typ) = |d| Instruction::Copy(d, v));
build_inst!(build_int_op(op: IntOp, a: IntValue, b: IntValue) -> alloc IntValue(op.result_size(a.size, b.size)) = |d| Instruction::IntOp(op, d, a, b));

build_inst!(build_load(p: PtrValue, t: ValueType) -> alloc Value(t) = |d| Instruction::Load(d, p));
build_inst!(build_store(p: PtrValue, v: impl Into<Value>) = Instruction::Store(p, v.into()));
