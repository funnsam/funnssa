#![warn(
    clippy::complexity,
    clippy::correctness,
    clippy::perf,
    clippy::nursery,
    clippy::suspicious,
    clippy::style
)]
#![allow(
    clippy::semicolon_inside_block,
    clippy::just_underscores_and_digits
)]

pub mod algo;
pub mod arch;
pub mod builder;
pub mod types;
pub mod value;
mod display;

use std::collections::{HashMap, HashSet};
use types::*;
use value::*;

#[derive(Clone)]
pub struct Program<'a> {
    functions: Vec<Function<'a>>,
}

#[derive(Clone)]
struct Function<'a> {
    name: &'a str,
    blocks: Vec<BasicBlock>,

    val_alloc: ValAlloc,
}

#[derive(Clone)]
struct BasicBlock {
    args: Vec<Value>,
    insts: Vec<Instruction>,
    term: Terminator,
}

#[derive(Clone)]
pub enum Instruction {
    Alloc(PtrValue, Type),
    Assign(Value, u128),
    Copy(Value, Value),
    IntOp(IntOp, IntValue, IntValue, IntValue),

    Load(Value, PtrValue),
    Store(PtrValue, Value),
}

#[derive(Clone)]
pub enum Terminator {
    CondBranch(IntValue, TermBlockId, TermBlockId),
    UncondBranch(TermBlockId),
    Return(Option<Value>),
    None,
}

#[derive(Clone)]
pub struct TermBlockId {
    pub target: BlockId,
    args: Vec<ValueId>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, strum::Display)]
#[strum(serialize_all = "lowercase")]
pub enum IntOp {
    Add,
    Sub,
    Mul,
    UDiv,
    SDiv,
    Lsh,
    URsh,
    SRsh,
    And,
    Or,
    Xor,

    ULt,
    ULe,
    UGt,
    UGe,
    SLt,
    SLe,
    SGt,
    SGe,
    Eq,
    Ne,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct BlockId(pub(crate) usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FuncId(pub(crate) usize);

#[derive(Clone)]
struct ValAlloc(ValueId);

impl ValAlloc {
    fn alloc_val(&mut self) -> ValueId {
        let id = self.0;
        self.0.0 += 1;
        id
    }
}

impl From<BlockId> for TermBlockId {
    fn from(val: BlockId) -> Self {
        Self { target: val, args: vec![] }
    }
}

impl Terminator {
    fn immediate_successor(&self) -> Vec<&TermBlockId> {
        match self {
            Self::CondBranch(_, a, b) => vec![a, b],
            Self::UncondBranch(t) => vec![t],
            Self::Return(_) | Self::None => vec![],
        }
    }

    fn immediate_successor_mut(&mut self) -> Vec<&mut TermBlockId> {
        match self {
            Self::CondBranch(_, a, b) => vec![a, b],
            Self::UncondBranch(t) => vec![t],
            Self::Return(_) | Self::None => vec![],
        }
    }

    fn push_arg(&mut self, b: BlockId, arg: ValueId) {
        let update = |tb: &mut TermBlockId| if tb.target == b {
            tb.args.push(arg);
        };

        match self {
            Self::CondBranch(_, a, b) => {
                update(a);
                update(b);
            },
            Self::UncondBranch(t) => update(t),
            Self::Return(_) | Self::None => {},
        }
    }

    fn replace(&mut self, pat: BlockId, to: BlockId) {
        let update = |tb: &mut TermBlockId| if tb.target == pat {
            tb.target = to;
        };

        match self {
            Self::CondBranch(_, a, b) => {
                update(a);
                update(b);
            },
            Self::UncondBranch(t) => update(t),
            Self::Return(_) | Self::None => {},
        }
    }
}

impl Program<'_> {
    pub fn print_cfg(&self) {
        for f in self.functions.iter() {
            f.print_cfg();
        }
    }
}

impl Function<'_> {
    pub fn print_cfg(&self) {
        print!("digraph {} {{\n    ", self.name);
        for (bi, b) in self.blocks.iter().enumerate() {
            print!("{bi};");
            for ep in b.term.immediate_successor() {
                print!("{bi}->{};", ep.target.0);
            }
        }
        println!("\n}}");
    }
}

impl IntOp {
    pub const fn result_size(&self, a: usize, _b: usize) -> usize {
        match self {
            Self::ULt | Self::ULe
                | Self::UGt | Self::UGe
                | Self::SLt | Self::SLe
                | Self::SGt | Self::SGe
                | Self::Eq | Self::Ne
            => 1,
            _ => a,
        }
    }
}
