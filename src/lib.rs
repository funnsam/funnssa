pub mod builder;

pub struct Program {
    functions: Vec<Function>,
}

struct Function {
    blocks: Vec<BasicBlock>,
}

struct BasicBlock {
    args: Vec<VarId>,
    insts: Vec<Instruction>,
    term: Terminator,
}

pub enum Instruction {
    Store(VarId, Value),
    Add(VarId, Value, Value),

    LoadPtr(VarId, PtrId),
    StorePtr(PtrId, Value),
}

pub enum Terminator {
    CondBranch(Value, BlockId, BlockId),
    UncondBranch(BlockId),
    Return(Option<Value>),
    None,
}

pub enum Value {
    Integer(u128),
    Variable(VarId),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct VarId(pub(crate) usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct BlockId(pub(crate) usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct PtrId(pub(crate) usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FuncId(pub(crate) usize);

use core::fmt;
use std::collections::HashSet;

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, fc) in self.functions.iter().enumerate() {
            writeln!(f, "fn {i}:\n{fc}")?;
        }

        Ok(())
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, bb) in self.blocks.iter().enumerate() {
            write!(f, "{i}: {bb}")?;
        }

        Ok(())
    }
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.args.is_empty() {
            write!(f, "require ")?;
            for a in self.args.iter() {
                write!(f, "{a} ")?;
            }
        }
        writeln!(f)?;

        for i in self.insts.iter() {
            writeln!(f, "  {i}")?;
        }

        writeln!(f, "  {}", self.term)
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Store(t, v) => write!(f, "{t} = {v}"),
            Self::Add(t, a, b) => write!(f, "{t} = add {a}, {b}"),
            Self::LoadPtr(t, v) => write!(f, "{t} = load {v}"),
            Self::StorePtr(t, v) => write!(f, "store {t}, {v}"),
        }
    }
}

impl fmt::Display for Terminator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CondBranch(c, a, b) => write!(f, "br {c} {a} {b}"),
            Self::UncondBranch(t) => write!(f, "br {t}"),
            Self::Return(Some(v)) => write!(f, "ret {v}"),
            Self::Return(None) => write!(f, "ret"),
            Self::None => write!(f, "noterm"),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer(i) => write!(f, "{i}"),
            Self::Variable(v) => write!(f, "{v}"),
        }
    }
}

impl fmt::Display for VarId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "#{}", self.0) }
}

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "${}", self.0) }
}

impl fmt::Display for PtrId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "&{}", self.0) }
}

fn destruct(f: &mut Function) {
    let pred = pred(f);
    println!("{pred:?}");
    let dom = dom(f, &pred);
    println!("{dom:?}");
}

/* fn dft(f: &Function) -> Vec<Vec<BlockId>> {
    let mut dft = vec![vec![]; f.blocks.len()];

    for (i, b) in f.blocks.iter().enumerate() {
        let ipred = b.term.imm_pred();
        if ipred.len() >= 2 {
            let idom_b = idom(BlockId(i));

            for p in ipred.iter() {
                let mut runner = *p;
                while runner.0 != idom_b.0 {
                    dft[runner.0].push(BlockId(i));
                    runner = idom(runner);
                }
            }
        }
    }

    dft
} */

fn dom(f: &Function, pred: &[HashSet<BlockId>]) -> Vec<HashSet<BlockId>> {
    let mut dom = vec![HashSet::new(); f.blocks.len()];
    dom[0].insert(BlockId(0));

    let all_v = (0..f.blocks.len()).map(|b| BlockId(b)).collect::<HashSet<BlockId>>();
    for (vi, _) in f.blocks.iter().enumerate().skip(1) {
        dom[vi] = all_v.clone();
    }

    let mut changed = true;

    while changed {
        changed = false;

        for (vi, _) in f.blocks.iter().enumerate().skip(1) {
            let mut new = all_v.clone();
            for (qi, _) in pred[vi].iter().enumerate() {
                new.retain(|e| dom[qi].contains(&e));
            }

            new.insert(BlockId(vi));
            changed |= new == dom[vi];
            dom[vi] = new;
        }
    }

    dom
}

fn pred(f: &Function) -> Vec<HashSet<BlockId>> {
    let mut pred = vec![HashSet::new(); f.blocks.len()];

    fn ins(pred: &mut [HashSet<BlockId>], f: &Function, at: BlockId, b: BlockId) {
        for ip in f.blocks[at.0].term.imm_pred() {
            if !pred[ip.0].contains(&b) {
                pred[ip.0].insert(b);
                ins(pred, f, ip, b);
            }
        }
    }

    for b in 0..f.blocks.len() {
        ins(&mut pred, f, BlockId(b), BlockId(b));
    }

    pred
}

impl Terminator {
    fn imm_pred(&self) -> Vec<BlockId> {
        match self {
            Self::CondBranch(_, a, b) => vec![*a, *b],
            Self::UncondBranch(t) => vec![*t],
            Self::Return(_) | Self::None => vec![],
        }
    }
}
