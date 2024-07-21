pub mod builder;

pub struct Program {
    functions: Vec<Function>,
}

struct Function {
    blocks: Vec<BasicBlock>,
    next_var: VarId,
    next_ptr: PtrId,
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

impl Function {
    fn alloc_var(&mut self) -> VarId {
        let id = self.next_var;
        self.next_var.0 += 1;
        id
    }

    fn alloc_ptr(&mut self) -> PtrId {
        let id = self.next_ptr;
        self.next_ptr.0 += 1;
        id
    }
}

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
    let dom = dominators(f, &pred);
    println!("{dom:?}");
    let idom = immediate_dominators(&dom);
    println!("{idom:?}");
    let dft = dominance_frontiers(f, &idom, &pred);
    println!("{dft:?}");

    let defs = get_defs(f);
    println!("{defs:?}");

    for d in defs.iter() {
        add_args(f, d, &dft);
    }
}

fn get_defs(f: &Function) -> BlockIdSet {
    let mut defs = vec![HashSet::new(); f.next_ptr.0];
    for (bi, b) in f.blocks.iter().enumerate() {
        for i in b.insts.iter() {
            match i {
                Instruction::StorePtr(PtrId(var), _) if !defs[*var].contains(&BlockId(bi))=> {
                    defs[*var].insert(BlockId(bi));
                },
                _ => {},
            }
        }
    }

    defs
}

fn add_args(f: &mut Function, defs: &HashSet<BlockId>, dft: &BlockIdSet) {
    let mut added = Vec::new();
    let mut w = defs.iter().copied().collect::<Vec<BlockId>>();

    while let Some(x) = w.pop() {
        for y in dft[x.0].iter() {
            if !added.contains(y) {
                f.blocks[y.0].args.push(VarId(0));
                added.push(*y);
                if !defs.contains(y) {
                    w.push(*y);
                }
            }
        }
    }
}

type BlockIdSet = Vec<HashSet<BlockId>>;

fn dominance_frontiers(f: &Function, idom: &[Option<BlockId>], pred: &BlockIdSet) -> BlockIdSet {
    let mut dft = vec![HashSet::new(); f.blocks.len()];

    for (bi, _) in f.blocks.iter().enumerate() {
        if let Some(ib) = idom[bi] {
            for p in pred[bi].iter() {
                let mut runner = *p;

                while runner != ib {
                    dft[runner.0].insert(BlockId(bi));
                    if let Some(r) = idom[runner.0] {
                        runner = r;
                    } else {
                        break;
                    }
                }
            }
        }
    }

    dft
}

fn immediate_dominators(dom: &BlockIdSet) -> Vec<Option<BlockId>> {
    let mut idom = vec![None; dom.len()];

    for (bi, b) in dom.iter().enumerate().skip(1) {
        'check: for (ci, c) in dom.iter().enumerate().rev() {
            if ci == bi || !b.contains(&BlockId(ci)) {
                println!("{bi} !sdom {ci} {c:?}");
                continue;
            }

            for o in c.iter() {
                if o.0 == ci { continue; }
                if bi != o.0 && dom[o.0].contains(&BlockId(bi)) {
                    println!("{bi} {ci} {o}");
                    continue 'check;
                }
            }

            idom[bi] = Some(BlockId(ci));
            break;
        }
    }

    idom
}

// https://users-cs.au.dk/gerth/advising/thesis/henrik-knakkegaard-christensen.pdf page 18
fn dominators(f: &Function, pred: &BlockIdSet) -> BlockIdSet {
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
            for q in pred[vi].iter() {
                new.retain(|e| dom[q.0].contains(&e));
            }

            new.insert(BlockId(vi));
            changed |= new != dom[vi];
            dom[vi] = new;
        }
    }

    dom
}

fn pred(f: &Function) -> BlockIdSet {
    let mut pred = vec![HashSet::new(); f.blocks.len()];

    for b in 0..f.blocks.len() {
        for is in f.blocks[b].term.immediate_successor() {
            pred[is.0].insert(BlockId(b));
        }
    }

    pred
}

impl Terminator {
    fn immediate_successor(&self) -> Vec<BlockId> {
        match self {
            Self::CondBranch(_, a, b) => vec![*a, *b],
            Self::UncondBranch(t) => vec![*t],
            Self::Return(_) | Self::None => vec![],
        }
    }
}
