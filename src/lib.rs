pub mod builder;
pub mod types;
pub mod value;
mod display;

use std::collections::{HashMap, HashSet};
use types::*;
use value::*;

pub struct Program<'a> {
    functions: Vec<Function<'a>>,
}

struct Function<'a> {
    name: &'a str,
    blocks: Vec<BasicBlock>,

    next_val: ValueId,
}

struct BasicBlock {
    args: Vec<Value>,
    insts: Vec<Instruction>,
    term: Terminator,
}

enum Instruction {
    Assign(Value, u128),
    Alloc(PtrValue, Type),
    IntOp(IntOp, IntValue, IntValue, IntValue),

    Load(Value, PtrValue),
    Store(PtrValue, Value),
}

pub enum Terminator {
    CondBranch(IntValue, BlockId, BlockId),
    UncondBranch(BlockId),
    Return(Option<Value>),
    None,
}

pub enum IntOp {
    Add,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct BlockId(pub(crate) usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FuncId(pub(crate) usize);

impl Function<'_> {
    fn alloc_val(&mut self) -> ValueId {
        let id = self.next_val;
        self.next_val.0 += 1;
        id
    }
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

    for (_, (d, typ)) in defs.iter() {
        add_args(f, d, *typ, &dft);
    }
}

fn get_defs(f: &Function) -> HashMap<ValueId, (HashSet<BlockId>, ValueType)> {
    let mut defs = HashMap::new();
    for (bi, b) in f.blocks.iter().enumerate() {
        for i in b.insts.iter().rev() {
            match i {
                Instruction::Alloc(v, t) => {
                    defs.insert(v.0, (HashSet::new(), t.clone().into()));
                },
                Instruction::Store(ptr, _) => if let Some(d) = defs.get_mut(&ptr.0) {
                    d.0.insert(BlockId(bi));
                },
                _ => {},
            }
        }
    }

    defs
}

fn add_args(f: &mut Function, defs: &HashSet<BlockId>, typ: ValueType, dft: &BlockIdSet) {
    let mut added = Vec::new();
    let mut w = defs.iter().copied().collect::<Vec<BlockId>>();

    while let Some(x) = w.pop() {
        for y in dft[x.0].iter() {
            if !added.contains(y) {
                f.blocks[y.0].args.push(Value { typ, id: ValueId(0) });
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

impl Program<'_> {
    pub fn print_cfg(&self) {
        for f in self.functions.iter() {
            f.print_cfg();
        }
    }
}

impl Function<'_> {
    pub fn print_cfg(&self) {
        println!("digraph {} {{", self.name);
        for (bi, b) in self.blocks.iter().enumerate() {
            println!("    {bi};");
            for ep in b.term.immediate_successor() {
                println!("    {bi} -> {};", ep.0);
            }
        }
        println!("}}");
    }
}
