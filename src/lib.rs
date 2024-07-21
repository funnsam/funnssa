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
    Move(Value, Value),

    Load(Value, PtrValue),
    Store(PtrValue, Value),
}

enum Terminator {
    CondBranch(IntValue, TermBlockId, TermBlockId),
    UncondBranch(TermBlockId),
    Return(Option<Value>),
    None,
}

#[derive(Clone)]
struct TermBlockId {
    target: BlockId,
    args: Vec<ValueId>,
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

impl Into<TermBlockId> for BlockId {
    fn into(self) -> TermBlockId {
        TermBlockId { target: self, args: vec![] }
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

    let vdefs = get_val_defs(f);
    println!("{vdefs:?}");
    let adefs = get_alloc_defs(f);
    println!("{adefs:?}");

    for (i, (d, typ)) in adefs.iter() {
        add_args(f, d, *i, *typ, &dft, &pred);
    }

    let mut rdef: Vec<Option<ValueId>> = vec![Some(ValueId(1)); f.next_val.0];
    dom_tree_iter(&idom, BlockId(0), &mut |node| {
        let update_reaching_def = |rdef: &mut Vec<Option<ValueId>>, v: &ValueId, ii| {
            let mut r = rdef[v.0];
            while let Some(cr) = r {
                let def_r = vdefs[cr.0];
                if dominates_inst(def_r.0, def_r.1, node, ii, &dom) { break; }

                r = rdef[cr.0];
            }
            println!("{v} {:?} {r:?}", rdef[v.0]);
            rdef[v.0] = r;
        };

        println!("{node}");

        for (ii, i) in f.blocks[node.0].insts.iter_mut().enumerate() {
            match i {
                Instruction::Load(d, p) => {
                    update_reaching_def(&mut rdef, &p.0, ii);
                    if let Some(rdef) = rdef[p.0.0] {
                        *i = Instruction::Move(*d, Value { typ: d.typ, id: rdef });
                    }
                },
                Instruction::Store(d, v) => {
                    update_reaching_def(&mut rdef, &v.id, ii);
                    rdef[d.0.0] = rdef[v.id.0];
                    rdef[v.id.0] = Some(d.0);
                },
                _ => {},
            }
        }

        println!("φ");

        for pred in f.blocks[node.0].term.immediate_successor_mut() {
            for v in pred.args.iter_mut() {
                update_reaching_def(&mut rdef, v, usize::MAX);
                if let Some(rdef) = rdef[v.0] {
                    *v = rdef;
                }
            }
        }
    });

    println!("{rdef:?}");
}

fn get_alloc_defs(f: &Function) -> HashMap<ValueId, (HashSet<BlockId>, ValueType)> {
    let mut defs = HashMap::new();
    for b in f.blocks.iter() {
        for i in b.insts.iter().rev() {
            match i {
                Instruction::Alloc(v, t) => {
                    defs.insert(v.0, (HashSet::new(), t.clone().into()));
                },
                _ => {},
            }
        }
    }
    for (bi, b) in f.blocks.iter().enumerate() {
        for i in b.insts.iter().rev() {
            match i {
                Instruction::Store(ptr, _) => if let Some(d) = defs.get_mut(&ptr.0) {
                    d.0.insert(BlockId(bi));
                },
                _ => {},
            }
        }
    }

    defs
}

fn get_val_defs(f: &Function) -> Vec<(BlockId, usize)> {
    let mut defs = vec![(BlockId(0), 0); f.next_val.0];

    for (bi, b) in f.blocks.iter().enumerate() {
        for (ii, i) in b.insts.iter().enumerate() {
            match i {
                Instruction::Assign(Value { id, .. }, _)
                    | Instruction::Load(Value { id, .. }, _)
                    | Instruction::Move(Value { id, .. }, _)
                    | Instruction::IntOp(_, IntValue { id, .. }, ..)
                    | Instruction::Alloc(PtrValue(id), _)
                => defs[id.0] = (BlockId(bi), ii),
                _ => {},
            }
        }
    }

    defs
}

fn add_args(f: &mut Function, defs: &HashSet<BlockId>, id: ValueId, typ: ValueType, dft: &BlockIdSet, pred: &BlockIdSet) {
    let mut added = Vec::new();
    let mut w = defs.iter().copied().collect::<Vec<BlockId>>();

    while let Some(x) = w.pop() {
        for y in dft[x.0].iter() {
            if !added.contains(y) {
                f.blocks[y.0].args.push(Value { typ, id });
                for p in pred[y.0].iter() {
                    f.blocks[p.0].term.push_arg(*y, id);
                }

                added.push(*y);
                if !defs.contains(y) {
                    w.push(*y);
                }
            }
        }
    }
}

fn dom_tree_iter<F: FnMut(BlockId)>(idom: &[Option<BlockId>], at: BlockId, f: &mut F) {
    f(at);
    for (i, d) in idom.iter().enumerate() {
        if *d == Some(at) {
            dom_tree_iter(idom, BlockId(i), f);
        }
    }
}

// fn find_reaching_def(f: &mut Function, v: ValueId, bb: BlockId, inst: usize, defs: &[(BlockId, usize)], dom: &BlockIdSet) -> ValueId {
//     let mut rdef = Some(v);
// 
//     while let Some(rdef) = rdef {
//         let (db, di) = defs[rdef.0];
//         if dominates_inst(bb, inst, db, di, dom) { break; }
// 
//         rdef = rdef.rdef;
//     }
// 
//     rdef
// }
// 

fn dominates_inst(bb1: BlockId, inst1: usize, bb2: BlockId, inst2: usize, dom: &BlockIdSet) -> bool {
    return dom[bb1.0].contains(&bb2) || (bb1 == bb2 && inst1 < inst2);
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
            pred[is.target.0].insert(BlockId(b));
        }
    }

    pred
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
                println!("    {bi} -> {};", ep.target.0);
            }
        }
        println!("}}");
    }
}
