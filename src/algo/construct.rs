use crate::*;

type BlockIdSet = Vec<HashSet<BlockId>>;

fn dominates_inst(bb2: BlockId, inst2: usize, bb1: BlockId, inst1: usize, dom: &BlockIdSet) -> bool {
    return dom[bb1.0].contains(&bb2) || (bb1 == bb2 && inst1 <= inst2);
}

fn immediate_dominators(dom: &BlockIdSet) -> Vec<Option<BlockId>> {
    let mut idom = vec![None; dom.len()];

    for (bi, b) in dom.iter().enumerate().skip(1) {
        'check: for (ci, c) in dom.iter().enumerate().rev() {
            if ci == bi || !b.contains(&BlockId(ci)) {
                continue;
            }

            for o in c.iter() {
                if o.0 == ci { continue; }
                if bi != o.0 && dom[o.0].contains(&BlockId(bi)) {
                    continue 'check;
                }
            }

            idom[bi] = Some(BlockId(ci));
            break;
        }
    }

    idom
}

fn dom_tree_iter<F: FnMut(BlockId)>(idom: &[Option<BlockId>], at: BlockId, f: &mut F) {
    f(at);
    for (i, d) in idom.iter().enumerate() {
        if *d == Some(at) {
            dom_tree_iter(idom, BlockId(i), f);
        }
    }
}

impl Function<'_> {
    pub fn construct_ssa(&mut self) {
        let mut pred = self.pred();
        println!("{pred:?}");

        self.remove_crit_edge(&mut pred);

        let dom = self.dominators(&pred);
        println!("{dom:?}");
        let idom = immediate_dominators(&dom);
        println!("{idom:?}");
        let dft = self.dominance_frontiers(&idom, &pred);
        println!("{dft:?}");

        let mut vdefs = self.get_val_defs();
        println!("{vdefs:?}");
        let adefs = self.get_alloc_defs();
        println!("{adefs:?}");

        self.phi_lower(&mut vdefs, &adefs, &dft, &dom, &idom, &pred);
    }

    fn remove_crit_edge(&mut self, pred: &mut BlockIdSet) {
        for bi in 0..self.blocks.len() {
            let is: Vec<_> = self.blocks[bi].term.immediate_successor().iter().map(|v| v.target).collect();

            if is.len() < 2 { continue; }
            for s in is.into_iter() {
                if pred[s.0].len() < 2 { continue; }

                let ceid = self.blocks.len();
                self.blocks.push(BasicBlock {
                    args: vec![],
                    insts: vec![],
                    term: Terminator::UncondBranch(s.into()),
                });

                self.blocks[bi].term.replace(s, BlockId(ceid));
                pred[s.0].remove(&BlockId(bi));
                pred[s.0].insert(BlockId(ceid));
                let mut pr = HashSet::new();
                pr.insert(BlockId(bi));
                pred.push(pr);
            }
        }
    }

    fn phi_lower(
        &mut self,
        vdefs: &mut Vec<(BlockId, usize)>,
        adefs: &HashMap<ValueId, (HashSet<BlockId>, ValueType)>,
        dft: &BlockIdSet,
        dom: &BlockIdSet,
        idom: &[Option<BlockId>],
        pred: &BlockIdSet,
    ) {
        for (i, (d, typ)) in adefs.iter() {
            self.add_args(d, *i, *typ, &dft, &pred);
        }

        let mut rdef: Vec<Option<ValueId>> = vec![None; self.val_alloc.0.0];
        dom_tree_iter(&idom, BlockId(0), &mut |node| {
            let update_reaching_def = |rdef: &mut Vec<Option<ValueId>>, vdefs: &Vec<(BlockId, usize)>, v: &ValueId, ii| {
                let mut r = rdef[v.0];
                while let Some(cr) = r {
                    let def_r = vdefs[cr.0];
                    if dominates_inst(def_r.0, def_r.1, node, ii, &dom) { break; }

                    r = rdef[cr.0];
                }
                rdef[v.0] = r;
            };

            for pi in 0..self.blocks[node.0].args.len() {
                let parm = self.blocks[node.0].args[pi];
                update_reaching_def(&mut rdef, &vdefs, &parm.id, 0);
                let vd = self.val_alloc.alloc_val();
                rdef.push(rdef[parm.id.0]);
                vdefs.push((node, 0));
                rdef[parm.id.0] = Some(vd);
                self.blocks[node.0].args[pi].id = vd;
            }

            for (ii, i) in self.blocks[node.0].insts.iter_mut().enumerate() {
                match i.clone() {
                    Instruction::Load(d, p) => {
                        update_reaching_def(&mut rdef, &vdefs, &p.0, ii);
                        if let Some(rdef) = rdef[p.0.0] {
                            *i = Instruction::Copy(d, Value { typ: d.typ, id: rdef });
                        }
                    },
                    Instruction::Store(d, v) => {
                        update_reaching_def(&mut rdef, &vdefs, &d.0, ii);
                        let vd = self.val_alloc.alloc_val();
                        rdef.push(rdef[d.0.0]);
                        vdefs.push((node, 0));
                        rdef[d.0.0] = Some(vd);

                        *i = Instruction::Copy(Value { typ: v.typ, id: vd }, v);
                    },
                    _ => {},
                }
            }

            for succ in self.blocks[node.0].term.immediate_successor_mut() {
                for v in succ.args.iter_mut() {
                    update_reaching_def(&mut rdef, &vdefs, v, usize::MAX);
                    if let Some(rdef) = rdef[v.0] {
                        *v = rdef;
                    }
                }
            }
        });
    }

    fn get_alloc_defs(&self) -> HashMap<ValueId, (HashSet<BlockId>, ValueType)> {
        let mut defs = HashMap::new();
        for b in self.blocks.iter() {
            for i in b.insts.iter().rev() {
                match i {
                    Instruction::Alloc(v, t) => {
                        defs.insert(v.0, (HashSet::new(), t.clone().into()));
                    },
                    _ => {},
                }
            }
        }
        for (bi, b) in self.blocks.iter().enumerate() {
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

    fn get_val_defs(&self) -> Vec<(BlockId, usize)> {
        let mut defs = vec![(BlockId(0), 0); self.val_alloc.0.0];

        for (bi, b) in self.blocks.iter().enumerate() {
            for (ii, i) in b.insts.iter().enumerate() {
                match i {
                    Instruction::Assign(Value { id, .. }, _)
                        | Instruction::Load(Value { id, .. }, _)
                        | Instruction::Copy(Value { id, .. }, _)
                        | Instruction::IntOp(_, IntValue { id, .. }, ..)
                        | Instruction::Alloc(PtrValue(id), _)
                        => defs[id.0] = (BlockId(bi), ii),
                    _ => {},
                }
            }
        }

        defs
    }

    fn add_args(&mut self, defs: &HashSet<BlockId>, id: ValueId, typ: ValueType, dft: &BlockIdSet, pred: &BlockIdSet) {
        let mut added = Vec::new();
        let mut w = defs.iter().copied().collect::<Vec<BlockId>>();

        while let Some(x) = w.pop() {
            for y in dft[x.0].iter() {
                if !added.contains(y) {
                    self.blocks[y.0].args.push(Value { typ, id });
                    for p in pred[y.0].iter() {
                        self.blocks[p.0].term.push_arg(*y, id);
                    }

                    added.push(*y);
                    if !defs.contains(y) {
                        w.push(*y);
                    }
                }
            }
        }
    }

    fn dominance_frontiers(&self, idom: &[Option<BlockId>], pred: &BlockIdSet) -> BlockIdSet {
        let mut dft = vec![HashSet::new(); self.blocks.len()];

        for (bi, _) in self.blocks.iter().enumerate() {
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

    // https://users-cs.au.dk/gerth/advising/thesis/henrik-knakkegaard-christensen.pdf page 18
    fn dominators(&self, pred: &BlockIdSet) -> BlockIdSet {
        let mut dom = vec![HashSet::new(); self.blocks.len()];
        dom[0].insert(BlockId(0));

        let all_v = (0..self.blocks.len()).map(|b| BlockId(b)).collect::<HashSet<BlockId>>();
        for (vi, _) in self.blocks.iter().enumerate().skip(1) {
            dom[vi] = all_v.clone();
        }

        let mut changed = true;

        while changed {
            changed = false;

            for (vi, _) in self.blocks.iter().enumerate().skip(1) {
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

    fn pred(&self) -> BlockIdSet {
        let mut pred = vec![HashSet::new(); self.blocks.len()];

        for b in 0..self.blocks.len() {
            for is in self.blocks[b].term.immediate_successor() {
                pred[is.target.0].insert(BlockId(b));
            }
        }

        pred
    }
}
