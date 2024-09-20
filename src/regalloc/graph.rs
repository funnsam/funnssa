use super::*;
use non_random_state::*;

type InnerLoc = (Option<BlockId>, usize);

pub struct GraphAlloc<R: Register> {
    first_def: HashMap<VReg<R>, InnerLoc>,
    last_uses: HashMap<VReg<R>, Vec<InnerLoc>>,
    coalesce_to: HashMap<VReg<R>, Vec<VReg<R>>>,
    priority: HashMap<VReg<R>, isize>,

    at_block: Option<BlockId>,
    at_inst: usize,
}

impl<R: Register + 'static +core::fmt::Debug> RegAlloc<R> for GraphAlloc<R> {
    fn new_sized(_size: usize) -> Self {
        Self {
            first_def: HashMap::new(),
            last_uses: HashMap::new(),
            coalesce_to: HashMap::new(),
            priority: HashMap::new(),

            at_block: None,
            at_inst: 0,
        }
    }

    fn next_inst(&mut self) {
        self.at_inst += 1;
    }

    fn next_block(&mut self) {
        if let Some(ab) = &mut self.at_block {
            ab.0 += 1;
        }

        self.at_inst = 0;
    }

    fn next_fn(&mut self) {
        self.first_def.clear();
        self.last_uses.clear();
        self.coalesce_to.clear();

        self.at_block = None;
        self.at_inst = 0;
    }

    fn prologue_end(&mut self) {
        self.at_block = Some(BlockId(0));
        self.at_inst = 0;
    }

    fn define(&mut self, vr: VReg<R>) {
        self.first_def.entry(vr)
            .and_modify(|_| panic!("redef vreg"))
            .or_insert((self.at_block, self.at_inst));
    }

    fn add_use(&mut self, vr: VReg<R>) {
        let u = self.last_uses.entry(vr).or_default();

        if u.last().map_or(false, |(b, _)| b == &self.at_block) {
            u.last_mut().unwrap().1 = self.at_inst
        } else {
            u.push((self.at_block, self.at_inst));
        }

        self.prioritize(vr, 1);
    }

    fn coalesce_move(&mut self, from: VReg<R>, to: VReg<R>) {
        self.coalesce_to.entry(from).or_default().push(to);
        self.coalesce_to.entry(to).or_default().push(from);
        self.prioritize(from, 2);
        self.prioritize(to, 2);
    }

    fn prioritize(&mut self, vr: VReg<R>, by: isize) {
        *self.priority.entry(vr).or_default() += by;
    }

    fn alloc_regs<I: Inst<Register = R>>(&mut self, alloc: &mut [VReg<R>], cfg: cfg::Cfg, gen: &VCodeGen<I>) {
        let mut live_in = vec![Vec::new(); self.at_block.unwrap().0 + 1];
        let mut live_out = vec![Vec::new(); self.at_block.unwrap().0 + 1];

        for (v, uses) in self.last_uses.iter() {
            for (ub, _ui) in uses.iter() {
                if ub.is_some() && cfg.bb_term(ub.unwrap()).immediate_successor().into_iter().any(|e| {
                    e.args.iter().any(|e| gen.get_value_vreg_no_add(*e) == Some(*v))
                }) {
                    let bidx = ub.map_or(0, |b| b.0 + 1);
                    live_out[bidx].push(*v);
                }

                self.mark(*ub, *v, &cfg, gen, &mut live_in, &mut live_out);
            }
        }

        println!("{live_in:?}");
        println!("{live_out:?}");

        let mut intg: HashMap<VReg<R>, HashSet<VReg<R>>> = HashMap::new();
        for live in live_in.iter().chain(live_out.iter()) {
            for i in live.iter() {
                for j in live.iter() {
                    if i != j {
                        intg.entry(*i).or_default().insert(*j);
                    }
                }
            }
        }

        for (i, ..) in self.first_def.iter() {
            intg.entry(*i).or_default();
        }


        // lives in the bb
        //
        // a b
        //      bb:
        // |        def a
        // |*|      def b
        // |*|      last use a
        //   |      last use b
        for (i, (ib, ii)) in self.first_def.iter() {
            if self.last_uses.get(i).map_or(true, |u| u.len() != 1 || u[0].0 != *ib) { continue; }

            for (j, (jb, ji)) in self.first_def.iter() {
                if ib != jb || self.last_uses.get(j).map_or(true, |u| u.len() != 1 || u[0].0 != *ib) { continue; }


                if i != j && *ii < self.last_uses[j][0].1 && *ji < self.last_uses[i][0].1 {
                    intg.get_mut(i).unwrap().insert(*j);
                    intg.get_mut(j).unwrap().insert(*i);
                }
            }
        }

        // lives across the entire bb
        //
        // a b
        // |    bb: a
        // |*|      def b
        // |*|      last use b
        // |        escape a
        for (i, (db, _)) in self.first_def.iter() {
            let db_idx = db.map_or(0, |i| i.0 + 1);

            for j in live_in[db_idx].iter() {
                if live_out[db_idx].contains(j) && i != j {
                    intg.get_mut(i).unwrap().insert(*j);
                    intg.get_mut(j).unwrap().insert(*i);
                }
            }
        }

        // lives partially in the bb, came in
        //
        // a b
        // |    bb: a
        // |*|      def b
        // |*|      last use a
        //   |      use b
        for (i, u) in self.last_uses.iter() {
            for (ab, ai) in u.iter() {
                let ab_idx = ab.map_or(0, |i| i.0 + 1);

                for (j, bi) in live_in[ab_idx].iter().filter_map(|j| self.last_uses.get(j)?.iter().find(|(b, _)| ab == b).map(|(_, i)| (j, i))) {
                    if i != j && ai <= bi {
                        intg.get_mut(i).unwrap().insert(*j);
                        intg.get_mut(j).unwrap().insert(*i);
                    }
                }
            }
        }

        // lives partially in the bb, came out
        //
        // a b
        //      bb:
        // |        def a
        // |*|      def b
        // |*|      last use a
        //   |      escape b
        for (i, u) in self.last_uses.iter() {
            for (ab, ai) in u.iter() {
                let ab_idx = ab.map_or(0, |i| i.0 + 1);

                for (j, bi) in live_in[ab_idx].iter().filter_map(|j| self.last_uses.get(j)?.iter().find(|(b, _)| ab == b).map(|(_, i)| (j, i))) {
                    if i != j && ai <= bi {
                        intg.get_mut(i).unwrap().insert(*j);
                        intg.get_mut(j).unwrap().insert(*i);
                    }
                }
            }
        }

        for (i, (db, _)) in self.first_def.iter() {
            for j in live_out[db.map_or(0, |b| b.0 + 1)].iter() {
                if i != j {
                    intg.get_mut(i).unwrap().insert(*j);
                    intg.get_mut(j).unwrap().insert(*i);
                }
            }
        }

        for (cf, cts) in self.coalesce_to.iter_mut() {
            cts.retain(|t| intg.get(cf).map_or(true, |i| !i.contains(t)));
        }
        self.coalesce_to.retain(|_, c| !c.is_empty());

        let mut color = HashMap::with_capacity(intg.len());
        let mut tryc = HashMap::new();
        let mut order = Vec::with_capacity(intg.len());

        for (node, edges) in intg.iter() {
            match node {
                VReg::VirtReal(v, r) => {
                    alloc[*v] = VReg::Real(*r);
                    if let Some((c, _)) = R::get_regs().iter().enumerate().find(|(_, r2)| r == *r2) {
                        color.insert(*node, c);
                        self.color_coalesce_recursive(node, &mut tryc, c);
                    }
                },
                VReg::Real(r) => {
                    if let Some((c, _)) = R::get_regs().iter().enumerate().find(|(_, r2)| r == *r2) {
                        color.insert(*node, c);
                        self.color_coalesce_recursive(node, &mut tryc, c);
                    }
                },
                _ => {
                    order.push((node, edges.len(), *self.priority.get(node).unwrap_or(&0)));
                },
            }
        }

        order.sort_unstable_by(|(_, ae, au), (_, be, bu)| {
            bu.cmp(au).then_with(|| be.cmp(ae))
        });

        for (node, ..) in order.into_iter() {
            if color.contains_key(node) { continue; }

            let color_ok = |edges: &HashSet<VReg<R>>, color: &HashMap<VReg<R>, usize>, c: usize| {
                !edges.iter().any(|e| color.get(e).map_or(false, |e| *e == c))
            };

            if let Some(tc) = tryc.get(node) {
                if color_ok(&intg[node], &color, *tc) {
                    color.insert(*node, *tc);
                    tryc.remove_entry(node);
                    continue;
                }
            }

            let c = (0..).find(|c| color_ok(&intg[node], &color, *c)).unwrap();
            color.insert(*node, c);
            self.color_coalesce_recursive(node, &mut color, c);
        }

        for (v, c) in color.iter() {
            if let VReg::Virtual(v) = v { alloc[*v] = R::get_regs().get(*c).cloned().map_or_else(|| VReg::Spilled(*c - R::get_regs().len()), VReg::Real) }
        }

        #[cfg(debug_assertions)] {
            print!("graph intg {{");
            for (v, i) in intg.iter() {
                let vc = color.get(v);
                print!("\"{v}({})\";", vc.unwrap_or(&usize::MAX));

                for i in i.iter() {
                    let ic = color.get(i);
                    print!("\"{v}({})\"--\"{i}({})\";", vc.unwrap_or(&usize::MAX), ic.unwrap_or(&usize::MAX));

                    if vc.is_some() && vc == ic {
                        println!("");
                    }
                }
            }
            println!("}}");

            for (v, i) in intg.iter() {
                let vc = color.get(v);

                for i in i.iter() {
                    let ic = color.get(i);

                    if vc.is_some() && vc == ic {
                        println!("warn: coloring incorrect at \"{v}\"--\"{i}\"");
                    }
                }
            }
        }
    }
}

impl<R: Register> GraphAlloc<R> {
    fn mark<I: Inst<Register = R>>(
        &self,
        block: Option<BlockId>,
        v: VReg<R>,
        cfg: &cfg::Cfg,
        gen: &VCodeGen<I>,
        live_in: &mut [Vec<VReg<R>>],
        live_out: &mut [Vec<VReg<R>>],
    ) {
        let bidx = block.map_or(0, |b| b.0 + 1);
        if self.first_def.contains_key(&v) && self.first_def[&v].0 == block { return; }
        if live_in[bidx].last() == Some(&v) { return; }
        live_in[bidx].push(v);

        if block.is_some() && cfg.bb_def_args(block.unwrap()).iter().any(|e| gen.get_value_vreg_no_add(e.id) == Some(v)) { return; }

        if let Some(block) = block {
            for p in cfg.bb_imm_preds(block) {
                live_out[p.0 + 1].push(v);

                self.mark(Some(*p), v, cfg, gen, live_in, live_out);
            }
        }
    }

    fn color_coalesce_recursive(&self, node: &VReg<R>, color: &mut HashMap<VReg<R>, usize>, c: usize) {
        if let Some(cts) = self.coalesce_to.get(node) {
            for ct in cts.iter() {
                if !color.contains_key(ct) {
                    color.insert(*ct, c);
                    self.color_coalesce_recursive(ct, color, c);
                }
            }
        }
    }
}
