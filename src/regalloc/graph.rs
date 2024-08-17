use super::*;
use std::collections::HashMap;

type InnerLoc = (Option<BlockId>, usize);

pub struct GraphAlloc<R: Register> {
    first_def: HashMap<VReg<R>, InnerLoc>,
    last_uses: HashMap<VReg<R>, (Vec<InnerLoc>, usize)>,
    coalesce_to: HashMap<VReg<R>, Vec<VReg<R>>>,

    at_block: Option<BlockId>,
    at_inst: usize,
}

impl<R: Register + 'static + core::fmt::Debug> RegAlloc<R> for GraphAlloc<R> {
    fn new_sized(_size: usize) -> Self {
        Self {
            first_def: HashMap::new(),
            last_uses: HashMap::new(),
            coalesce_to: HashMap::new(),

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

        u.1 += 1;
        if u.0.last().map_or(false, |(b, _)| b == &self.at_block) {
            u.0.last_mut().unwrap().1 = self.at_inst
        } else {
            u.0.push((self.at_block, self.at_inst));
        }
    }

    fn coalesce_move(&mut self, from: VReg<R>, to: VReg<R>) {
        self.coalesce_to.entry(from).or_default().push(to);
        self.coalesce_to.entry(to).or_default().push(from);
    }

    fn alloc_regs<I: Inst<Register = R>>(&mut self, alloc: &mut [VReg<R>], cfg: cfg::Cfg, gen: &VCodeGen<I>) {
        let mut live_in = vec![Vec::new(); self.at_block.unwrap().0 + 1];
        let mut live_out = vec![Vec::new(); self.at_block.unwrap().0 + 1];

        for (v, (uses, _)) in self.last_uses.iter() {
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

        for (i, (db, di)) in self.first_def.iter() {
            intg.entry(*i).or_default();

            if let Some((du, _)) = self.last_uses.get(i) {
                if du.len() > 1 || (!du.is_empty() && &du[0].0 != db) { continue; }
                let dr = *di..du.get(0).map_or(usize::MAX, |u| u.1);

                for (j, (uses, _)) in self.last_uses.iter() {
                    intg.entry(*j).or_default();

                    if i == j || uses.len() > 1 || (!uses.is_empty() && &uses[0].0 != db) { continue; }
                    if let Some(ud) = self.first_def.get(j) {
                        let ur = ud.1..uses.get(0).map_or(ud.1, |u| u.1);

                        if dr.start < ur.end && ur.start < dr.end {
                            intg.get_mut(i).unwrap().insert(*j);
                            intg.get_mut(j).unwrap().insert(*i);
                        }
                    }
                }
            }
        }

        print!("graph h{{"); for (v, i) in intg.iter() { print!("{v};"); for i in i.iter() { print!("{v}--{i};"); } }println!("}}");

        for (i, (db, _)) in self.first_def.iter() {
            for j in live_out[db.map_or(0, |b| b.0 + 1)].iter() {
                if i != j {
                    intg.get_mut(i).unwrap().insert(*j);
                    intg.get_mut(j).unwrap().insert(*i);
                }
            }
        }

        print!("graph h{{"); for (v, i) in intg.iter() { print!("{v};"); for i in i.iter() { print!("{v}--{i};"); } }println!("}}");

        for (cf, cts) in self.coalesce_to.iter_mut() {
            cts.retain(|t| intg.get(cf).map_or(true, |i| !i.contains(t)));
        }
        self.coalesce_to.retain(|_, c| !c.is_empty());
        println!("{:?}", self.coalesce_to);

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
                    order.push((node, edges.len(), self.last_uses.get(node).map_or(0, |u| u.1)));
                },
            }
        }

        order.sort_unstable_by(|(a, ae, au), (b, be, bu)| {
            let am = self.coalesce_to.contains_key(a) as usize + 2;
            let bm = self.coalesce_to.contains_key(b) as usize + 2;

            (bu * bm).cmp(&(au * am)).then_with(|| (be * bm).cmp(&(ae * am)))
        });

        for (node, ..) in order.into_iter() {
            if color.contains_key(node) { continue; }

            let color_ok = |edges: &HashSet<VReg<R>>, color: &HashMap<VReg<R>, usize>, c: usize| {
                edges.iter().find(|e| color.get(e).map_or(false, |e| *e == c)).is_none()
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
        println!("{color:?}");

        for (v, c) in color.iter() {
            match v {
                VReg::Virtual(v) => alloc[*v] = R::get_regs().get(*c).cloned().map_or_else(|| VReg::Spilled(*c - R::get_regs().len()), VReg::Real),
                _ => {},
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
