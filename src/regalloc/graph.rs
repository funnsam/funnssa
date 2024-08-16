use super::*;
use std::collections::HashMap;

type InnerRange = core::ops::Range<usize>;

pub struct GraphAlloc<R: Register> {
    first_def: HashMap<VReg<R>, (Option<BlockId>, usize)>,
    last_uses: HashMap<VReg<R>, Vec<(Option<BlockId>, usize)>>,

    at_block: Option<BlockId>,
    at_inst: usize,
}

impl<R: Register + 'static + core::fmt::Debug> RegAlloc<R> for GraphAlloc<R> {
    fn new_sized(_size: usize) -> Self {
        Self {
            first_def: HashMap::new(),
            last_uses: HashMap::new(),

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

        self.at_block = None;
        self.at_inst = 0;
    }

    fn prologue_end(&mut self) {
        self.at_block = Some(BlockId(0));
        self.at_inst = 0;
    }

    fn define(&mut self, vr: VReg<R>) {
        self.first_def.entry(vr)
            .and_modify(|_| panic!("redef {vr}"))
            .or_insert((self.at_block, self.at_inst));
    }

    fn add_use(&mut self, vr: VReg<R>) {
        self.last_uses.entry(vr)
            .or_default()
            .push((self.at_block, self.at_inst));
    }

    fn coalesce_move(&mut self, from: VReg<R>, to: VReg<R>) {
    }

    fn alloc_regs<I: Inst<Register = R>>(&mut self, alloc: &mut [VReg<R>], cfg: cfg::Cfg, gen: &VCodeGen<I>) {
        println!("{:?}", self.first_def);
        println!("{:?}", self.last_uses);

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

        println!("i {live_in:?}");
        println!("o {live_out:?}");

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

            for (j, uses) in self.last_uses.iter() {
                intg.entry(*j).or_default();

                for (ub, ui) in uses.iter() {
                    // continue if use bef defs too
                    if db != ub || di >= ui { continue; }

                    if i != j {
                        intg.get_mut(i).unwrap().insert(*j);
                        intg.get_mut(j).unwrap().insert(*i);
                    }
                }
            }
        }
        println!("{intg:?}");

        let mut color = HashMap::new();
        for (node, _) in intg.iter() {
            match node {
                VReg::VirtReal(v, r) => {
                    alloc[*v] = VReg::Real(*r);
                    if let Some((i, _)) = R::get_regs().iter().enumerate().find(|(_, r2)| r == *r2) {
                        color.insert(*node, i);
                    }
                },
                _ => {},
            }
        }

        for (node, edges) in intg.iter() {
            if matches!(node, VReg::VirtReal(..)) { continue; }

            color.insert(*node, (0..).find(|c| {
                edges.iter().find(|e| color.get(e).map_or(false, |e| e == c)).is_none()
            }).unwrap());
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
}
