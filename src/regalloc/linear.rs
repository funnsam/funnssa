use std::collections::{HashMap, HashSet};

use super::*;

type LiveRange = core::ops::Range<usize>;

#[deprecated]
pub struct LinearAlloc<R: Register> {
    reg_v: Vec<LinearReg<R>>,
    reg_r: Vec<LinearReg<R>>,
    reg_s: HashMap<usize, LinearReg<R>>,
    range: LiveRange,
}

#[derive(Clone)]
struct LinearReg<R: Register> {
    uses: usize,
    range: LiveRange,
    try_coalesce: Option<VReg<R>>,
}

impl<R: Register> Default for LinearReg<R> {
    fn default() -> Self {
        Self {
            uses: 0,
            range: usize::MAX..usize::MAX,
            try_coalesce: None,
        }
    }
}

impl<R: Register> LinearAlloc<R> {
    fn get_vreg_mut(&mut self, vr: VReg<R>) -> &mut LinearReg<R> {
        match vr {
            VReg::Real(r) => &mut self.reg_r[r.into()],
            VReg::Virtual(v) => &mut self.reg_v[v],
            VReg::Spilled(s) => self.reg_s.entry(s).or_insert(LinearReg::default()),
        }
    }
}

impl<R: Register + 'static> RegAlloc<R> for LinearAlloc<R> {
    fn new_sized(size: usize) -> Self {
        Self {
            reg_v: vec![LinearReg::default(); size],
            reg_r: vec![LinearReg::default(); R::REG_COUNT],
            reg_s: HashMap::new(),
            range: 0..0,
        }
    }

    fn next_inst(&mut self) {
        self.range.end += 1;
    }

    fn next_block(&mut self) {}

    fn next_fn(&mut self) {
        self.reg_r.fill(LinearReg::default());
        self.reg_s.clear();
        self.range.start = self.range.end;
    }

    fn define(&mut self, vr: VReg<R>) {
        let end = self.range.end;
        let lr = self.get_vreg_mut(vr);

        if lr.range.start == lr.range.end && lr.range.start == usize::MAX {
            lr.range.start = end;
        }

        lr.range.end = end;
    }

    fn add_use(&mut self, vr: VReg<R>) {
        let end = self.range.end;
        let lr = self.get_vreg_mut(vr);

        if lr.range.start == lr.range.end && lr.range.start == usize::MAX {
            lr.range.start = end;
        }

        lr.range.end = end;
        lr.uses += 1;
    }

    fn coalesce_move(&mut self, from: VReg<R>, to: VReg<R>) {
        let lr = self.get_vreg_mut(from);

        lr.try_coalesce = Some(to);
    }

    fn alloc_regs(&mut self, alloc: &mut [VReg<R>]) {
        let mut regs = R::get_regs().to_vec();
        let mut using_regs = Vec::new();
        let mut spilled = HashSet::new();
        let mut spill_av = Vec::new();

        for time in self.range.clone() {
            for (ri, reg) in self.reg_r.iter().enumerate() {
                let r = ri.try_into().ok().unwrap();
                if reg.range.start == time && regs.contains(&r) {
                    regs.retain(|e| (*e).into() != ri);
                    using_regs.push(r);
                }

                if reg.range.end == time && using_regs.contains(&r) {
                    using_regs.retain(|e: &R| (*e).into() != ri);
                    regs.push(r);
                }
            }

            for (ri, reg) in self.reg_s.iter() {
                if reg.range.start == time {
                    spill_av.retain(|e| *e != *ri);
                    spilled.insert(*ri);
                }

                // if reg.range.end == time && using_regs.contains(&r) {
                //     using_regs.retain(|e: &R| (*e).into() != ri);
                //     regs.push(r);
                // }
            }

            for (ri, reg) in self.reg_v.iter().enumerate() {
                if reg.range.start == time {
                    alloc[ri] = regs.pop().map_or_else(|| {
                        let s = spill_av.pop().unwrap_or(spilled.len());
                        spilled.insert(s);
                        VReg::Spilled(s)
                    }, VReg::Real);
                }

                if reg.range.end == time {
                    match alloc[ri] {
                        VReg::Real(r) => regs.push(r),
                        VReg::Spilled(s) => {
                            spill_av.push(s);
                            spilled.remove(&s);
                        },
                        VReg::Virtual(_) => unreachable!(),
                    }
                }
            }
        }
    }
}
