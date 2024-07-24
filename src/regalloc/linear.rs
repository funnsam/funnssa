use std::collections::HashSet;

use super::*;

type LiveRange = core::ops::Range<usize>;

pub struct LinearAlloc<R: Register> {
    reg_v: Vec<LinearReg<R>>,
    reg_r: Vec<LinearReg<R>>,
    range: LiveRange,
}

#[derive(Clone)]
struct LinearReg<R: Register> {
    uses: usize,
    range: LiveRange,
    try_coalesce: Option<VReg<R>>,
}

impl<R: Register + 'static> RegAlloc<R> for LinearAlloc<R> {
    fn new_sized(size: usize) -> Self {
        Self {
            reg_v: vec![LinearReg {
                uses: 0,
                range: usize::MAX..usize::MAX,
                try_coalesce: None,
            }; size],
            reg_r: vec![LinearReg {
                uses: 0,
                range: usize::MAX..usize::MAX,
                try_coalesce: None,
            }; size],
            range: 0..0,
        }
    }

    fn next(&mut self) {
        self.range.end += 1;
    }

    fn define(&mut self, vr: VReg<R>) {
        let lr = match vr {
            VReg::Real(r) => &mut self.reg_r[r.into()],
            VReg::Virtual(v) => &mut self.reg_v[v],
            VReg::Spilled(_) => return,
        };

        if lr.range.start == lr.range.end && lr.range.start == usize::MAX {
            lr.range.start = self.range.end;
        }

        lr.range.end = self.range.end;
    }

    fn add_use(&mut self, vr: VReg<R>) {
        let lr = match vr {
            VReg::Real(r) => &mut self.reg_r[r.into()],
            VReg::Virtual(v) => &mut self.reg_v[v],
            VReg::Spilled(_) => return,
        };

        if lr.range.start == lr.range.end && lr.range.start == usize::MAX {
            lr.range.start = self.range.end;
        }

        lr.range.end = self.range.end;
        lr.uses += 1;
    }

    fn coalesce_move(&mut self, from: VReg<R>, to: VReg<R>) {
        let lr = match from {
            VReg::Real(r) => &mut self.reg_r[r.into()],
            VReg::Virtual(v) => &mut self.reg_v[v],
            VReg::Spilled(_) => return,
        };

        lr.try_coalesce = Some(to);
    }

    fn alloc_regs(&mut self, alloc: &mut [VReg<R>]) {
        let mut regs = R::get_regs().to_vec();
        let mut using_regs = Vec::new();
        let mut spilled = HashSet::new();
        let mut spill_av = Vec::new();

        for time in self.range.clone() {
            for (ri, reg) in self.reg_r.iter().enumerate() {
                if reg.range.start == time {
                    regs.retain(|e| (*e).into() != ri);
                    using_regs.push(ri.try_into().ok().unwrap());
                }

                if reg.range.end == time {
                    using_regs.retain(|e: &R| (*e).into() != ri);
                    regs.push(ri.try_into().ok().unwrap());
                }
            }

            for (ri, reg) in self.reg_v.iter().enumerate() {
                if reg.range.start == time {
                    alloc[ri] = if let Some(r) = regs.pop() {
                        VReg::Real(r)
                    } else {
                        let s = if let Some(s) = spill_av.pop() {
                            s
                        } else {
                            spilled.len()
                        };
                        spilled.insert(s);
                        VReg::Spilled(s)
                    }
                }

                if reg.range.end == time {
                    match alloc[ri] {
                        VReg::Real(r) => regs.push(r),
                        VReg::Spilled(s) => spill_av.push(s),
                        VReg::Virtual(_) => unreachable!(),
                    }
                }
            }
        }
    }

    fn clear(&mut self) {
        self.reg_r.fill(LinearReg {
            uses: 0,
            range: usize::MAX..usize::MAX,
            try_coalesce: None,
        });
        self.range.start = self.range.end;
    }
}
