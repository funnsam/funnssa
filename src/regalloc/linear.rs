use std::collections::HashSet;

use super::*;

type LiveRange = core::ops::Range<usize>;

pub struct LinearAlloc<R: Register> {
    registers: Vec<LinearReg<R>>,
    position: usize,
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
            registers: vec![LinearReg {
                uses: 0,
                range: usize::MAX..usize::MAX,
                try_coalesce: None,
            }; size],
            position: 0,
        }
    }

    fn next(&mut self) {
        self.position += 1;
    }

    fn define(&mut self, vr: VReg<R>) {
        if let VReg::Virtual(vr) = vr {
            let lr = &mut self.registers[vr];
            if lr.range.start == lr.range.end && lr.range.start == usize::MAX {
                lr.range.start = self.position;
            }

            lr.range.end = self.position;
        }
    }

    fn add_use(&mut self, vr: VReg<R>) {
        if let VReg::Virtual(vr) = vr {
            let lr = &mut self.registers[vr];
            if lr.range.start == lr.range.end && lr.range.start == usize::MAX {
                lr.range.start = self.position;
            }

            lr.range.end = self.position;
            lr.uses += 1;
        }
    }

    fn coalesce_move(&mut self, from: VReg<R>, to: VReg<R>) {
        if let VReg::Virtual(from) = from {
            self.registers[from].try_coalesce = Some(to);
        }
    }

    fn alloc_regs(mut self) -> Vec<VReg<R>> {
        let mut alloc = vec![VReg::Virtual(0); self.registers.len()];
        let mut regs = R::get_regs().to_vec();
        let mut spilled = HashSet::new();
        let mut spill_av = Vec::new();

        for time in 0..self.position {
            for (ri, reg) in self.registers.iter_mut().enumerate() {
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

        alloc
    }
}
