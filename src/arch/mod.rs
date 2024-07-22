use core::fmt;
use crate::{*, regalloc::*};

pub mod urcl;

pub trait InstSelector: Sized {
    type Instruction: Inst;

    fn new() -> Self;

    fn select_pre_fn(&mut self, gen: &mut VCodeGen<Self::Instruction>);
    fn select_inst(&mut self, gen: &mut VCodeGen<Self::Instruction>, inst: &Instruction);
    fn select_term(&mut self, gen: &mut VCodeGen<Self::Instruction>, term: &Terminator);

    fn peephole_opt(
        &mut self,
        area: &[Self::Instruction],
        bi: Option<BlockId>,
    ) -> Option<(Vec<Self::Instruction>, usize)> {
        _ = (area, bi);
        None
    }
}

pub trait Inst: Sized + fmt::Display {
    type Register: Register;

    fn register_regalloc(&self, ra: &mut impl RegAlloc<Self::Register>);
    fn apply_alloc(&mut self, ra: &[VReg<Self::Register>]);
}

pub struct VCodeGen<'a, I: Inst> {
    vcode: VCode<'a, I>,

    at_fn: Option<usize>,
    pub vreg_alloc: VRegAlloc,
}

pub struct VCode<'a, I: Inst> {
    funcs: Vec<VFunction<'a, I>>,
}

pub struct VFunction<'a, I: Inst> {
    name: &'a str,
    value_to_vreg: HashMap<ValueId, VReg<I::Register>>,

    pre: Vec<I>,
    body: Vec<Vec<I>>,
}

impl<'a, I: Inst> Default for VCodeGen<'a, I> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, I: Inst> VCodeGen<'a, I> {
    pub const fn new() -> Self {
        Self {
            vcode: VCode {
                funcs: Vec::new(),
            },

            at_fn: None,
            vreg_alloc: VRegAlloc(0),
        }
    }

    pub fn push_inst(&mut self, i: I) {
        let f = &mut self.vcode.funcs[self.at_fn.unwrap()];
        f.body.last_mut().unwrap_or(&mut f.pre).push(i);
    }

    pub fn get_function(&self) -> &'a VFunction<I> {
        &self.vcode.funcs[self.at_fn.unwrap()]
    }

    pub fn get_function_mut(&mut self) -> &'a mut VFunction<I> {
        &mut self.vcode.funcs[self.at_fn.unwrap()]
    }

    pub fn get_value_vreg(&mut self, v: ValueId) -> VReg<I::Register> {
        if let Some(v) = self.get_function().value_to_vreg.get(&v) {
            *v
        } else {
            let vr = self.vreg_alloc.alloc_virtual();
            self.vcode.funcs[self.at_fn.unwrap()].value_to_vreg.insert(v, vr);
            vr
        }
    }
}

impl<'a, I: Inst> VCode<'a, I> {
    pub fn generate<S: InstSelector<Instruction = I>, A: RegAlloc<I::Register>>(ir: Program<'a>) -> Self {
        let mut gen = VCodeGen::new();
        let mut sel = S::new();
        for (fi, f) in ir.functions.iter().enumerate() {
            gen.at_fn = Some(fi);
            gen.vcode.funcs.push(VFunction {
                name: f.name,
                value_to_vreg: HashMap::new(),
                pre: vec![],
                body: vec![],
            });
            sel.select_pre_fn(&mut gen);

            for b in f.blocks.iter() {
                gen.vcode.funcs.last_mut().unwrap().body.push(vec![]);
                for i in b.insts.iter() {
                    sel.select_inst(&mut gen, i);
                }
                sel.select_term(&mut gen, &b.term);
            }
        }

        let mut ra = A::new_sized(gen.vreg_alloc.0);
        for f in gen.vcode.funcs.iter() {
            for i in f.pre.iter() {
                i.register_regalloc(&mut ra);
                ra.next();
            }

            for b in f.body.iter() {
                for i in b.iter() {
                    i.register_regalloc(&mut ra);
                ra.next();
                }
            }
        }
        let ra = ra.alloc_regs();
        for f in gen.vcode.funcs.iter_mut() {
            for i in f.pre.iter_mut() {
                i.apply_alloc(&ra);
            }

            for b in f.body.iter_mut() {
                for i in b.iter_mut() {
                    i.apply_alloc(&ra);
                }
            }
        }

        for f in gen.vcode.funcs.iter_mut() {
            let mut p = |inst: &mut Vec<I>, bi| {
                let mut h = 0;
                while h < inst.len() {
                    if let Some((repl, del)) = sel.peephole_opt(&inst[h..], bi) {
                        for _ in 0..del {
                            inst.remove(h);
                        }

                        for r in repl {
                            inst.insert(h, r);
                            h += 1;
                        }
                    } else {
                        h += 1;
                    }
                }
            };

            p(&mut f.pre, None);
            for (bi, b) in f.body.iter_mut().enumerate() {
                p(b, Some(BlockId(bi)));
            }
        }

        gen.vcode
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Location {
    Function(usize),
    Block(usize),
}

impl From<BlockId> for Location {
    fn from(val: BlockId) -> Self {
        Self::Block(val.0)
    }
}

impl<I: Inst> fmt::Display for VCode<'_, I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for n in self.funcs.iter() {
            n.fmt(f)?;
        }

        Ok(())
    }
}

impl<I: Inst> fmt::Display for VFunction<'_, I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "F{}:", self.name)?;
        for i in self.pre.iter() {
            i.fmt(f)?;
        }
        for (bi, b) in self.body.iter().enumerate() {
            writeln!(f, ".L{bi}:")?;
            for i in b.iter() {
                i.fmt(f)?;
            }
        }
        Ok(())
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Function(i) => write!(f, "F{i}"),
            Self::Block(b) => write!(f, ".L{b}"),
        }
    }
}
