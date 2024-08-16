use core::fmt;
use crate::{*, regalloc::*};

#[cfg(any(feature = "arch-aarch64", all(feature = "arch-native", target_arch = "aarch64")))]
pub mod aarch64;
// #[cfg(feature = "arch-urcl")]
// pub mod urcl;
#[cfg(any(feature = "arch-x86_64", all(feature = "arch-native", target_arch = "x86_64")))]
pub mod x86_64;

const PEEPHOLE_OPT_ITERS: usize = 2048;

pub trait InstSelector: Sized {
    type Instruction: Inst;

    fn select_pre_fn(&mut self, gen: &mut VCodeGen<Self::Instruction>, args: &[ValueType]);
    fn select_inst(&mut self, gen: &mut VCodeGen<Self::Instruction>, inst: &Instruction);
    fn select_term(&mut self, gen: &mut VCodeGen<Self::Instruction>, term: &Terminator);
}

pub trait Inst: Sized {
    type Register: Register;

    fn register_regalloc(&self, ra: &mut impl RegAlloc<Self::Register>);
    fn apply_alloc(&mut self, ra: &[VReg<Self::Register>]);

    fn apply_mandatory_transforms(vcode: &mut VCode<Self>);

    fn peephole_opt(
        area: &[Self],
        bi: Option<BlockId>,
    ) -> Option<(Vec<Self>, usize)> {
        _ = (area, bi);
        None
    }

    fn emit_assembly<W: std::io::Write>(f: &mut W, vcode: &VCode<Self>) -> std::io::Result<()>;
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
    linkage: Linkage,
    name: &'a str,
    value_to_vreg: HashMap<ValueId, VReg<I::Register>>,

    pre: Vec<I>,
    body: Vec<Vec<I>>,

    original_fn: &'a Function<'a>,
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
        self.get_value_vreg_no_add(v).unwrap_or_else(|| {
            let vr = self.vreg_alloc.alloc_virtual();
            self.vcode.funcs[self.at_fn.unwrap()].value_to_vreg.insert(v, vr);
            vr
        })
    }

    pub fn get_value_vreg_no_add(&self, v: ValueId) -> Option<VReg<I::Register>> {
        self.get_function().value_to_vreg.get(&v).copied()
    }

    pub fn get_arg_vreg(&mut self, a: usize) -> VReg<I::Register> {
        self.get_value_vreg(ValueId(a))
    }

    pub fn get_bb_arg_dest(&'a self, id: BlockId) -> &'a [Value] {
        &self.get_function().original_fn.blocks[id.0].args
    }
}

impl<'a, I: Inst> VCode<'a, I> {
    pub fn generate<S: InstSelector<Instruction = I>, A: RegAlloc<I::Register>>(
        ir: &'a Program<'a>,
        mut sel: S,
    ) -> Self {
        let mut gen = VCodeGen::new();

        for (fi, f) in ir.functions.iter().enumerate() {
            gen.at_fn = Some(fi);
            gen.vcode.funcs.push(VFunction {
                linkage: f.linkage,
                name: f.name,
                value_to_vreg: HashMap::new(),

                pre: vec![],
                body: vec![],

                original_fn: f,
            });

            if f.linkage == Linkage::External { continue; }

            sel.select_pre_fn(&mut gen, &f.arguments);

            for b in f.blocks.iter() {
                gen.vcode.funcs.last_mut().unwrap().body.push(vec![]);
                for i in b.insts.iter() {
                    sel.select_inst(&mut gen, i);
                }
                sel.select_term(&mut gen, &b.term);
            }
        }

        let mut ra = A::new_sized(gen.vreg_alloc.0);
        let mut alloc = vec![VReg::Virtual(0); gen.vreg_alloc.0];

        for (fi, f) in gen.vcode.funcs.iter().enumerate() {
            for i in f.pre.iter() {
                i.register_regalloc(&mut ra);
                ra.next_inst();
            }

            ra.prologue_end();

            for b in f.body.iter() {
                for i in b.iter() {
                    i.register_regalloc(&mut ra);
                    ra.next_inst();
                }

                ra.next_block();
            }

            ra.alloc_regs(&mut alloc, cfg::Cfg::new(&ir.functions[fi]), &gen);
            ra.next_fn();
        }

        for f in gen.vcode.funcs.iter_mut() {
            for i in f.pre.iter_mut() {
                i.apply_alloc(&alloc);
            }

            for b in f.body.iter_mut() {
                for i in b.iter_mut() {
                    i.apply_alloc(&alloc);
                }
            }
        }

        I::apply_mandatory_transforms(&mut gen.vcode);

        for _ in (0..PEEPHOLE_OPT_ITERS).take_while(|_| gen.vcode.apply_peephole_once()) {}

        gen.vcode
    }

    fn apply_peephole_once(&mut self) -> bool {
        let mut changed = false;

        for f in self.funcs.iter_mut() {
            let mut p = |inst: &mut Vec<I>, bi| {
                let mut h = 0;
                while h < inst.len() {
                    if let Some((repl, del)) = I::peephole_opt(&inst[h..], bi) {
                        for _ in 0..del {
                            inst.remove(h);
                        }

                        for r in repl {
                            inst.insert(h, r);
                            h += 1;
                        }

                        changed = true;
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

        changed
    }

    pub fn emit_assembly<W: std::io::Write>(&self, f: &mut W) -> std::io::Result<()> {
        I::emit_assembly(f, self)
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

impl From<FuncId> for Location {
    fn from(val: FuncId) -> Self {
        Self::Function(val.0)
    }
}

impl<I: Inst + fmt::Display> fmt::Display for VCode<'_, I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for n in self.funcs.iter() {
            n.fmt(f)?;
        }

        Ok(())
    }
}

impl<I: Inst + fmt::Display> fmt::Display for VFunction<'_, I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "F{}:", self.name)?;
        for i in self.pre.iter() {
            writeln!(f, "{i}")?;
        }
        for (bi, b) in self.body.iter().enumerate() {
            writeln!(f, ".L{bi}:")?;

            for i in b.iter() {
                writeln!(f, "{i}")?;
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
