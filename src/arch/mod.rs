use core::fmt;
use std::ops::Range;
use crate::{Function, *};
use regalloc2::*;

#[cfg(any(feature = "arch-aarch64", all(feature = "arch-native", target_arch = "aarch64")))]
pub mod aarch64;
#[cfg(feature = "arch-urcl")]
pub mod urcl;
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
    fn is_branch(&self) -> bool;
    fn is_ret(&self) -> bool;
    fn clobbers(&self) -> PRegSet;
    fn operands<'a>(&'a self) -> &'a [Operand];
    fn spill_size(rc: RegClass) -> usize;

    fn reg_env() -> MachineEnv;

    fn apply_mandatory_transforms(vf: &mut VFunction<Self>, ra: Output);

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
}

pub struct VCode<'a, I: Inst> {
    funcs: Vec<VFunction<'a, I>>,
}

pub struct VFunction<'a, I: Inst> {
    linkage: Linkage,
    name: &'a str,
    value_to_vreg: HashMap<ValueId, VReg>,

    body: Vec<I>,
    body_range: Vec<Range<usize>>,
    vreg_alloc: VRegAlloc,

    orig_fn: &'a Function<'a>,
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
        }
    }

    pub fn push_inst(&mut self, i: I) {
        let f = &mut self.vcode.funcs[self.at_fn.unwrap()];
        f.body.push(i);
        if let Some(r) = f.body_range.last_mut() {
            r.end += 1;
        }
    }

    pub fn get_function(&self) -> &'a VFunction<I> {
        &self.vcode.funcs[self.at_fn.unwrap()]
    }

    pub fn get_function_mut(&mut self) -> &'a mut VFunction<I> {
        &mut self.vcode.funcs[self.at_fn.unwrap()]
    }

    pub fn get_value_vreg(&mut self, v: ValueId) -> VReg {
        let f = &mut self.vcode.funcs[self.at_fn.unwrap()];

        if let Some(v) = f.value_to_vreg.get(&v) {
            *v
        } else {
            let vr = f.vreg_alloc.alloc_virtual();
            f.value_to_vreg.insert(v, vr);
            vr
        }
    }

    pub fn get_arg_vreg(&mut self, a: usize) -> VReg {
        self.get_value_vreg(ValueId(a))
    }
}

impl<'a, I: Inst> VCode<'a, I> {
    pub fn generate<S: InstSelector<Instruction = I>>(
        ir: &'a Program<'a>,
        mut sel: S,
    ) -> Self {
        let mut gen = VCodeGen::new();
        let menv = I::reg_env();

        for (fi, f) in ir.functions.iter().enumerate() {
            gen.at_fn = Some(fi);
            gen.vcode.funcs.push(VFunction {
                linkage: f.linkage,
                name: f.name,
                value_to_vreg: HashMap::new(),
                body: vec![],
                body_range: vec![],
                vreg_alloc: VRegAlloc(0),
                orig_fn: f,
            });

            if f.linkage == Linkage::External { continue; }

            sel.select_pre_fn(&mut gen, &f.arguments);

            for b in f.blocks.iter() {
                let f = gen.vcode.funcs.last_mut().unwrap();
                let l = f.body.len();
                f.body_range.push(l..l);

                for i in b.insts.iter() {
                    sel.select_inst(&mut gen, i);
                }
                sel.select_term(&mut gen, &b.term);
            }

            let out = regalloc2::run(gen.get_function(), &menv, &RegallocOptions::default()).unwrap();
            I::apply_mandatory_transforms(gen.vcode.funcs.last_mut().unwrap(), out);
        }

        for _ in (0..PEEPHOLE_OPT_ITERS).take_while(|_| gen.vcode.apply_peephole_once()) {}

        gen.vcode
    }

    fn apply_peephole_once(&mut self) -> bool {
        let mut changed = false;

        /* for f in self.funcs.iter_mut() {
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
        } */

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
        for (ii, i) in self.body.iter().enumerate() {
            // writeln!(f, ".L{bi}:")?;

            writeln!(f, "{i}")?;
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

#[derive(Clone, Debug)]
pub struct VRegAlloc(pub(crate) usize);

impl VRegAlloc {
    pub fn alloc_virtual(&mut self) -> VReg {
        let id = self.0;
        self.0 += 1;
        VReg::new(id, RegClass::Int)
    }
}

impl<I: Inst> regalloc2::Function for VFunction<'_, I> {
    fn num_insts(&self) -> usize {
        self.body.len()
    }

    fn num_blocks(&self) -> usize {
        self.body_range.len()
    }

    fn entry_block(&self) -> Block {
        Block(0)
    }

    fn block_insns(&self, block: Block) -> InstRange {
        let r = &self.body_range[block.0 as usize];
        InstRange::forward(Inst(r.start as _), Inst(r.end as _))
    }

    fn block_succs(&self, block: Block) -> &[Block] {
        &self.orig_fn.blocks[block.0 as usize].succ
    }

    fn block_preds(&self, block: Block) -> &[Block] {
        todo!()
    }

    fn block_params(&self, block: Block) -> &[VReg] {
        todo!()
    }

    fn is_ret(&self, insn: regalloc2::Inst) -> bool {
        self.body[insn.0 as usize].is_ret()
    }

    fn is_branch(&self, insn: regalloc2::Inst) -> bool {
        self.body[insn.0 as usize].is_branch()
    }

    fn branch_blockparams(&self, block: Block, insn: regalloc2::Inst, succ_idx: usize) -> &[VReg] {
        todo!()
    }

    fn inst_operands(&self, insn: regalloc2::Inst) -> &[Operand] {
        self.body[insn.0 as usize].operands()
    }

    fn inst_clobbers(&self, insn: regalloc2::Inst) -> PRegSet {
        self.body[insn.0 as usize].clobbers()
    }

    fn num_vregs(&self) -> usize {
        self.vreg_alloc.0
    }

    fn spillslot_size(&self, regclass: RegClass) -> usize {
        I::spill_size(regclass)
    }
}
