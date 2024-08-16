use core::fmt;

use crate::*;
use arch::*;

pub mod graph;
// pub mod linear;

pub trait Register: Sized + Clone + Copy + fmt::Display + Eq + core::hash::Hash + TryFrom<usize> + Into<usize> {
    const REG_COUNT: usize;
    fn get_regs() -> &'static [Self];
}

pub trait RegAlloc<R: Register> where Self: Sized {
    fn new_sized(size: usize) -> Self;

    fn next_inst(&mut self);
    fn next_block(&mut self);
    fn next_fn(&mut self);
    fn prologue_end(&mut self) { self.next_block(); }

    fn define(&mut self, vr: VReg<R>);
    fn add_use(&mut self, vr: VReg<R>);
    fn coalesce_move(&mut self, from: VReg<R>, to: VReg<R>);

    fn alloc_regs<I: Inst<Register = R>>(&mut self, alloc: &mut [VReg<R>], cfg: cfg::Cfg, gen: &VCodeGen<I>);
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum VReg<R: Register> {
    Real(R),
    Spilled(usize),

    Virtual(usize),
    VirtReal(usize, R),
}

impl<R: Register> VReg<R> {
    pub fn force_in_reg(self, r: R) -> Self {
        match self {
            Self::Real(_) => Self::Real(r),
            Self::Spilled(_) => panic!("{self} can't be in a register"),

            Self::Virtual(v) | Self::VirtReal(v, _) => Self::VirtReal(v, r),
        }
    }
}

impl<R: Register> From<R> for VReg<R> {
    fn from(value: R) -> Self {
        Self::Real(value)
    }
}

impl<R: Register> fmt::Display for VReg<R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Real(r) => r.fmt(f),
            Self::Spilled(v) => write!(f, "s{v}"),

            Self::Virtual(v) => write!(f, "v{v}"),
            Self::VirtReal(v, r) => write!(f, "v{v}_{r}"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct VRegAlloc(pub(crate) usize);

impl VRegAlloc {
    pub fn alloc_virtual<R: Register>(&mut self) -> VReg<R> {
        let id = self.0;
        self.0 += 1;
        VReg::Virtual(id)
    }
}
