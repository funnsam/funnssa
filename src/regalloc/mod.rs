use core::fmt;

pub mod linear;

pub trait Register: Sized + Clone + Copy + fmt::Display + Eq + core::hash::Hash {
    fn get_regs() -> &'static [Self];
}

pub trait RegAlloc<R: Register> where Self: Sized {
    fn new_sized(size: usize) -> Self;
    fn next(&mut self);

    fn define(&mut self, vr: VReg<R>);
    fn add_use(&mut self, vr: VReg<R>);
    fn coalesce_move(&mut self, from: VReg<R>, to: VReg<R>);
    fn alloc_regs(self) -> Vec<VReg<R>>;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum VReg<R: Register> {
    Virtual(usize),
    Real(R),
    Spilled(usize),
}

impl<R: Register> fmt::Display for VReg<R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Virtual(v) => write!(f, "v{v}"),
            Self::Real(r) => r.fmt(f),
            Self::Spilled(v) => write!(f, "s{v}"),
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
