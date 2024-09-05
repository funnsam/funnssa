//! # Notes
//! - Only SystemV ABI is supported for now
//! - AT&T syntax is outputed

use core::fmt;
use crate::arch::*;
use strum::EnumMessage;

const CALLER_SAVE: &[X64Reg] = &[
    X64Reg::Ax,
    X64Reg::Di,
    X64Reg::Si,
    X64Reg::Dx,
    X64Reg::Cx,
    X64Reg::R8,
    X64Reg::R9,
];

const CALLEE_SAVE: &[X64Reg] = &[
    X64Reg::Bx,
    X64Reg::R12,
    X64Reg::R13,
    X64Reg::R14,
    X64Reg::R15,
];

const FRAME_SIZE_MARKER: i64 = i64::MAX;

pub type X64VReg = VReg<X64Reg>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, strum::Display, strum::EnumMessage)]
pub enum X64BitSize {
    #[strum(serialize = "q", message = "r")]
    Quad,
    #[strum(serialize = "l", message = "e")]
    Long,
    #[strum(serialize = "w")]
    Word,
    #[strum(serialize = "b")]
    Byte,
}

impl X64BitSize {
    pub fn from_bit_size(b: usize) -> Self {
        #[allow(clippy::match_overlapping_arm)]
        match b {
            ..=8 => Self::Byte,
            ..=16 => Self::Word,
            ..=32 => Self::Long,
            ..=64 => Self::Quad,
            _ => todo!(">64 bits ({b})"),
        }
    }

    pub fn from_vt(vt: &ValueType) -> Self {
        match vt {
            ValueType::Int(s) => Self::from_bit_size(*s),
            ValueType::Ptr => Self::Quad,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, strum::Display, strum::EnumCount, strum::FromRepr)]
#[strum(serialize_all = "lowercase")]
pub enum X64Reg {
    Ax,
    Bx,
    Cx,
    Sp,
    Bp,
    Di,
    Si,
    Dx,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl Register for X64Reg {
    const REG_COUNT: usize = <Self as strum::EnumCount>::COUNT;

    fn get_regs() -> &'static [Self] {
        &[
            Self::Ax,
            Self::Di,
            Self::Si,
            Self::Dx,
            Self::Cx,
            Self::R8,
            Self::R9,

            Self::Bx,
            Self::R12,
            Self::R13,
            Self::R14,
            Self::R15,
            Self::Di,
        ]
    }
}

impl TryFrom<usize> for X64Reg {
    type Error = ();

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        Self::from_repr(value).ok_or(())
    }
}

impl From<X64Reg> for usize {
    fn from(value: X64Reg) -> Self {
        value as _
    }
}

#[derive(Clone)]
pub enum X64Inst {
    Mov(X64BitSize, X64VReg, X64VReg),
    MovI(X64BitSize, i64, X64VReg),
    Cmp(X64BitSize, X64VReg, X64VReg),
    Int2(X64BitSize, X64IntOp, X64VReg, X64VReg),
    Int2I(X64BitSize, X64IntOp, i64, X64VReg),
    Push(X64BitSize, X64VReg),
    Pop(X64BitSize, X64VReg),
    CSet(X64Cond, X64VReg),
    Jmp(Location),
    Jcc(X64Cond, Location),
    Ret,
    Leave,

    BlockArgMov(X64BitSize, X64VReg, X64VReg),
    SavingMov(X64BitSize, X64VReg, X64VReg),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, strum::Display)]
#[strum(serialize_all = "lowercase")]
pub enum X64Cond {
    E,
    Ne,
    B,
    Ae,
    Be,
    A,
    L,
    Le,
    G,
    Ge,
    Nz,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, strum::Display)]
#[strum(serialize_all = "lowercase")]
pub enum X64IntOp {
    Add,
    Sub,
    IMul,
    And,
    Or,
    Xor,
}

impl TryFrom<IntOp> for X64Cond {
    type Error = ();

    fn try_from(value: IntOp) -> Result<Self, Self::Error> {
        match value {
            IntOp::Eq => Ok(Self::E),
            IntOp::Ne => Ok(Self::Ne),
            IntOp::ULt => Ok(Self::B),
            IntOp::UGe => Ok(Self::Ae),
            IntOp::ULe => Ok(Self::Be),
            IntOp::UGt => Ok(Self::A),
            IntOp::SLt => Ok(Self::L),
            IntOp::SLe => Ok(Self::Le),
            IntOp::SGt => Ok(Self::G),
            IntOp::SGe => Ok(Self::Ge),
            _ => Err(()),
        }
    }
}

impl TryFrom<IntOp> for X64IntOp {
    type Error = ();

    fn try_from(value: IntOp) -> Result<Self, Self::Error> {
        match value {
            IntOp::Add => Ok(Self::Add),
            IntOp::Sub => Ok(Self::Sub),
            // IntOp::Mul => Ok(Self::IMul),
            IntOp::And => Ok(Self::And),
            IntOp::Or => Ok(Self::Or),
            IntOp::Xor => Ok(Self::Xor),
            _ => Err(()),
        }
    }
}

impl Inst for X64Inst {
    type Register = X64Reg;

    fn register_regalloc(&self, ra: &mut impl RegAlloc<Self::Register>) {
        match self {
            Self::Mov(_, s, d) => {
                ra.add_use(*s);
                ra.define(*d);
                ra.coalesce_move(*s, *d);
            },
            Self::BlockArgMov(_, s, d) => {
                ra.add_use(*s);
                ra.coalesce_move(*s, *d);
            },
            Self::SavingMov(_, s, d) => {
                ra.add_use(*s);
                ra.define(*d);
                ra.coalesce_move(*s, *d);
                ra.prioritize(*s, -10);
            },
            Self::MovI(_, _, d) => {
                ra.define(*d);
            },
            Self::CSet(_, d) => {
                ra.define(*d);
            },
            Self::Cmp(_, a, b) => {
                ra.add_use(*a);
                ra.add_use(*b);
            },
            Self::Int2(_, _, s, d) => {
                ra.add_use(*s);
                ra.add_use(*d);
            },
            Self::Int2I(_, _, _, d) => {
                ra.add_use(*d);
                ra.define(*d);
            },
            Self::Push(_, r) => {
                ra.add_use(*r);
            },
            Self::Pop(_, r) => {
                ra.define(*r);
            },
            Self::Jmp(_) | Self::Jcc(..) | Self::Ret | Self::Leave => {},
        }
    }

    fn apply_alloc(&mut self, ra: &[VReg<Self::Register>]) {
        let apply = |r: &mut X64VReg| match r {
            VReg::Virtual(v) | VReg::VirtReal(v, _) => {
                *r = ra[*v];
            },
            _ => {},
        };

        match self {
            Self::Mov(_, s, d) | Self::BlockArgMov(_, s, d) | Self::SavingMov(_, s, d) => {
                apply(s);
                apply(d);
            },
            Self::MovI(_, _, d) => {
                apply(d);
            },
            Self::CSet(_, d) => {
                apply(d);
            },
            Self::Cmp(_, a, b) => {
                apply(a);
                apply(b);
            },
            Self::Int2(_, _, s, d) => {
                apply(s);
                apply(d);
                apply(d);
            },
            Self::Int2I(_, _, _, d) => {
                apply(d);
            },
            Self::Push(_, r) => {
                apply(r);
            },
            Self::Pop(_, r) => {
                apply(r);
            },
            Self::Jmp(_) | Self::Jcc(..) | Self::Ret | Self::Leave => {},
        }
    }

    fn apply_mandatory_transforms(vcode: &mut VCode<Self>) {
        for f in vcode.funcs.iter_mut() {
            let mut frame_size = 0;
            let mut unspill = |inst: &mut Vec<Self>| {
                let mut i = 0;
                while i < inst.len() {
                    let mut d_spilled = None;
                    let mut a_spilled = None;
                    let mut b_spilled = None;
                    let mut ud = |d: &mut X64VReg| if let VReg::Spilled(s) = d {
                        d_spilled = Some(*s);
                    };
                    let mut ua = |a: &mut X64VReg| if let VReg::Spilled(s) = a {
                        a_spilled = Some(*s);
                    };
                    let mut ub = |b: &mut X64VReg| if let VReg::Spilled(s) = b {
                        b_spilled = Some(*s);
                    };

                    match &mut inst[i] {
                        Self::Mov(_, s, d) => {
                            ua(s);
                            ud(d);
                        },
                        Self::BlockArgMov(t, s, d) | Self::SavingMov(t, s, d) => {
                            ua(s);
                            ud(d);
                            inst[i] = Self::Mov(*t, *s, *d);
                        },
                        Self::MovI(_, _, d) => {
                            ud(d);
                        },
                        Self::CSet(_, d) => {
                            ud(d);
                        },
                        Self::Cmp(_, a, b) => {
                            ua(a);
                            ub(b);
                        },
                        Self::Int2(_, _, s, d) => {
                            ua(s);
                            ud(d);
                        },
                        Self::Int2I(_, _, _, d) => {
                            ud(d);
                        },
                        Self::Push(_, r) => {
                            ud(r);
                        },
                        Self::Pop(_, r) => {
                            ua(r);
                        },
                        Self::Jmp(_) | Self::Jcc(..) | Self::Ret | Self::Leave => {},
                    }

                    if let Some(d) = d_spilled {
                        frame_size = frame_size.max(d + 1);
                    }
                    if let Some(a) = a_spilled {
                        frame_size = frame_size.max(a + 1);
                        if d_spilled.is_some() || b_spilled.is_some() {
                            todo!(">1 spilled");
                            // inst.insert(
                            //     i,
                            //     Self::Llod(SPILL_0.into(), UrclReg::Sp.into(), a as _),
                            // );
                        }
                    }
                    if let Some(b) = b_spilled {
                        frame_size = frame_size.max(b + 1);
                    }

                    i += 1;
                }
            };

            unspill(&mut f.pre);
            for b in f.body.iter_mut() {
                unspill(b);
            }

            let update = |inst: &mut Vec<Self>| for i in inst.iter_mut() {
                if let Self::Int2I(_, _, v, VReg::Real(X64Reg::Sp)) = i {
                    if *v == FRAME_SIZE_MARKER { *v = frame_size as i64 * 8 }
                }
            };

            update(&mut f.pre);
            for b in f.body.iter_mut() {
                update(b);
            }
        }
    }

    fn peephole_opt(
        area: &[Self],
        bi: Option<BlockId>,
    ) -> Option<(Vec<Self>, usize)> {
        match area {
            [Self::Mov(_, v, d), ..] if d == v => Some((vec![], 1)),
            [Self::Jmp(Location::Block(t))] if bi.map_or(false, |b| b.0 + 1 == *t) => Some((vec![], 1)),
            [Self::MovI(t, 0, d), ..] => Some((vec![Self::Int2(*t, X64IntOp::Xor, *d, *d)], 1)),
            [Self::CSet(c, r1), Self::Int2(X64BitSize::Byte, X64IntOp::Or, r2, r3), Self::Jcc(X64Cond::Nz, d), ..] if r1 == r2 && r2 == r3 => Some((
                vec![Self::CSet(*c, *r1), Self::Jcc(*c, *d)],
                3,
            )),
            [Self::Int2I(_, X64IntOp::Sub, 0, VReg::Real(X64Reg::Sp)), ..] => Some((vec![], 1)),
            _ => None,
        }
    }

    fn emit_assembly<W: std::io::Write>(f: &mut W, vcode: &VCode<Self>) -> std::io::Result<()> {
        for (i, n) in vcode.funcs.iter().enumerate() {
            writeln!(f, ".set F{i}, {}", n.name)?;
        }

        for n in vcode.funcs.iter() {
            if n.linkage == Linkage::External { continue; }

            if n.linkage == Linkage::Public {
                writeln!(f, ".global {}", n.name)?;
            }

            writeln!(f, "{}:", n.name)?;

            for i in n.pre.iter() {
                writeln!(f, "{i}")?;
            }

            for (bi, b) in n.body.iter().enumerate() {
                writeln!(f, ".L{bi}:")?;

                for i in b.iter() {
                    writeln!(f, "{i}")?;
                }
            }
        }

        Ok(())
    }
}

pub struct X64Selector {
    callee_save_vregs: [VReg<X64Reg>; CALLEE_SAVE.len()],
}

impl InstSelector for X64Selector {
    type Instruction = X64Inst;

    fn select_pre_fn(&mut self, gen: &mut VCodeGen<X64Inst>, args: &[ValueType]) {
        gen.push_inst(X64Inst::Push(X64BitSize::Quad, X64Reg::Bp.into()));
        gen.push_inst(X64Inst::Mov(X64BitSize::Quad, X64Reg::Sp.into(), X64Reg::Bp.into()));
        gen.push_inst(X64Inst::Int2I(X64BitSize::Quad, X64IntOp::Sub, FRAME_SIZE_MARKER, X64Reg::Sp.into()));

        for (ri, r) in CALLEE_SAVE.iter().enumerate() {
            let r = gen.vreg_alloc.alloc_virtual().force_in_reg(*r);
            let save = gen.vreg_alloc.alloc_virtual();
            self.callee_save_vregs[ri] = save;
            gen.push_inst(X64Inst::SavingMov(X64BitSize::Quad, r, save));
        }

        // TODO: fetch args
    }

    fn select_inst(&mut self, gen: &mut VCodeGen<X64Inst>, inst: &Instruction) {
        match inst {
            Instruction::Assign(d, v) => {
                let bits = X64BitSize::from_vt(&d.typ);
                let d = gen.get_value_vreg(d.id);
                gen.push_inst(X64Inst::MovI(bits, *v as i64, d));
            },
            Instruction::IntOp(op, d, a, b) if X64Cond::try_from(*op).is_ok() => {
                let bits = X64BitSize::from_bit_size(a.size);
                let d = gen.get_value_vreg(d.id);
                let a = gen.get_value_vreg(a.id);
                let b = gen.get_value_vreg(b.id);
                gen.push_inst(X64Inst::Cmp(bits, b, a));
                gen.push_inst(X64Inst::CSet((*op).try_into().unwrap(), d));
            },
            Instruction::IntOp(op, d, a, b) => {
                let bits = X64BitSize::from_bit_size(d.size);
                let d = gen.get_value_vreg(d.id);
                let a = gen.get_value_vreg(a.id);
                let b = gen.get_value_vreg(b.id);
                gen.push_inst(X64Inst::Mov(bits, a, d));
                gen.push_inst(X64Inst::Int2(bits, (*op).try_into().unwrap(), b, d));
            },
            Instruction::Copy(d, v) => {
                let bits = X64BitSize::from_vt(&d.typ);
                let d = gen.get_value_vreg(d.id);
                let v = gen.get_value_vreg(v.id);
                gen.push_inst(X64Inst::Mov(bits, v, d));
            },
            _ => todo!("{inst}"),
        }
    }

    fn select_term(&mut self, gen: &mut VCodeGen<X64Inst>, term: &Terminator) {
        match term {
            Terminator::UncondBranch(t) => {
                self.select_pre_jump(gen, t);
                gen.push_inst(X64Inst::Jmp(t.target.into()));
            },
            Terminator::CondBranch(c, a, b) => {
                let c = gen.get_value_vreg(c.id);
                gen.push_inst(X64Inst::Int2(X64BitSize::Byte, X64IntOp::Or, c, c));
                self.select_pre_jump(gen, a);
                gen.push_inst(X64Inst::Jcc(X64Cond::Nz, a.target.into()));
                self.select_pre_jump(gen, b);
                gen.push_inst(X64Inst::Jmp(b.target.into()));
            },
            Terminator::Return(v) => {
                if let Some(v) = v {
                    let bits = X64BitSize::from_vt(&v.typ);
                    let v = gen.get_value_vreg(v.id);
                    let ax = gen.vreg_alloc.alloc_virtual().force_in_reg(X64Reg::Ax);
                    gen.push_inst(X64Inst::Mov(bits, v, ax));
                }

                for (ri, r) in CALLEE_SAVE.iter().enumerate() {
                    let r = gen.vreg_alloc.alloc_virtual().force_in_reg(*r);
                    gen.push_inst(X64Inst::SavingMov(X64BitSize::Quad, self.callee_save_vregs[ri], r));
                }

                gen.push_inst(X64Inst::Leave);
                gen.push_inst(X64Inst::Ret);
            },
            Terminator::None => {},
        }
    }
}

impl Default for X64Selector {
    fn default() -> Self {
        Self::new()
    }
}

impl X64Selector {
    pub const fn new() -> Self {
        Self {
            callee_save_vregs: [VReg::Virtual(0); CALLEE_SAVE.len()],
        }
    }

    fn select_pre_jump(&self, gen: &mut VCodeGen<X64Inst>, t: &TermBlockId) {
        let mut pc = t.args.iter().zip(gen.get_bb_arg_dest(t.target).to_vec().iter()).map(|(f, t)| {
            let fv = gen.get_value_vreg(*f);
            let tv = gen.get_value_vreg(t.id);
            ((tv, t.typ), (fv, t.typ))
        }).collect();
        let seq = crate::algo::par_move::parallel_move(&mut pc, &mut |a, _| (gen.vreg_alloc.alloc_virtual(), a.1));

        for ((to, typ), (from, _)) in seq {
            let bits = X64BitSize::from_vt(&typ);
            gen.push_inst(X64Inst::BlockArgMov(bits, from, to));
        }
    }
}

impl fmt::Display for X64Inst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Mov(t, s, d) | Self::BlockArgMov(t, s, d) | Self::SavingMov(t, s, d) => write!(f, "mov{t} {}, {}", VRegFmt(s, t), VRegFmt(d, t)),
            Self::MovI(t, s, d) => write!(f, "mov{t} ${s}, {}", VRegFmt(d, t)),
            Self::CSet(c, d) => write!(f, "set{c} {}", VRegFmt(d, &X64BitSize::Byte)),
            Self::Cmp(t, a, b) => write!(f, "cmp{t} {}, {}", VRegFmt(a, t), VRegFmt(b, t)),
            Self::Int2(t, o, s, d) => write!(f, "{o} {}, {}", VRegFmt(s, t), VRegFmt(d, t)),
            Self::Int2I(t, o, s, d) => write!(f, "{o} ${s}, {}", VRegFmt(d, t)),
            Self::Push(t, r) => write!(f, "push{t} {}", VRegFmt(r, t)),
            Self::Pop(t, r) => write!(f, "pop{t} {}", VRegFmt(r, t)),
            Self::Jmp(d) => write!(f, "jmp {}", LocFmt(d)),
            Self::Jcc(c, d) => write!(f, "j{c} {}", LocFmt(d)),
            Self::Ret => write!(f, "ret"),
            Self::Leave => write!(f, "leave"),
        }
    }
}

struct LocFmt<'a>(&'a Location);

impl fmt::Display for LocFmt<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Location::Function(n) => write!(f, "F{n}"),
            Location::Block(b) => write!(f, ".L{b}"),
        }
    }
}


struct VRegFmt<'a>(&'a X64VReg, &'a X64BitSize);

impl fmt::Display for VRegFmt<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            VReg::Real(r) => match (self.1, r) {
                (X64BitSize::Byte, X64Reg::Ax) => write!(f, "%al"),
                (X64BitSize::Byte, X64Reg::Bx) => write!(f, "%bl"),
                (X64BitSize::Byte, X64Reg::Cx) => write!(f, "%cl"),
                (X64BitSize::Byte, X64Reg::Sp) => write!(f, "%spl"),
                (X64BitSize::Byte, X64Reg::Bp) => write!(f, "%bph"),
                (X64BitSize::Byte, X64Reg::Di) => write!(f, "%dil"),
                (X64BitSize::Byte, X64Reg::Si) => write!(f, "%sil"),
                (X64BitSize::Byte, X64Reg::Dx) => write!(f, "%dl"),
                (t, r) if (*r as usize) < 8 => write!(f, "%{}{r}", t.get_message().unwrap_or("")),

                (X64BitSize::Quad, r) => write!(f, "%{r}"),
                (X64BitSize::Long, r) => write!(f, "%{r}d"),
                (X64BitSize::Word, r) => write!(f, "%{r}w"),
                (X64BitSize::Byte, r) => write!(f, "%{r}b"),
            },
            VReg::Virtual(v) | VReg::VirtReal(v, _) => write!(f, "v{v}"),
            VReg::Spilled(s) => write!(f, "{}(%rbp)", -8 - ((*s as isize) * 8)),
        }
    }
}
