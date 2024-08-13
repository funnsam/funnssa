//! # Notes
//! - Only SystemV ABI is supported for now
//! - AT&T syntax is outputed

use core::fmt;
use crate::arch::*;

const CALLER_SAVE: &[X64RegIdx] = &[
    X64RegIdx::Ax,
    X64RegIdx::Di,
    X64RegIdx::Si,
    X64RegIdx::Dx,
    X64RegIdx::Cx,
    X64RegIdx::R8,
    X64RegIdx::R9,
];

const CALLEE_SAVE: &[X64RegIdx] = &[
    X64RegIdx::Bx,
    X64RegIdx::R12,
    X64RegIdx::R13,
    X64RegIdx::R14,
    X64RegIdx::R15,
];

const FRAME_SIZE_MARKER: i64 = i64::MAX;

pub type X64VReg = VReg<X64Reg>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct X64Reg {
    typ: X64RegType,
    idx: X64RegIdx,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, strum::Display)]
enum X64RegType {
    #[strum(serialize = "r")]
    QWord,
    #[strum(serialize = "e")]
    DWord,
    #[strum(serialize = "")]
    Word,
    #[strum(serialize = "")]
    Byte,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, strum::Display, strum::EnumCount, strum::FromRepr)]
#[strum(serialize_all = "lowercase")]
enum X64RegIdx {
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

impl fmt::Display for X64Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use X64RegIdx::*;
        use X64RegType::*;

        match (self.typ, self.idx) {
            (Byte, Ax) => write!(f, "%al"),
            (Byte, Bx) => write!(f, "%bl"),
            (Byte, Cx) => write!(f, "%cl"),
            (Byte, Sp) => write!(f, "%spl"),
            (Byte, Bp) => write!(f, "%bph"),
            (Byte, Di) => write!(f, "%dil"),
            (Byte, Si) => write!(f, "%sil"),
            (Byte, Dx) => write!(f, "%dl"),

            (t, Ax) => write!(f, "%{t}ax"),
            (t, Bx) => write!(f, "%{t}bx"),
            (t, Cx) => write!(f, "%{t}cx"),
            (t, Sp) => write!(f, "%{t}sp"),
            (t, Bp) => write!(f, "%{t}bp"),
            (t, Di) => write!(f, "%{t}di"),
            (t, Si) => write!(f, "%{t}si"),
            (t, Dx) => write!(f, "%{t}dx"),

            (QWord, r) => write!(f, "%{r}"),
            (DWord, r) => write!(f, "%{r}d"),
            (Word, r) => write!(f, "%{r}w"),
            (Byte, r) => write!(f, "%{r}b"),
        }
    }
}

impl Register for X64Reg {
    const REG_COUNT: usize = <X64RegIdx as strum::EnumCount>::COUNT;

    fn get_regs() -> &'static [Self] {
        &[
            Self { typ: X64RegType::QWord, idx: X64RegIdx::Bx },
            Self { typ: X64RegType::QWord, idx: X64RegIdx::R12 },
            Self { typ: X64RegType::QWord, idx: X64RegIdx::R13 },
            Self { typ: X64RegType::QWord, idx: X64RegIdx::R14 },
            Self { typ: X64RegType::QWord, idx: X64RegIdx::R15 },
            Self { typ: X64RegType::QWord, idx: X64RegIdx::Di },

            Self { typ: X64RegType::QWord, idx: X64RegIdx::Si },
            Self { typ: X64RegType::QWord, idx: X64RegIdx::Dx },
            Self { typ: X64RegType::QWord, idx: X64RegIdx::Cx },
            Self { typ: X64RegType::QWord, idx: X64RegIdx::R8 },
            Self { typ: X64RegType::QWord, idx: X64RegIdx::R9 },
        ]
    }
}

impl TryFrom<usize> for X64Reg {
    type Error = ();

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        X64RegIdx::from_repr(value).map(|idx| Self { typ: X64RegType::QWord, idx }).ok_or(())
    }
}

impl From<X64Reg> for usize {
    fn from(value: X64Reg) -> Self {
        value.idx as _
    }
}

impl From<X64RegIdx> for VReg<X64Reg> {
    fn from(idx: X64RegIdx) -> Self {
        VReg::Real(X64Reg { typ: X64RegType::QWord, idx })
    }
}

#[derive(Clone)]
pub enum X64Inst {
    Mov(X64VReg, X64VReg),
    MovI(i64, X64VReg),
    CSet(X64Cond, X64VReg),
    Cmp(X64VReg, X64VReg),
    Int2(X64IntOp, X64VReg, X64VReg),
    Int2I(X64IntOp, i64, X64VReg),
    Push(X64VReg),
    Pop(X64VReg),
    Jmp(Location),
    Jnz(Location),
    Ret,
    Leave,
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
            Self::Mov(s, d) => {
                ra.add_use(*s);
                ra.define(*d);
            },
            Self::MovI(_, d) => {
                ra.define(*d);
            },
            Self::CSet(_, d) => {
                ra.define(*d);
            },
            Self::Cmp(a, b) => {
                ra.add_use(*a);
                ra.add_use(*b);
            },
            Self::Int2(_, s, d) => {
                ra.add_use(*s);
                ra.add_use(*d);
                ra.define(*d);
            },
            Self::Int2I(_, _, d) => {
                ra.add_use(*d);
                ra.define(*d);
            },
            Self::Push(r) => {
                ra.add_use(*r);
            },
            Self::Pop(r) => {
                ra.define(*r);
            },
            Self::Jmp(_) | Self::Jnz(_) | Self::Ret | Self::Leave => {},
        }
    }

    fn apply_alloc(&mut self, ra: &[VReg<Self::Register>]) {
        let apply = |r: &mut X64VReg| if let VReg::Virtual(v) = r {
            *r = ra[*v];
        };

        match self {
            Self::Mov(s, d) => {
                apply(s);
                apply(d);
            },
            Self::MovI(_, d) => {
                apply(d);
            },
            Self::CSet(_, d) => {
                apply(d);
            },
            Self::Cmp(a, b) => {
                apply(a);
                apply(b);
            },
            Self::Int2(_, s, d) => {
                apply(s);
                apply(d);
                apply(d);
            },
            Self::Int2I(_, _, d) => {
                apply(d);
            },
            Self::Push(r) => {
                apply(r);
            },
            Self::Pop(r) => {
                apply(r);
            },
            Self::Jmp(_) | Self::Jnz(_) | Self::Ret | Self::Leave => {},
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
                        Self::Mov(s, d) => {
                            ua(s);
                            ud(d);
                        },
                        Self::MovI(_, d) => {
                            ud(d);
                        },
                        Self::CSet(_, d) => {
                            ud(d);
                        },
                        Self::Cmp(a, b) => {
                            ua(a);
                            ub(b);
                        },
                        Self::Int2(_, s, d) => {
                            ua(s);
                            ud(d);
                        },
                        Self::Int2I(_, _, d) => {
                            ud(d);
                        },
                        Self::Push(r) => {
                            ud(r);
                        },
                        Self::Pop(r) => {
                            ua(r);
                        },
                        Self::Jmp(_) | Self::Jnz(_) | Self::Ret | Self::Leave => {},
                    }

                    if let Some(d) = d_spilled {
                        frame_size = frame_size.max(d + 1);
                        // inst.insert(
                        //     i + 1,
                        //     Self::Lstr(UrclReg::Sp.into(), d as _, SPILL_0.into()),
                        // );
                    }
                    if let Some(a) = a_spilled {
                        frame_size = frame_size.max(a + 1);
                        // inst.insert(
                        //     i,
                        //     Self::Llod(SPILL_0.into(), UrclReg::Sp.into(), a as _),
                        // );
                    }
                    if let Some(b) = b_spilled {
                        frame_size = frame_size.max(b + 1);
                        // inst.insert(
                        //     i,
                        //     Self::Llod(SPILL_0.into(), UrclReg::Sp.into(), a as _),
                        // );
                    }

                    i += 1;
                }
            };

            unspill(&mut f.pre);
            for b in f.body.iter_mut() {
                unspill(b);
            }

            let update = |inst: &mut Vec<Self>| for i in inst.iter_mut() {
                if let Self::Int2I(_, v, VReg::Real(X64Reg { idx: X64RegIdx::Sp, .. })) = i {
                    match *v {
                        FRAME_SIZE_MARKER => *v = frame_size as i64 * 8,
                        _ => {},
                    }
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
            [Self::Mov(v, d), ..] if d == v => Some((vec![], 1)),
            [Self::Jmp(Location::Block(t))] if bi.map_or(false, |b| b.0 + 1 == *t) => Some((vec![], 1)),
            [Self::MovI(0, d), ..] => Some((vec![Self::Int2(X64IntOp::Xor, *d, *d)], 1)),
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

pub struct X64Selector;

impl InstSelector for X64Selector {
    type Instruction = X64Inst;

    fn select_pre_fn(&mut self, gen: &mut VCodeGen<X64Inst>, args: &[ValueType]) {
        gen.push_inst(X64Inst::Push(X64RegIdx::Bp.into()));
        gen.push_inst(X64Inst::Mov(X64RegIdx::Sp.into(), X64RegIdx::Bp.into()));
        gen.push_inst(X64Inst::Int2I(X64IntOp::Sub, FRAME_SIZE_MARKER, X64RegIdx::Sp.into()));

        for (ri, r) in CALLEE_SAVE.iter().enumerate() {
            gen.push_inst(X64Inst::Mov((*r).into(), VReg::Spilled(ri)));
        }
    }

    fn select_inst(&mut self, gen: &mut VCodeGen<X64Inst>, inst: &Instruction) {
        match inst {
            Instruction::Assign(d, v) => {
                let d = gen.get_value_vreg(d.id);
                gen.push_inst(X64Inst::MovI(*v as i64, d));
            },
            Instruction::IntOp(op, d, a, b) if X64Cond::try_from(*op).is_ok() => {
                let d = gen.get_value_vreg(d.id);
                let a = gen.get_value_vreg(a.id);
                let b = gen.get_value_vreg(b.id);
                gen.push_inst(X64Inst::Cmp(b, a));
                // TODO: make `d` 8-bits bc gas is confused
                // -- requires extra attrs in vregs.. kinda screwed up
                gen.push_inst(X64Inst::CSet((*op).try_into().unwrap(), d));
            },
            Instruction::IntOp(op, d, a, b) => {
                let d = gen.get_value_vreg(d.id);
                let a = gen.get_value_vreg(a.id);
                let b = gen.get_value_vreg(b.id);
                gen.push_inst(X64Inst::Mov(a, d));
                gen.push_inst(X64Inst::Int2((*op).try_into().unwrap(), b, d));
            },
            Instruction::Copy(d, v) => {
                let d = gen.get_value_vreg(d.id);
                let v = gen.get_value_vreg(v.id);
                gen.push_inst(X64Inst::Mov(v, d));
            },
            _ => todo!("{inst}"),
        }
    }

    fn select_term(&mut self, gen: &mut VCodeGen<X64Inst>, term: &Terminator) {
        match term {
            Terminator::UncondBranch(t) => {
                gen.push_inst(X64Inst::Jmp(t.target.into()));
            },
            Terminator::CondBranch(c, a, b) => {
                let c = gen.get_value_vreg(c.id);
                gen.push_inst(X64Inst::Int2(X64IntOp::Or, c, c));
                gen.push_inst(X64Inst::Jnz(a.target.into()));
                gen.push_inst(X64Inst::Jmp(b.target.into()));
            },
            Terminator::Return(v) => {
                if let Some(v) = v {
                    let v = gen.get_value_vreg(v.id);
                    gen.push_inst(X64Inst::Mov(v, X64RegIdx::Ax.into()));
                }

                for (ri, r) in CALLEE_SAVE.iter().enumerate() {
                    gen.push_inst(X64Inst::Mov(VReg::Spilled(ri), (*r).into()));
                }

                gen.push_inst(X64Inst::Leave);
                gen.push_inst(X64Inst::Ret);
            },
            _ => todo!("{term}"),
        }
    }
}

impl fmt::Display for X64Inst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Mov(s, d) => write!(f, "mov {}, {}", VRegFmt(s), VRegFmt(d)),
            Self::MovI(s, d) => write!(f, "mov ${s}, {}", VRegFmt(d)),
            Self::CSet(c, d) => write!(f, "set{c} {}", VRegFmt(d)),
            Self::Cmp(a, b) => write!(f, "cmp {}, {}", VRegFmt(a), VRegFmt(b)),
            Self::Int2(o, s, d) => write!(f, "{o} {}, {}", VRegFmt(s), VRegFmt(d)),
            Self::Int2I(o, s, d) => write!(f, "{o} ${s}, {}", VRegFmt(d)),
            Self::Push(r) => write!(f, "push {}", VRegFmt(r)),
            Self::Pop(r) => write!(f, "pop {}", VRegFmt(r)),
            Self::Jmp(d) => write!(f, "jmp {}", LocFmt(d)),
            Self::Jnz(d) => write!(f, "jnz {}", LocFmt(d)),
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


struct VRegFmt<'a>(&'a X64VReg);

impl fmt::Display for VRegFmt<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            VReg::Real(r) => r.fmt(f),
            VReg::Virtual(v) => write!(f, "v{v}"),
            VReg::Spilled(s) => write!(f, "{}(%rbp)", -8 - ((*s as isize) * 8)),
        }
    }
}
