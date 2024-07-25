//! # Notes
//! - Only SystemV ABI is supported for now
//! - AT&T syntax is outputed

use core::fmt;
use crate::arch::*;

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
            Self { typ: X64RegType::QWord, idx: X64RegIdx::Di },
            Self { typ: X64RegType::QWord, idx: X64RegIdx::Si },
            Self { typ: X64RegType::QWord, idx: X64RegIdx::Dx },
            Self { typ: X64RegType::QWord, idx: X64RegIdx::Cx },
            Self { typ: X64RegType::QWord, idx: X64RegIdx::R8 },
            Self { typ: X64RegType::QWord, idx: X64RegIdx::R9 },
            Self { typ: X64RegType::QWord, idx: X64RegIdx::Bx },
            Self { typ: X64RegType::QWord, idx: X64RegIdx::R12 },
            Self { typ: X64RegType::QWord, idx: X64RegIdx::R13 },
            Self { typ: X64RegType::QWord, idx: X64RegIdx::R14 },
            Self { typ: X64RegType::QWord, idx: X64RegIdx::R15 },
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
    CMov(X64Cond, X64VReg, X64VReg),
    Cmp(X64VReg, X64VReg),
    Int2(X64IntOp, X64VReg, X64VReg),
    Int2I(X64IntOp, i64, X64VReg),
    Push(X64VReg),
    Pop(X64VReg),
    Jmp(Location),
    Bne(Location),
    Ret,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, strum::Display)]
#[strum(serialize_all = "lowercase")]
pub enum X64Cond {
    E,
    Ne,
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
            Self::CMov(_, s, d) => {
                ra.add_use(*s);
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
            Self::Jmp(_) => {},
            Self::Bne(_) => {},
            Self::Ret => {},
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
            Self::CMov(_, s, d) => {
                apply(s);
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
                apply(d);
            },
            Self::Push(r) => {
                apply(r);
            },
            Self::Pop(r) => {
                apply(r);
            },
            Self::Jmp(_) => {},
            Self::Bne(_) => {},
            Self::Ret => {},
        }
    }

    fn apply_mandatory_transforms(vcode: &mut VCode<Self>) {
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
        gen.push_inst(X64Inst::Int2I(X64IntOp::Add, 0, X64RegIdx::Sp.into()));
    }

    fn select_inst(&mut self, gen: &mut VCodeGen<X64Inst>, inst: &Instruction) {
        match inst {
            Instruction::Assign(d, v) => {
                let d = gen.get_value_vreg(d.id);
                gen.push_inst(X64Inst::MovI(*v as i64, d));
            },
            Instruction::IntOp(IntOp::Add, d, a, b) => {
                let d = gen.get_value_vreg(d.id);
                let a = gen.get_value_vreg(a.id);
                let b = gen.get_value_vreg(b.id);
                gen.push_inst(X64Inst::Mov(a, d));
                gen.push_inst(X64Inst::Int2(X64IntOp::Add, b, d));
            },
            _ => todo!(),
        }
    }

    fn select_term(&mut self, gen: &mut VCodeGen<X64Inst>, term: &Terminator) {
        match term {
            Terminator::UncondBranch(t) => {
                gen.push_inst(X64Inst::Jmp(t.target.into()));
            },
            Terminator::Return(v) => {
                if let Some(v) = v {
                    let v = gen.get_value_vreg(v.id);
                    gen.push_inst(X64Inst::Mov(v, X64RegIdx::Ax.into()));
                }

                gen.push_inst(X64Inst::Mov(X64RegIdx::Bp.into(), X64RegIdx::Sp.into()));
                gen.push_inst(X64Inst::Pop(X64RegIdx::Bp.into()));
                gen.push_inst(X64Inst::Ret);
            },
            _ => todo!(),
        }
    }
}

impl fmt::Display for X64Inst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Mov(s, d) => write!(f, "mov {s}, {d}"),
            Self::MovI(s, d) => write!(f, "mov ${s}, {d}"),
            Self::CMov(c, s, d) => write!(f, "cmov{c} {s}, {d}"),
            Self::Cmp(a, b) => write!(f, "cmp {a}, {b}"),
            Self::Int2(o, s, d) => write!(f, "{o} {s}, {d}"),
            Self::Int2I(o, s, d) => write!(f, "{o} ${s}, {d}"),
            Self::Push(r) => write!(f, "push {r}"),
            Self::Pop(r) => write!(f, "pop {r}"),
            Self::Jmp(d) => write!(f, "jmp {}", LocFmt(d)),
            Self::Bne(d) => write!(f, "bne {}", LocFmt(d)),
            Self::Ret => write!(f, "ret"),
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
