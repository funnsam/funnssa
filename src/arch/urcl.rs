//! # Calling convention
//! - `R1`: Return value
//! - `R1` - `R5`: Arguments and temporaries
//! - `R6` - `R7`: Scratch registers

use core::fmt;
use crate::arch::*;

const CALLEE_SAVE: &[UrclReg] = &[UrclReg::R1, UrclReg::R2, UrclReg::R3, UrclReg::R4, UrclReg::R5];
const ARG_REGS: &[UrclReg] = &[UrclReg::R1, UrclReg::R2, UrclReg::R3, UrclReg::R4, UrclReg::R5];
const RET_REG: UrclReg = UrclReg::R1;

// markers for addsp
const SP_INCR: i64 = i64::MAX;
const SP_DECR: i64 = i64::MIN;

const SPILL_0: UrclReg = UrclReg::R6;
const SPILL_1: UrclReg = UrclReg::R7;

#[derive(Default)]
pub struct UrclSelector;

#[derive(Clone)]
pub enum Operand {
    Immediate(i64),
    Register(VReg<UrclReg>),
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Immediate(i) => i.fmt(f),
            Self::Register(r) => r.fmt(f),
        }
    }
}

impl<T: Into<VReg<UrclReg>>> From<T> for Operand {
    fn from(value: T) -> Self {
        Self::Register(value.into())
    }
}

#[derive(Clone)]
pub enum UrclInst {
    Int2(UrclIntOp, VReg<UrclReg>, Operand, Operand),
    Mov(VReg<UrclReg>, VReg<UrclReg>),
    Imm(VReg<UrclReg>, u64),

    Cal(Location),
    Jmp(Location),
    Bnz(Location, VReg<UrclReg>),
    Ret,

    Llod(VReg<UrclReg>, VReg<UrclReg>, i64),
    Lstr(VReg<UrclReg>, i64, VReg<UrclReg>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, strum::Display)]
#[strum(serialize_all = "lowercase")]
pub enum UrclIntOp {
    Add,
    Sub,
    Mlt,
    Div,
    SDiv,
    Bsl,
    Bsr,
    Bss,
    And,
    Or,
    Xor,

    SetL,
    SetLe,
    SetG,
    SetGe,
    SSetL,
    SSetLe,
    SSetG,
    SSetGe,
    SetE,
    SetNe,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, strum::Display, strum::EnumCount, strum::FromRepr)]
#[strum(serialize_all = "lowercase")]
pub enum UrclReg {
    Sp,
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
}

impl Register for UrclReg {
    const REG_COUNT: usize = <Self as strum::EnumCount>::COUNT;

    fn get_regs() -> &'static [Self] {
        &[Self::R1, Self::R2, Self::R3, Self::R4, Self::R5]
    }
}

impl From<UrclReg> for usize {
    fn from(value: UrclReg) -> Self {
        value as _
    }
}

impl TryFrom<usize> for UrclReg {
    type Error = ();

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        Self::from_repr(value).ok_or(())
    }
}

impl Inst for UrclInst {
    type Register = UrclReg;

    fn register_regalloc(&self, ra: &mut impl RegAlloc<UrclReg>) {
        match self {
            Self::Int2(_, d, a, b) => {
                ra.define(*d);
                if let Operand::Register(a) = a { ra.add_use(*a); }
                if let Operand::Register(b) = b { ra.add_use(*b); }
            },
            Self::Mov(d, v) => {
                ra.define(*d);
                ra.add_use(*v);
            },
            Self::Imm(d, _) => ra.define(*d),
            Self::Bnz(_, c) => ra.add_use(*c),
            Self::Llod(d, b, _) => {
                // ra.define(*d);
                // ra.add_use(*b);
            },
            Self::Lstr(b, _, v) => {
                // ra.add_use(*b);
                // ra.add_use(*v);
            },
            Self::Ret | Self::Jmp(_) | Self::Cal(_) => {},
        }
    }

    fn apply_alloc(&mut self, ra: &[VReg<UrclReg>]) {
        let apply = |r: &mut VReg<UrclReg>| if let VReg::Virtual(v) = r {
            *r = ra[*v];
        };

        match self {
            Self::Int2(_, d, a, b) => {
                apply(d);
                if let Operand::Register(a) = a { apply(a); }
                if let Operand::Register(b) = b { apply(b); }
            },
            Self::Mov(d, v) => {
                apply(d);
                apply(v);
            },
            Self::Imm(d, _) => apply(d),
            Self::Bnz(_, c) => apply(c),
            Self::Llod(d, b, _) => {
                apply(d);
                apply(b);
            },
            Self::Lstr(b, _, v) => {
                apply(b);
                apply(v);
            },
            Self::Ret | Self::Jmp(_) | Self::Cal(_) => {},
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
                    let mut ud = |d: &mut VReg<UrclReg>| if let VReg::Spilled(s) = d {
                        d_spilled = Some(*s);
                        *d = SPILL_0.into();
                    };
                    let mut ua = |a: &mut VReg<UrclReg>| if let VReg::Spilled(s) = a {
                        a_spilled = Some(*s);
                        *a = SPILL_0.into();
                    };
                    let mut ub = |b: &mut VReg<UrclReg>| if let VReg::Spilled(s) = b {
                        b_spilled = Some(*s);
                        *b = SPILL_1.into();
                    };

                    match &mut inst[i] {
                        Self::Int2(_, d, a, b) => {
                            ud(d);
                            if let Operand::Register(a) = a { ua(a); }
                            if let Operand::Register(b) = b { ub(b); }
                        },
                        Self::Mov(d, v) => {
                            ud(d);
                            ua(v);
                        },
                        Self::Imm(d, _) => ud(d),
                        Self::Bnz(_, c) => ua(c),
                        Self::Llod(d, b, _) => {
                            ud(d);
                            ua(b);
                        },
                        Self::Lstr(b, _, v) => {
                            ua(b);
                            ub(v);
                        },
                        Self::Ret | Self::Jmp(_) | Self::Cal(_) => {},
                    }

                    let stk = |i| CALLEE_SAVE.len() as i64 + i as i64;

                    if let Some(d) = d_spilled {
                        frame_size = frame_size.max(d + 1);
                        inst.insert(
                            i + 1,
                            Self::Lstr(UrclReg::Sp.into(), stk(d), SPILL_0.into()),
                        );
                    }
                    if let Some(a) = a_spilled {
                        frame_size = frame_size.max(a + 1);
                        inst.insert(
                            i,
                            Self::Llod(SPILL_0.into(), UrclReg::Sp.into(), stk(a)),
                        );
                    }
                    if let Some(b) = b_spilled {
                        frame_size = frame_size.max(b + 1);
                        inst.insert(
                            i,
                            Self::Llod(SPILL_1.into(), UrclReg::Sp.into(), stk(b)),
                        );
                    }

                    i += 1 + d_spilled.is_some() as usize + a_spilled.is_some() as usize + b_spilled.is_some() as usize;
                }
            };

            unspill(&mut f.pre);
            for b in f.body.iter_mut() {
                unspill(b);
            }

            let frame_size = (frame_size + CALLEE_SAVE.len()) as i64;
            let update = |inst: &mut Vec<Self>| for i in inst.iter_mut() {
                if let Self::Int2(UrclIntOp::Add, VReg::Real(UrclReg::Sp), Operand::Register(VReg::Real(UrclReg::Sp)), Operand::Immediate(v)) = i {
                    match *v {
                        SP_INCR => *v = frame_size,
                        SP_DECR => *v = -frame_size,
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
            [Self::Mov(d, v), ..] if d == v => Some((vec![], 1)),
            [Self::Jmp(Location::Block(t))] if bi.map_or(false, |b| b.0 + 1 == *t) => Some((vec![], 1)),
            [Self::Imm(d, 0), ..] => Some((
                vec![Self::Int2(UrclIntOp::Xor, *d, (*d).into(), (*d).into())],
                1,
            )),
            [Self::Mov(a1, b1), Self::Mov(b2, a2), ..] if *a1 == *a2 && *b1 == *b2 => Some((
                vec![area[0].clone()],
                2,
            )),
            [Self::Mov(a1, _), Self::Mov(a2, _), ..] if *a1 == *a2 => Some((
                vec![area[1].clone()],
                2,
            )),
            _ => None,
        }
    }

    fn emit_assembly<W: std::io::Write>(f: &mut W, vcode: &VCode<Self>) -> std::io::Result<()> {
        writeln!(f, "bits 32")?;
        writeln!(f, "minreg 7")?;
        writeln!(f, "minstack 512")?;

        for (i, n) in vcode.funcs.iter().enumerate() {
            writeln!(f, "@define F{i} .F{}", n.name)?;
        }

        writeln!(f, "cal .F_start")?;
        writeln!(f, "hlt")?;

        for n in vcode.funcs.iter() {
            if n.linkage == Linkage::External { continue; }

            writeln!(f, "\n.F{}", n.name)?;

            for i in n.pre.iter() {
                writeln!(f, "{}", InstFmt(i, n.name))?;
            }
            for (bi, b) in n.body.iter().enumerate() {
                writeln!(f, ".L{bi}_W{}", n.name)?;
                for i in b.iter() {
                    writeln!(f, "{}", InstFmt(i, n.name))?;
                }
            }
        }

        Ok(())
    }
}

impl InstSelector for UrclSelector {
    type Instruction = UrclInst;

    fn select_pre_fn(&mut self, gen: &mut VCodeGen<Self::Instruction>, args: &[ValueType]) {
        gen.push_inst(UrclInst::Int2(UrclIntOp::Add, UrclReg::Sp.into(), UrclReg::Sp.into(), Operand::Immediate(SP_DECR)));
        for (i, r) in CALLEE_SAVE.iter().copied().enumerate() {
            gen.push_inst(UrclInst::Lstr(UrclReg::Sp.into(), i as i64, r.into()));
        }

        // TODO: big ass arg counts
        for (i, (_, r)) in args.iter().zip(ARG_REGS.iter().copied()).enumerate() {
            let i = gen.get_arg_vreg(i);
            gen.push_inst(UrclInst::Mov(i, r.into()));
        }
    }

    fn select_inst(&mut self, gen: &mut VCodeGen<Self::Instruction>, inst: &Instruction) {
        match inst {
            Instruction::IntOp(op, d, a, b) => {
                let d = gen.get_value_vreg(d.id);
                let a = gen.get_value_vreg(a.id);
                let b = gen.get_value_vreg(b.id);
                gen.push_inst(UrclInst::Int2((*op).into(), d, a.into(), b.into()));
            },
            Instruction::Copy(d, v) => {
                let d = gen.get_value_vreg(d.id);
                let v = gen.get_value_vreg(v.id);
                gen.push_inst(UrclInst::Mov(d, v));
            },
            Instruction::SignExt(d, v) => {
                let dr = gen.get_value_vreg(d.id);
                let vr = gen.get_value_vreg(v.id);
                gen.push_inst(UrclInst::Int2(UrclIntOp::Bsl, dr, vr.into(), Operand::Immediate(32 - v.size as i64)));
                gen.push_inst(UrclInst::Int2(UrclIntOp::Bss, vr, vr.into(), Operand::Immediate(32 - v.size as i64)));
            },
            Instruction::ZeroExt(d, v) => {
                let d = gen.get_value_vreg(d.id);
                let v = gen.get_value_vreg(v.id);
                gen.push_inst(UrclInst::Mov(d, v));
            },
            Instruction::Assign(d, v) => {
                let d = gen.get_value_vreg(d.id);
                gen.push_inst(UrclInst::Imm(d, *v as _));
            },
            Instruction::Call(r, d, a) => {
                let mut saved = Vec::with_capacity(a.len());
                for (a, r) in a.iter().zip(ARG_REGS.iter().copied()) {
                    let a = gen.get_value_vreg(a.id);
                    let save = gen.vreg_alloc.alloc_virtual();
                    gen.push_inst(UrclInst::Mov(save, r.into()));
                    gen.push_inst(UrclInst::Mov(r.into(), a));
                    saved.push(save);
                }

                gen.push_inst(UrclInst::Cal((*d).into()));

                if let Some(r) = r {
                    let r = gen.get_value_vreg(r.id);
                    gen.push_inst(UrclInst::Mov(r, RET_REG.into()));
                }

                for (s, r) in saved.into_iter().zip(ARG_REGS.iter().copied()) {
                    gen.push_inst(UrclInst::Mov(r.into(), s));
                }
            },
            Instruction::Alloc(..) => todo!(),
            Instruction::Load(..) | Instruction::Store(..) => todo!(),
        }
    }

    fn select_term(&mut self, gen: &mut VCodeGen<Self::Instruction>, term: &Terminator) {
        match term {
            Terminator::CondBranch(c, a, b) => {
                let c = gen.get_value_vreg(c.id);
                gen.push_inst(UrclInst::Bnz(a.target.into(), c));
                gen.push_inst(UrclInst::Jmp(b.target.into()));
            },
            Terminator::UncondBranch(t) => {
                gen.push_inst(UrclInst::Jmp(t.target.into()));
            },
            Terminator::Return(r) => {
                if let Some(r) = r {
                    let r = gen.get_value_vreg(r.id);
                    gen.push_inst(UrclInst::Mov(RET_REG.into(), r));
                }

                for (i, r) in CALLEE_SAVE.iter().copied().enumerate().skip(r.is_some() as _) {
                    gen.push_inst(UrclInst::Llod(r.into(), UrclReg::Sp.into(), i as i64));
                }
                gen.push_inst(UrclInst::Int2(UrclIntOp::Add, UrclReg::Sp.into(), UrclReg::Sp.into(), Operand::Immediate(SP_INCR)));
                gen.push_inst(UrclInst::Ret);
            },
            Terminator::None => {},
        }
    }
}

impl From<IntOp> for UrclIntOp {
    fn from(v: IntOp) -> Self {
        match v {
            IntOp::Add => Self::Add,
            IntOp::Sub => Self::Sub,
            IntOp::Mul => Self::Mlt,
            IntOp::UDiv => Self::Div,
            IntOp::SDiv => Self::SDiv,
            IntOp::Lsh => Self::Bsl,
            IntOp::URsh => Self::Bsr,
            IntOp::SRsh => Self::Bss,
            IntOp::And => Self::And,
            IntOp::Or => Self::Or,
            IntOp::Xor => Self::Xor,

            IntOp::ULt => Self::SetL,
            IntOp::ULe => Self::SetLe,
            IntOp::UGt => Self::SetG,
            IntOp::UGe => Self::SetGe,
            IntOp::SLt => Self::SSetL,
            IntOp::SLe => Self::SSetLe,
            IntOp::SGt => Self::SSetG,
            IntOp::SGe => Self::SSetGe,
            IntOp::Eq => Self::SetE,
            IntOp::Ne => Self::SetNe,
        }
    }
}

impl fmt::Display for UrclInst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        InstFmt(self, "").fmt(f)
    }
}

struct InstFmt<'a>(&'a UrclInst, &'a str);

impl fmt::Display for InstFmt<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            UrclInst::Int2(op, d, a, b) => write!(f, "{op} {d} {a} {b}"),
            UrclInst::Mov(d, v) => write!(f, "mov {d} {v}"),
            UrclInst::Imm(d, v) => write!(f, "imm {d} {v}"),
            UrclInst::Jmp(d) => write!(f, "jmp {}", LbFmt(d, self.1)),
            UrclInst::Bnz(d, c) => write!(f, "bnz {} {c}", LbFmt(d, self.1)),
            UrclInst::Ret => write!(f, "ret"),
            UrclInst::Cal(d) => write!(f, "cal {}", LbFmt(d, self.1)),

            UrclInst::Llod(d, b, o) => write!(f, "llod {d} {b} {o}"),
            UrclInst::Lstr(b, o, v) => write!(f, "lstr {b} {o} {v}"),
        }
    }
}

struct LbFmt<'a>(&'a Location, &'a str);

impl fmt::Display for LbFmt<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Location::Function(n) => write!(f, "F{n}"),
            Location::Block(b) => write!(f, ".L{b}_W{}", self.1),
        }
    }
}
