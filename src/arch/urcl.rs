use core::fmt;
use crate::arch::*;

pub struct UrclSelector {
}

pub enum UrclInst {
    Int2(UrclIntOp, VReg<UrclReg>, VReg<UrclReg>, VReg<UrclReg>),
    Jmp(Location),
    Bnz(Location, VReg<UrclReg>),
    Mov(VReg<UrclReg>, VReg<UrclReg>),
    Imm(VReg<UrclReg>, u128),
    Ret
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
    Srs,
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, strum::Display)]
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

impl Inst for UrclInst {
    type Register = UrclReg;
}

impl InstSelector for UrclSelector {
    type Instruction = UrclInst;

    fn new() -> Self {
        Self {}
    }

    fn select_pre_fn(&mut self, _gen: &mut VCodeGen<Self::Instruction>) {
    }

    fn select_inst(&mut self, gen: &mut VCodeGen<Self::Instruction>, inst: &Instruction) {
        match inst {
            Instruction::IntOp(op, d, a, b) => {
                let d = gen.get_value_vreg(d.id);
                let a = gen.get_value_vreg(a.id);
                let b = gen.get_value_vreg(b.id);
                gen.push_inst(UrclInst::Int2((*op).into(), d, a, b));
            },
            Instruction::Copy(d, v) => {
                let d = gen.get_value_vreg(d.id);
                let v = gen.get_value_vreg(v.id);
                gen.push_inst(UrclInst::Mov(d, v));
            },
            Instruction::Assign(d, v) => {
                let d = gen.get_value_vreg(d.id);
                gen.push_inst(UrclInst::Imm(d, *v));
            },
            Instruction::Alloc(..) => {},
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
            Terminator::Return(Some(r)) => {
                let r = gen.get_value_vreg(r.id);
                gen.push_inst(UrclInst::Mov(VReg::Real(UrclReg::R1), r));
                gen.push_inst(UrclInst::Ret);
            },
            Terminator::Return(None) => {
                gen.push_inst(UrclInst::Ret);
            },
            Terminator::None => {},
        }
    }
}

impl fmt::Display for UrclInst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int2(op, d, a, b) => writeln!(f, "{op} {d} {a} {b}"),
            Self::Mov(d, v) => writeln!(f, "mov {d} {v}"),
            Self::Imm(d, v) => writeln!(f, "imm {d} {v}"),
            Self::Jmp(d) => writeln!(f, "jmp {d}"),
            Self::Bnz(d, c) => writeln!(f, "bnz {d} {c}"),
            Self::Ret => writeln!(f, "ret"),
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
            IntOp::SRsh => Self::Srs,
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
