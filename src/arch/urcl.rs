//! # Calling convention
//! - `R1`: Return value
//! - `R1` - `R5`: Arguments and temporaries
//! - `R6` - `R7`: Scratch registers

const CALLEE_SAVE: &[UrclReg] = &[UrclReg::R1, UrclReg::R2, UrclReg::R3, UrclReg::R4, UrclReg::R5];

// markers for addsp
const SP_INCR: isize = isize::MAX;
const SP_DECR: isize = isize::MIN;

const SPILL_0: UrclReg = UrclReg::R6;
const SPILL_1: UrclReg = UrclReg::R7;

use core::fmt;
use crate::arch::*;

pub struct UrclSelector {
}

#[derive(Clone)]
pub enum UrclInst {
    Int2(UrclIntOp, VReg<UrclReg>, VReg<UrclReg>, VReg<UrclReg>),
    Jmp(Location),
    Bnz(Location, VReg<UrclReg>),
    Mov(VReg<UrclReg>, VReg<UrclReg>),
    Imm(VReg<UrclReg>, u128),
    AddSp(isize),
    Ret,

    Llod(VReg<UrclReg>, VReg<UrclReg>, isize),
    Lstr(VReg<UrclReg>, isize, VReg<UrclReg>),
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

impl Register for UrclReg {
    fn get_regs() -> &'static [Self] {
        &[Self::R1, Self::R2, Self::R3, Self::R4, Self::R5]
    }

    // fn get_scratch() -> &'static [Self] {
    //     &[Self::R6, Self::R7]
    // }
}

impl Inst for UrclInst {
    type Register = UrclReg;

    fn register_regalloc(&self, ra: &mut impl RegAlloc<UrclReg>) {
        match self {
            Self::Int2(_, d, a, b) => {
                ra.define(*d);
                ra.add_use(*a);
                ra.add_use(*b);
            },
            Self::Mov(d, v) => {
                ra.define(*d);
                ra.add_use(*v);
                ra.coalesce_move(*v, *d);
            },
            Self::Imm(d, _) => ra.define(*d),
            Self::Bnz(_, c) => ra.add_use(*c),
            Self::Llod(d, b, _) => {
                ra.define(*d);
                ra.add_use(*b);
            },
            Self::Lstr(b, _, v) => {
                ra.add_use(*b);
                ra.add_use(*v);
            },
            Self::AddSp(_) | Self::Ret | Self::Jmp(_) => {},
        }
    }

    fn apply_alloc(&mut self, ra: &[VReg<UrclReg>]) {
        let apply = |r: &mut VReg<UrclReg>| if let VReg::Virtual(v) = r {
            *r = ra[*v];
        };

        match self {
            Self::Int2(_, d, a, b) => {
                apply(d);
                apply(a);
                apply(b);
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
            Self::AddSp(_) | Self::Ret | Self::Jmp(_) => {},
        }
    }
}

impl fmt::Display for UrclInst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::AddSp(v) => writeln!(f, "add sp sp {v}"),
            Self::Int2(op, d, a, b) => writeln!(f, "{op} {d} {a} {b}"),
            Self::Mov(d, v) => writeln!(f, "mov {d} {v}"),
            Self::Imm(d, v) => writeln!(f, "imm {d} {v}"),
            Self::Jmp(d) => writeln!(f, "jmp {d}"),
            Self::Bnz(d, c) => writeln!(f, "bnz {d} {c}"),
            Self::Ret => writeln!(f, "ret"),

            Self::Llod(d, b, o) => writeln!(f, "llod {d} {b} {o}"),
            Self::Lstr(b, o, v) => writeln!(f, "lstr {b} {o} {v}"),
        }
    }
}

impl InstSelector for UrclSelector {
    type Instruction = UrclInst;

    fn new() -> Self {
        Self {}
    }

    fn select_pre_fn(&mut self, gen: &mut VCodeGen<Self::Instruction>) {
        gen.push_inst(UrclInst::AddSp(SP_DECR));
        for (i, r) in CALLEE_SAVE.iter().enumerate() {
            gen.push_inst(UrclInst::Lstr(VReg::Real(UrclReg::Sp), -(i as isize), VReg::Real(*r)));
        }
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
            Terminator::Return(r) => {
                if let Some(r) = r {
                    let r = gen.get_value_vreg(r.id);
                    gen.push_inst(UrclInst::Mov(VReg::Real(UrclReg::R1), r));
                }

                for (i, r) in CALLEE_SAVE.iter().enumerate().skip(r.is_some() as _) {
                    gen.push_inst(UrclInst::Llod(VReg::Real(*r), VReg::Real(UrclReg::Sp), -(i as isize)));
                }
                gen.push_inst(UrclInst::AddSp(SP_INCR));
                gen.push_inst(UrclInst::Ret);
            },
            Terminator::None => {},
        }
    }

    fn apply_mandatory_transforms(&mut self, vcode: &mut VCode<Self::Instruction>) {
        for f in vcode.funcs.iter_mut() {
            let mut frame_size = 0;
            let mut unspill = |inst: &mut Vec<UrclInst>| {
                let mut i = 0;
                while i < inst.len() {
                    let mut d_spilled = None;
                    let mut a_spilled = None;
                    let mut b_spilled = None;
                    let mut ud = |d: &mut VReg<UrclReg>| if let VReg::Spilled(s) = d {
                        d_spilled = Some(*s);
                        *d = VReg::Real(SPILL_0);
                    };
                    let mut ua = |a: &mut VReg<UrclReg>| if let VReg::Spilled(s) = a {
                        a_spilled = Some(*s);
                        *a = VReg::Real(SPILL_0);
                    };
                    let mut ub = |b: &mut VReg<UrclReg>| if let VReg::Spilled(s) = b {
                        b_spilled = Some(*s);
                        *b = VReg::Real(SPILL_1);
                    };

                    match &mut inst[i] {
                        UrclInst::Int2(_, d, a, b) => {
                            ud(d);
                            ua(a);
                            ub(b);
                        },
                        UrclInst::Mov(d, v) => {
                            ud(d);
                            ua(v);
                        },
                        UrclInst::Imm(d, _) => ud(d),
                        UrclInst::Bnz(_, c) => ua(c),
                        UrclInst::Llod(d, b, _) => {
                            ud(d);
                            ua(b);
                        },
                        UrclInst::Lstr(b, _, v) => {
                            ua(b);
                            ub(v);
                        },
                        UrclInst::AddSp(_) | UrclInst::Ret | UrclInst::Jmp(_) => {},
                    }

                    let stk = |i| -(CALLEE_SAVE.len() as isize + i as isize);

                    if let Some(d) = d_spilled {
                        frame_size = frame_size.max(d + 1);
                        inst.insert(
                            i + 1,
                            UrclInst::Lstr(VReg::Real(UrclReg::Sp), stk(d), VReg::Real(SPILL_0)),
                        );
                    }
                    if let Some(a) = a_spilled {
                        frame_size = frame_size.max(a + 1);
                        inst.insert(
                            i,
                            UrclInst::Llod(VReg::Real(SPILL_0), VReg::Real(UrclReg::Sp), stk(a)),
                        );
                    }
                    if let Some(b) = b_spilled {
                        frame_size = frame_size.max(b + 1);
                        inst.insert(
                            i,
                            UrclInst::Llod(VReg::Real(SPILL_1), VReg::Real(UrclReg::Sp), stk(b)),
                        );
                    }

                    i += 1 + d_spilled.is_some() as usize + a_spilled.is_some() as usize + b_spilled.is_some() as usize;
                }
            };

            unspill(&mut f.pre);
            for b in f.body.iter_mut() {
                unspill(b);
            }

            let frame_size = (frame_size + CALLEE_SAVE.len()) as isize;
            let update = |inst: &mut Vec<UrclInst>| for i in inst.iter_mut() {
                if let UrclInst::AddSp(v) = i {
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
        &mut self,
        area: &[Self::Instruction],
        bi: Option<BlockId>,
    ) -> Option<(Vec<Self::Instruction>, usize)> {
        match area {
            [UrclInst::Mov(d, v), ..] if d == v => Some((vec![], 1)),
            [UrclInst::Jmp(Location::Block(t))] if bi.map_or(false, |b| b.0 + 1 == *t) => Some((vec![], 1)),
            [UrclInst::Imm(d, 0), ..] => Some((vec![UrclInst::Int2(UrclIntOp::Xor, *d, *d, *d)], 1)),
            _ => None,
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
