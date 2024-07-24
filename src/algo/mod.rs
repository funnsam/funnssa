use crate::*;

pub mod construct;
pub mod destruct;
pub mod par_move;

impl Function<'_> {
    pub fn do_everything(&mut self) {
        if self.linkage != Linkage::External {
            self.construct_ssa();
            self.destruct_from_ssa();
            self.delete_unused();
        }
    }

    fn delete_unused(&mut self) {
        let mut used = vec![false; self.val_alloc.0.0];
        let mut u = |i: ValueId| used[i.0] = true;

        for b in self.blocks.iter() {
            for i in b.insts.iter() {
                match i {
                    Instruction::Alloc(..)
                    | Instruction::Assign(..)
                    | Instruction::Load(..) => {},

                    Instruction::Copy(_, v) => u(v.id),
                    Instruction::SignExt(_, v)
                    | Instruction::ZeroExt(_, v) => u(v.id),
                    Instruction::IntOp(_, _, a, b) => {
                        u(a.id);
                        u(b.id);
                    },
                    Instruction::Call(_, _, a) => for a in a.iter() {
                        u(a.id);
                    },

                    Instruction::Store(_, v) => u(v.id),
                }
            }

            match b.term {
                Terminator::CondBranch(c, ..) => u(c.id),
                Terminator::Return(Some(v)) => u(v.id),
                Terminator::UncondBranch(..) | Terminator::Return(None) | Terminator::None => {},
            }
        }

        for b in self.blocks.iter_mut() {
            let mut i = 0;
            while i < b.insts.len() {
                let ci = b.insts[i].clone();

                let mut del = |id: ValueId| if !used[id.0] {
                    b.insts.remove(i);
                } else {
                    i += 1;
                };

                match ci {
                    Instruction::Copy(d, _) => del(d.id),
                    Instruction::SignExt(d, _)
                    | Instruction::ZeroExt(d, _) => del(d.id),
                    Instruction::Load(d, _) => del(d.id),
                    Instruction::Alloc(d, _) => del(d.0),
                    Instruction::IntOp(_, d, _, _) => del(d.id),
                    Instruction::Assign(d, _) => del(d.id),

                    _ => i += 1,
                }
            }
        }
    }
}
