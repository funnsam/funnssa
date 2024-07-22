use crate::*;

impl Function<'_> {
    pub fn destruct_from_ssa(&mut self) {
        self.premove();
    }

    // requires crit edge splitting
    fn premove(&mut self) {
        let mut insert = Vec::new();

        for (bi, b) in self.blocks.iter().enumerate() {
            for succ in b.term.immediate_successor() {
                for (a, b) in succ.args.iter().zip(self.blocks[succ.target.0].args.iter()) {
                    insert.push((bi, *a, *b));
                }
            }
        }

        for (bi, a, b) in insert {
            self.blocks[bi].insts.push(Instruction::Copy(b, Value { typ: b.typ, id: a }));
        }

        for b in self.blocks.iter_mut() {
            b.args.clear();

            for s in b.term.immediate_successor_mut() {
                s.args.clear();
            }
        }
    }
}
