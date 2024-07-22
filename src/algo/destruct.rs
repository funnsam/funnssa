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
                let mut pcopy = succ.args.iter()
                    .copied()
                    .zip(self.blocks[succ.target.0].args.iter())
                    .map(|(a, b)| (Value { typ: b.typ, id: a }, *b))
                    .collect();
                let seq = super::par_move::parallel_move(&mut pcopy, &mut |_, a| Value { typ: a.typ, id: self.val_alloc.alloc_val() });
                insert.extend(seq.into_iter().map(|(a, b)| (bi, a, b)));
            }
        }

        for (bi, a, b) in insert {
            self.blocks[bi].insts.push(Instruction::Copy(b, a));
        }

        for b in self.blocks.iter_mut() {
            b.args.clear();

            for s in b.term.immediate_successor_mut() {
                s.args.clear();
            }
        }
    }
}
