use core::fmt;
use crate::*;

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "${}", self.0) }
}

impl fmt::Display for Program<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, fc) in self.functions.iter().enumerate() {
            writeln!(f, "fn {i} {}:\n{fc}", fc.name)?;
        }

        Ok(())
    }
}

impl fmt::Display for Function<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, bb) in self.blocks.iter().enumerate() {
            write!(f, "{i}: {bb}")?;
        }

        Ok(())
    }
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.args.is_empty() {
            write!(f, "require ")?;
            for a in self.args.iter() {
                write!(f, "{a} ")?;
            }
        }
        writeln!(f)?;

        for i in self.insts.iter() {
            writeln!(f, "  {i}")?;
        }

        writeln!(f, "  {}", self.term)
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Assign(t, v) => write!(f, "{t} = {v}"),
            Self::IntOp(op, t, a, b) => write!(f, "{t} = {op} {a}, {b}"),
            Self::Alloc(v, t) => write!(f, "{v} = alloc {t}"),
            Self::Load(t, v) => write!(f, "{t} = load {v}"),
            Self::Store(t, v) => write!(f, "store {t}, {v}"),
        }
    }
}

impl fmt::Display for Terminator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CondBranch(c, a, b) => write!(f, "br {c} {a} {b}"),
            Self::UncondBranch(t) => write!(f, "br {t}"),
            Self::Return(Some(v)) => write!(f, "ret {v}"),
            Self::Return(None) => write!(f, "ret"),
            Self::None => write!(f, "noterm"),
        }
    }
}

impl fmt::Display for IntOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => write!(f, "add"),
        }
    }
}
