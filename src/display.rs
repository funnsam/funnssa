use core::fmt;
use crate::*;

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "${}", self.0) }
}

impl fmt::Display for FuncId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "fn{}", self.0) }
}

impl fmt::Display for TermBlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.target)?;

        if !self.args.is_empty() {
            write!(f, "(")?;
            for (i, a) in self.args.iter().enumerate() {
                write!(f, "{a}")?;
                if self.args.len() - 1 != i { write!(f, ", ")?; }
            }
            write!(f, ")")?;
        }

        Ok(())
    }
}

impl fmt::Display for Program<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, fc) in self.functions.iter().enumerate() {
            write!(f, "fn{i} = {fc}")?;
        }

        Ok(())
    }
}

impl fmt::Display for Function<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} fn {}(", self.linkage, self.name)?;
        for (i, t) in self.arguments.iter().enumerate() {
            write!(f, "{t} #{i}")?;
            if i != self.arguments.len() - 1 { write!(f, ", ")?; }
        }
        write!(f, ")")?;
        if let Some(r) = self.returns { write!(f, " -> {r}")?; }
        writeln!(f, ":")?;

        for (i, bb) in self.blocks.iter().enumerate() {
            write!(f, "{i}: {bb}")?;
        }

        if !self.blocks.is_empty() { writeln!(f)?; }
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
            Self::Copy(t, v) => write!(f, "{t} = {v}"),
            Self::SignExt(t, v) => write!(f, "{t} = sext {v}"),
            Self::ZeroExt(t, v) => write!(f, "{t} = zext {v}"),
            Self::Call(r, n, a) => {
                if let Some(r) = r { write!(f, "{r} = ")?; }
                write!(f, "call {n} ")?;
                for a in a.iter() { write!(f, "{a} ")?; }
                Ok(())
            },
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
