use core::fmt;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Int(usize),
    Ptr,
    Array(Box<Type>, usize),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ValueType {
    Int(usize),
    Ptr,
}

impl Into<ValueType> for Type {
    fn into(self) -> ValueType {
        match self {
            Self::Int(s) => ValueType::Int(s),
            Self::Ptr | Self::Array(..) => ValueType::Ptr,
        }
    }
}

impl Into<Type> for ValueType {
    fn into(self) -> Type {
        match self {
            Self::Int(s) => Type::Int(s),
            Self::Ptr => Type::Ptr,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(s) => write!(f, "i{s}"),
            Self::Ptr => write!(f, "ptr"),
            Self::Array(t, s) => write!(f, "[{t}; {s}]"),
        }
    }
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(s) => write!(f, "i{s}"),
            Self::Ptr => write!(f, "ptr"),
        }
    }
}
