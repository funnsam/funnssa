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

impl From<Type> for ValueType {
    fn from(val: Type) -> Self {
        match val {
            Type::Int(s) => Self::Int(s),
            Type::Ptr | Type::Array(..) => Self::Ptr,
        }
    }
}

impl From<ValueType> for Type {
    fn from(val: ValueType) -> Self {
        match val {
            ValueType::Int(s) => Self::Int(s),
            ValueType::Ptr => Self::Ptr,
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
