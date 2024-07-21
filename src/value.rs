use core::fmt;
use crate::types::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Value {
    pub typ: ValueType,
    pub(crate) id: ValueId,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ValueId(pub(crate) usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct IntValue {
    pub(crate) size: usize,
    pub(crate) id: ValueId,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct PtrValue(pub(crate) ValueId);

impl Into<Value> for IntValue {
    fn into(self) -> Value {
        Value {
            typ: ValueType::Int(self.size),
            id: self.id,
        }
    }
}

impl TryInto<IntValue> for Value {
    type Error = ();

    fn try_into(self) -> Result<IntValue, Self::Error> {
        match self.typ {
            ValueType::Int(s) => Ok(IntValue {
                size: s,
                id: self.id,
            }),
            _ => Err(())
        }
    }
}

impl fmt::Display for ValueId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "#{}", self.0) }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{} {}", self.typ, self.id) }
}

impl fmt::Display for IntValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "i{} {}", self.size, self.id) }
}

impl fmt::Display for PtrValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "ptr {}", self.0) }
}
