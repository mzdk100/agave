use std::{
    fmt::{Display, Formatter},
    ops::{Add, Div, Mul, Sub},
    rc::Rc,
    str::FromStr,
};

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Opcode {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Token {
    Comma,
    Express(Opcode, Rc<Self>, Rc<Self>),
    Value(Vals),
    Ident(String),
    Assignment(String, Rc<Self>),
    CallFunction(Rc<Self>, Rc<Vec<Self>>),
}

impl Token {
    pub(crate) fn new_comma() -> Self {
        Self::Comma
    }

    pub(crate) fn new_ident(name: &str) -> Token {
        Self::Ident(name.to_string())
    }

    pub(crate) fn new_call_function(name: Token, args: Vec<Self>) -> Self {
        Self::CallFunction(name.into(), args.into())
    }

    pub(crate) fn new_assignment(name: &str, token: Self) -> Self {
        Self::Assignment(name.to_string(), Rc::from(token))
    }

    pub(crate) fn new_add(left: Self, right: Self) -> Self {
        Self::Express(Opcode::Add, Rc::from(left), Rc::from(right))
    }

    pub(crate) fn new_subtract(left: Self, right: Self) -> Self {
        Self::Express(Opcode::Subtract, Rc::from(left), Rc::from(right))
    }

    pub(crate) fn new_multiply(left: Self, right: Self) -> Self {
        Self::Express(Opcode::Multiply, Rc::from(left), Rc::from(right))
    }

    pub(crate) fn new_divide(left: Self, right: Self) -> Self {
        Self::Express(Opcode::Divide, Rc::from(left), Rc::from(right))
    }

    pub(crate) fn new_value(value: Vals) -> Self {
        Self::Value(value)
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Comma => write!(f, ","),
            Self::Express(opcode, left, right) => match opcode {
                Opcode::Add => write!(f, "{} + {}", left, right),
                Opcode::Subtract => write!(f, "{} - {}", left, right),
                Opcode::Multiply => write!(f, "{} * {}", left, right),
                Opcode::Divide => write!(f, "{} / {}", left, right)
            }
            Self::Value(x) => write!(f, "{}", x),
            Self::Ident(id) => write!(f, "{}", id),
            Self::Assignment(k, v) => write!(f, "{} = {}", k, v.as_ref()),
            Self::CallFunction(name, args) => write!(f, "{}({})", name.as_ref(), args.as_ref().iter().map(|i| format!("{}", i)).collect::<Vec<String>>().join(","))
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) enum Vals {
    Float(i32, i32),
    Integer(i32),
    String(String),
    Invalid,
}

impl Add for Vals {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Self::Float(i, d) => match rhs {
                Self::Float(i2, d2) => Vals::Float(i + i2, d + d2),
                Self::Integer(i2) => Self::Float(i + i2, d),
                Self::String(v2) => Vals::String(format!("{}{}{}", i, d, v2)),
                Self::Invalid => Vals::Invalid,
            },
            Self::String(v) => Vals::String(format!("{}{}", v, rhs)),
            Self::Integer(v) => match rhs {
                Self::Float(i, d) => Self::Float(v + i, d),
                Self::Integer(i) => Self::Integer(v + i),
                Self::String(v2) => Self::String(format!("{}{}", v, v2)),
                Self::Invalid => Self::Invalid,
            },
            Self::Invalid => Vals::Invalid,
        }
    }
}

impl Sub for Vals {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match self {
            Self::Float(i, d) => match rhs {
                Self::Float(i2, d2) => Vals::Float(i - i2, d - d2),
                Self::String(v2) => Vals::Float(i - i32::from_str(v2.as_ref()).unwrap_or(0i32), d),
                Self::Invalid => Vals::Invalid,
                _ => Self::Invalid,
            },
            Self::Integer(i) => match rhs {
                Self::Float(i2, d2) => Self::Float(i - i2, d2),
                Self::Integer(i2) => Self::Integer(i - i2),
                Self::String(_) => Self::Invalid,
                Self::Invalid => Self::Invalid,
            },
            Self::String(v) => match rhs {
                Self::Float(i2, d2) => Vals::String(v.replace(&format!("{}{}", i2, d2), "")),
                Self::Integer(i2) => Self::String(v.replace(&format!("{}", i2), "")),
                Self::String(v2) => Vals::String(v.replace(&v2, "")),
                Self::Invalid => Vals::Invalid,
            },
            Self::Invalid => Vals::Invalid,
        }
    }
}

impl Mul for Vals {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match self {
            Self::Float(i, d) => match rhs {
                Self::Float(i2, d2) => Vals::Float(i * i2, d * d2),
                Self::Integer(i2) => Self::Float(i * i2, d),
                Self::String(v2) => Vals::Float(i * i32::from_str(v2.as_ref()).unwrap_or(0i32), d),
                Self::Invalid => Vals::Invalid,
            },
            Self::Integer(i) => match rhs {
                Self::Float(i2, d2) => Self::Float(i * i2, d2),
                Self::Integer(i2) => Self::Integer(i * i2),
                Self::String(v) => Self::String(v.repeat(i as usize)),
                Self::Invalid => Self::Invalid,
            },
            Self::String(v) => match rhs {
                Self::Float(_, _) => Self::Invalid,
                Self::Integer(v2) => Self::String(v.repeat(v2 as usize)),
                Self::String(_) => Self::Invalid,
                Self::Invalid => Self::Invalid
            },
            Self::Invalid => Vals::Invalid,
        }
    }
}

impl Div for Vals {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match self {
            Self::Float(i, d) => match rhs {
                Self::Float(i2, d2) => Vals::Float(i / i2, d / d2),
                Self::Integer(i2) => Self::Float(i / i2, d),
                Self::String(v2) => Vals::Float(i / i32::from_str(v2.as_ref()).unwrap_or(1i32), d),
                Self::Invalid => Vals::Invalid,
            },
            Self::Integer(i) => match rhs {
                Self::Float(i2, d2) => Self::Float(i / i2, d2),
                Self::Integer(i2) => Self::Integer(i / i2),
                Self::String(_) => Self::Invalid,
                Self::Invalid => Self::Invalid,
            },
            Self::String(v) => Vals::String(format!("{}/{}", v, rhs)),
            Self::Invalid => Vals::Invalid,
        }
    }
}

impl Display for Vals {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Float(i, d) => write!(f, "{}.{}", i, d),
            Self::Integer(i) => write!(f, "{}", i),
            Self::String(v) => write!(f, "\"{}\"", v),
            Self::Invalid => write!(f, "Invalid!"),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum Program {
    Entry(Vec<Token>),
    Function{name: Token, args: Vec<Token>, sentences: Vec<Token>},
    Global(Token),
}

impl Program {
    pub(crate) fn new_function(name: Token, args: Vec<Token>, sentences: Vec<Token>) -> Self {
        Self::Function {
            name,
            args,
            sentences,
        }
    }

    pub(crate) fn new_entry(sentences: Vec<Token>) -> Self {
        Self::Entry (sentences)
    }

    pub(crate) fn new_global(name: &str, token: Token) -> Self {
        Self::Global(Token::new_assignment(name, token))
    }
}

pub(crate) type Tree = Vec<Program>;