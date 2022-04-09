use std::{rc::Rc, collections::{VecDeque, binary_heap::Iter}};

macro_rules! is_enum_variant {
    ($v:expr, $p:pat) => (
        if let $p = $v { true } else { false }
    );
}

pub enum KindValue {
    String(String),
    IntNumber(i32),
    FloatNumber(f32),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Kind {
    None,
    FloatNumber(f32),
    IntNumber(i32),
    Letter(String),
    Op(String),
    Keyword(String),
    LPAREN,
    RPAREN,
    ASSIGN,
    FNOP,
}

impl Kind {
    pub fn op<'a>(&self) -> Option<&String> {
        match self {
            Kind::Op(v) => Some(v),
            _ => None,
        }
    }
    
    pub fn take_value(&mut self) -> Option<KindValue> {
        match self {
            Kind::Letter(v) => {
                Some(KindValue::String(std::mem::replace(v, "".to_string())))
            },
            Kind::IntNumber(v) => Some(KindValue::IntNumber(*v)),
            Kind::FloatNumber(v) => Some(KindValue::FloatNumber(*v)),
            _ => None,
        }

    }

    pub fn take_letter(&mut self) -> Option<String> {
        match self {
            Kind::Letter(v) => {
                Some(std::mem::replace(v, "".to_string()))
            },
            _ => None,
        }
    }

    pub fn take_op(&mut self) -> Option<String> {
        match self {
            Kind::Op(v) => {
                Some(std::mem::replace(v, "".to_string()))
            },
            _ => None,
        }
    }

    pub fn is_op(&self) -> bool {
        is_enum_variant!(self, Kind::Op(_))
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    None,
    Tuple(VecDeque<Rc<Value>>),
    IntNumber(i32),
    FloatNumber(f32),
    String(String),
}

impl From<&str> for Value {
    fn from(v: &str) -> Self {
        Self::String(v.to_string())
    }
}

impl From<f32> for Value {
    fn from(v: f32) -> Self {
        Self::FloatNumber(v)
    }
}

impl From<i32> for Value {
    fn from(v: i32) -> Self {
        Self::IntNumber(v)
    }
}

impl Value {
    pub fn get_tuple(&mut self) -> Option<&mut VecDeque<Rc<Value>>> {
        if let Value::Tuple(v) = self {
            return Some(v);
        }
        return None;
    }

    pub fn get_result(&self) -> Option<f32> {
        match self {
            Value::IntNumber(v) => return Some(*v as f32),
            Value::FloatNumber(v) => return Some(*v),
            _ => return None,
        }
    }

    pub fn plus(&self, b: &Value) -> Option<Value> {
        match (self, b) {
            (Value::IntNumber(x), Value::IntNumber(y)) => Some(Value::IntNumber( x+y )),
            (Value::FloatNumber(x), Value::IntNumber(y)) => Some(Value::FloatNumber( x+(*y as f32) )),
            (Value::FloatNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( x+y )),
            (Value::IntNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( (*x as f32)+y )),
            _ => None,
        }
    }

    pub fn modulus(&self, b: &Value) -> Option<Value> {
        match (self, b) {
            (Value::IntNumber(x), Value::IntNumber(y)) => Some(Value::IntNumber( x%y )),
            (Value::FloatNumber(x), Value::IntNumber(y)) => Some(Value::FloatNumber( x%(*y as f32) )),
            (Value::FloatNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( x%y )),
            (Value::IntNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( (*x as f32)%y )),
            _ => None,
        }
    }

    pub fn multiply(&self, b: &Value) -> Option<Value> {
        match (self, b) {
            (Value::IntNumber(x), Value::IntNumber(y)) => Some(Value::IntNumber( x*y )),
            (Value::FloatNumber(x), Value::IntNumber(y)) => Some(Value::FloatNumber( x*(*y as f32) )),
            (Value::FloatNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( x*y )),
            (Value::IntNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( (*x as f32)*y )),
            _ => None,
        }
    }

    pub fn divide(&self, b: &Value) -> Option<Value> {
        match (self, b) {
            (Value::IntNumber(x), Value::IntNumber(y)) => Some(Value::IntNumber( x/y )),
            (Value::FloatNumber(x), Value::IntNumber(y)) => Some(Value::FloatNumber( x/(*y as f32) )),
            (Value::FloatNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( x/y )),
            (Value::IntNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( (*x as f32)/y )),
            _ => None,
        }
    }

    pub fn minus(&self, b: &Value) -> Option<Value> {
        match (self, b) {
            (Value::IntNumber(x), Value::IntNumber(y)) => Some(Value::IntNumber( x-y )),
            (Value::FloatNumber(x), Value::IntNumber(y)) => Some(Value::FloatNumber( x-(*y as f32) )),
            (Value::FloatNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( x-y )),
            (Value::IntNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( (*x as f32)-y )),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct Token {
    pub kind: Box<Kind>,
    pub value: Value,
}

impl Token {
    pub fn new(v: Kind) -> Self {
        Token{kind: Box::new(v), value: Value::None}
    }

    pub fn is_none(&self) -> bool {
        is_enum_variant!(*self.kind, Kind::None)
    }

    pub fn is_op(&self) -> bool {
        is_enum_variant!(*self.kind, Kind::Op(_))
    }

    pub fn is_letter(&self) -> bool {
        is_enum_variant!(*self.kind, Kind::Letter(_))
    }
    
    pub fn is_numbers(&self) -> bool {
        is_enum_variant!(*self.kind, Kind::IntNumber(_)) || is_enum_variant!(*self.kind, Kind::FloatNumber(_))
    }

    pub fn take(&mut self) -> Token {
        std::mem::replace(self,Token::default())
    }

    pub fn replace(&mut self, tok: Token) -> Token {
        std::mem::replace(self, tok)
    }

    pub fn take_if<F: FnOnce(&Self) -> bool>(&mut self, predicate: F) -> Option<Self> {
        if predicate(self) {
            Some(std::mem::replace(self,Token::default()))
        } else {
            None
        }
    }
}

impl Default for Token {
    fn default() -> Self { 
        Token{kind: Box::new(Kind::None), value: Value::None}
    }
}