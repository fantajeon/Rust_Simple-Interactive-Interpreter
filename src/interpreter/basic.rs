use std::rc::Rc;

macro_rules! is_enum_variant {
    ($v:expr, $p:pat) => (
        if let $p = $v { true } else { false }
    );
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
            (Value::IntNumber(x), Value::IntNumber(y)) => Some(Value::IntNumber( x+y )),
            (Value::FloatNumber(x), Value::IntNumber(y)) => Some(Value::FloatNumber( x+(*y as f32) )),
            (Value::FloatNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( x+y )),
            (Value::IntNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( (*x as f32)+y )),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum Node {
    None,
    FunctionDef{ 
        name: String, 
        params: Vec<Node>,
        body: Rc<Node>,
    },
    FunctionCall {
        name: String, 
        params: Vec<Node>,
    },
    Assign {
        left: Box<Node>, 
        right: Box<Node>,
    },
    BinOp{ 
        left: Box<Node>, 
        op: String,
        right: Box<Node>,
    },
    Num{ value: Value },
    Identifier{ value: String },
}

impl Node {
    pub fn identity_value(self) -> Option<String> {
        match self {
            Node::Identifier { value } => Some(value),
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