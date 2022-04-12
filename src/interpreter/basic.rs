macro_rules! is_enum_variant {
    ($v:expr, $p:pat) => (
        if let $p = $v { true } else { false }
    );
}

#[derive(Debug, Clone, PartialEq)]
pub enum Kind {
    None,
    Eof,
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

    pub fn is_op<F>(&self, pred: F) -> bool 
    where F: FnOnce(&str) -> bool
    {
        match self {
            Kind::Op(v) => return pred( &v ),
            _ => return false,
        }
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
            (Value::IntNumber(x), Value::IntNumber(y)) => Some(Value::FloatNumber( (*x as f32)/(*y as f32) )),
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
    pub raw_string: String,
    pub value: Value,
}

impl Token {
    pub fn new(v: Kind, raw_string: &str) -> Self {
        Token{
            kind: Box::new(v), 
            raw_string: raw_string.to_string(), 
            value: Value::None
        }
    }

    pub fn is_eof(&self) -> bool {
        is_enum_variant!(*self.kind, Kind::Eof)
    }

    pub fn is_letter(&self) -> bool {
        is_enum_variant!(*self.kind, Kind::Letter(_))
    }
    
    pub fn take(&mut self) -> Token {
        std::mem::replace(self,Token::default())
    }

    pub fn new_eof() -> Token {
        Token{
            kind: Box::new(Kind::Eof), 
            raw_string: "[EOF]".to_string(), 
            value: Value::None
        }
    }
}

impl Default for Token {
    fn default() -> Self { 
        Token{kind: Box::new(Kind::None), raw_string: "".to_string(), value: Value::None}
    }
}