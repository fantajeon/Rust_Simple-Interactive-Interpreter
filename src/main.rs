use regex::Regex;
use std::{collections::{HashMap, VecDeque}, rc::Rc, cell::RefCell};

/*
stat            ::= function | expression
function        ::= fn-keyword fn-name { identifier } fn-operator expression
fn-name         ::= identifier
fn-operator     ::= '=>'
fn-keyword      ::= 'fn'

expression      ::= factor | expression operator expression
factor          ::= number | identifier | assignment | '(' expression ')' | function-call
assignment      ::= identifier '=' expression
function-call   ::= fn-name { expression }

operator        ::= '+' | '-' | '*' | '/' | '%'

identifier      ::= letter | '_' { identifier-char }
identifier-char ::= '_' | letter | digit

number          ::= { digit } [ '.' digit { digit } ]

letter          ::= 'a' | 'b' | ... | 'y' | 'z' | 'A' | 'B' | ... | 'Y' | 'Z'
digit           ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
*/
macro_rules! is_enum_variant {
    ($v:expr, $p:pat) => (
        if let $p = $v { true } else { false }
    );
}

#[derive(Debug, Clone, PartialEq)]
enum Kind {
    None,
    FloatNumber(f32),
    IntNumber(i32),
    Letter(String),
    Op(String),
    Keyword(String),
}

impl Kind {
    fn op<'a>(&self) -> Option<&String> {
        match self {
            Kind::Op(v) => Some(v),
            _ => None,
        }
    }

    fn take_letter(&mut self) -> Option<String> {
        match self {
            Kind::Letter(v) => {
                Some(std::mem::replace(v, "".to_string()))
            },
            _ => None,
        }
    }

    fn take_op(&mut self) -> Option<String> {
        match self {
            Kind::Op(v) => {
                Some(std::mem::replace(v, "".to_string()))
            },
            _ => None,
        }
    }

    fn is_op(&self) -> bool {
        is_enum_variant!(self, Kind::Op(_))
    }
}

#[derive(Debug, Clone)]
enum Value {
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
    fn plus(&self, b: &Value) -> Option<Value> {
        match (self, b) {
            (Value::IntNumber(x), Value::IntNumber(y)) => Some(Value::IntNumber( x+y )),
            (Value::FloatNumber(x), Value::IntNumber(y)) => Some(Value::FloatNumber( x+(*y as f32) )),
            (Value::FloatNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( x+y )),
            (Value::IntNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( (*x as f32)+y )),
            _ => None,
        }
    }
    fn modulus(&self, b: &Value) -> Option<Value> {
        match (self, b) {
            (Value::IntNumber(x), Value::IntNumber(y)) => Some(Value::IntNumber( x%y )),
            (Value::FloatNumber(x), Value::IntNumber(y)) => Some(Value::FloatNumber( x%(*y as f32) )),
            (Value::FloatNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( x%y )),
            (Value::IntNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( (*x as f32)%y )),
            _ => None,
        }
    }
    fn multiply(&self, b: &Value) -> Option<Value> {
        match (self, b) {
            (Value::IntNumber(x), Value::IntNumber(y)) => Some(Value::IntNumber( x*y )),
            (Value::FloatNumber(x), Value::IntNumber(y)) => Some(Value::FloatNumber( x*(*y as f32) )),
            (Value::FloatNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( x*y )),
            (Value::IntNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( (*x as f32)*y )),
            _ => None,
        }
    }
    fn divide(&self, b: &Value) -> Option<Value> {
        match (self, b) {
            (Value::IntNumber(x), Value::IntNumber(y)) => Some(Value::IntNumber( x/y )),
            (Value::FloatNumber(x), Value::IntNumber(y)) => Some(Value::FloatNumber( x/(*y as f32) )),
            (Value::FloatNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( x/y )),
            (Value::IntNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( (*x as f32)/y )),
            _ => None,
        }
    }
    fn minus(&self, b: &Value) -> Option<Value> {
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
enum Node {
    None,
    FunctionDef{ 
        name: String, 
        params: Vec<Token>,
        body: Rc<Node>,
    },
    FunctionCall {
        name: String, 
        params: Vec<Node>,
    },
    BinOp{ 
        left: Box<Node>, 
        op: String,
        right: Box<Node>,
    },
    Num{ value: Value },
    Identifier{ value: String },
}

#[derive(Debug)]
struct Token {
    pub kind: Box<Kind>,
    pub value: Value,
}

impl Token {
    fn new(v: Kind) -> Self {
        Token{kind: Box::new(v), value: Value::None}
    }

    fn is_none(&self) -> bool {
        is_enum_variant!(*self.kind, Kind::None)
    }
    fn is_op(&self) -> bool {
        is_enum_variant!(*self.kind, Kind::Op(_))
    }

    fn take(&mut self) -> Token {
        std::mem::replace(self,Token::default())
    }

    fn replace(&mut self, tok: Token) -> Token {
        std::mem::replace(self, tok)
    }

    fn take_if<F: FnOnce(&Self) -> bool>(&mut self, predicate: F) -> Option<Self> {
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

#[derive(Debug)]
struct Parser {
    curr_token: Token,
    input: VecDeque<Token>,
}
impl Default for Parser {
    fn default() -> Self { 
        Parser { curr_token: Token::default(), input: VecDeque::new() }
    }
}
impl Parser {
    fn new() -> Self {
        Parser { 
            curr_token: Token::default(),
            input: VecDeque::new(),
        }
    }

    fn shift_input(&mut self) -> Token {
        let new = self.input.pop_front().unwrap_or(Token::default());
        println!("new_token! {:?}", new);
        return std::mem::replace( &mut self.curr_token, new);
    }

    fn _function_parameter(&mut self) -> Result<Vec<Token>, String> {
        let mut result: Vec<Token> = Vec::new();
        while is_enum_variant!(&*self.curr_token.kind, Kind::Letter(_)) {
            let ct = self.curr_token.take();
            result.push(ct);
            self.shift_input();
        }
        return Ok(result);
    }

    fn _function_expression(&mut self) -> Result<Node, String> {
        if let Kind::Op(v) = &*self.curr_token.kind {
            if v == "=>" {
                self.shift_input();
                return self._expression();
            }
        }
        return Err("Syntax Error! function Expression".to_string());
    }

    fn _function(&mut self) -> Result<Node,String> {
        if let Kind::Letter(fn_name) = &*self.curr_token.kind {
            println!("fn-name: {}", fn_name);
            let mut ct = self.curr_token.take();
            self.shift_input();
            let result = Node::FunctionDef { 
                name: (*ct.kind).take_letter().unwrap(), 
                params: self._function_parameter()?, 
                body: Rc::new(self._function_expression()?), 
            };
            return Ok(result);
        }
        return Err("Syntax Error! function must have fn-name".to_string());
    }

    fn _factor(&mut self) -> Result<Node,String> {
        println!("factor! {:?}", self.curr_token);
        match &*self.curr_token.kind {
            Kind::FloatNumber(v) => {
                let n = Node::Num { value: Value::FloatNumber(*v) };
                self.shift_input();
                return Ok(n);
            },
            Kind::IntNumber(v) => {
                let n = Node::Num { value: Value::IntNumber(*v) };
                self.shift_input();
                return Ok(n);
            },
            Kind::Letter(var) => {
                let n = Node::Identifier { value: var.clone() };
                self.shift_input();
                let mut ct = self.curr_token.take();
                if let Kind::Op(v) = &*ct.kind {
                    if v == "=" {
                        self.shift_input();
                        let expr_node = self._expression()?;
                        let node = Node::BinOp { 
                            left: Box::new(n), 
                            op: ct.kind.take_op().unwrap(),
                            right: Box::new(expr_node) 
                        };
                        return Ok(node);
                    } 
                } 
                self.curr_token.replace(ct);
                return Ok(n);
            },
            Kind::Op(v) => {
                if v == "(" {
                    self.shift_input();
                    let n = self._expression()?;
                    if let Kind::Op(v) = &*self.curr_token.kind {
                        if v != ")" {
                            return Err("Expected String: )".to_string());
                        }
                    }
                    self.shift_input();
                    return Ok(n);
                } else {
                    return Err("Unknown Syntax".to_string());
                }
            },
            _ => { },
        }
        println!("Factor Error!");
        return Err("Unknown Rules!".to_string());
    }

    fn _term(&mut self) -> Result<Node,String> {
        println!("term! {:?}", self.curr_token);
        let result = self._factor()?;
        if let Some(mut tok) = self.curr_token.take_if(|v| v.kind.is_op()) {
            let v = tok.kind.op().unwrap();
            if v == "*" || v == "/" || v == "%" {
                self.shift_input();
                let right = self._term()?;
                let n = Node::BinOp { 
                    left: Box::new(result), 
                    op: tok.kind.take_op().unwrap(),
                    right: Box::new(right),
                };
                return Ok(n);
            } else {
                self.curr_token.replace(tok);
            }
        }
        return Ok(result);
    }

    fn _expression(&mut self) -> Result<Node,String> {
        println!("expression entry");
        let mut result = self._term()?;

        println!("expression, curr_token={:?}", self.curr_token);
        if let Some(mut tok) = self.curr_token.take_if(|t| 
            is_enum_variant!(*t.kind, Kind::Op(_))
        ) {
            println!("BinOps: {:?}", tok);
            let v = (*tok.kind).op().unwrap();
            if v == "+" || v == "-" {
                self.shift_input();
                result = Node::BinOp { 
                    left: Box::new(result), 
                    op: tok.kind.take_op().unwrap(),
                    right: Box::new(self._expression()?), 
                };
            } else {
                self.curr_token.replace(tok);
            }
        }
        return Ok(result);
    }

    fn stmt(&mut self) -> Result<Node,String> {
        println!("start stmt with curr_token: {:?}, input={:?}", self.curr_token, self.input);
        match &*self.curr_token.kind {
            Kind::Keyword(v) => {
                println!("keyword={}", v);
                if v != "fn" {
                    return Err("syntax error!".to_string());
                } 
                self.shift_input();
                return self._function();
            },
            _ => {
                return self._expression();
            },
        }
    }

    fn parse(&mut self, token: VecDeque<Token>, e: &mut Interpreter) -> Result<Vec<Rc<Node>>,String> 
    {
        self.input = token;
        self.shift_input();
        let mut ast: Vec<Rc<Node>> = Vec::new();
        loop {
            if self.curr_token.is_none() {
                println!("EOF!");
                break;
            }
            let n = self.stmt()?;
            e.evaluate(&n);
            ast.push(Rc::new(n));
        }
        return Ok(ast);
    }
}
#[derive(Debug)]
struct Interpreter {
    vars: HashMap<String,Value>,
    func: HashMap<String,Rc<Node>>,
    ast: Vec<Rc<Node>>,
    line: u32,
}

fn lexer(express: &str) -> VecDeque<Token> {
    let re = Regex::new( r"(?x)
        (?P<fn_op>=>)              # fn_operator
        |(?P<ops>[\-+*/%=\(\)])    # ops
        |(?P<letter>[A-Za-z_][A-Za-z0-9_]*) # letter
        |(?P<float>[+\-]?[0-9]*\.[0-9]+)  # float
        |(?P<int>[+\-]?[0-9]+)          # digital
        |(?P<whitespace>\s+)    # whitespace
        ").unwrap();
    let mut toks: VecDeque<Token> = VecDeque::new();
    for cap in re.captures_iter(express) {
        //println!("cap={:?}", cap);
        let tok = re.capture_names()
            .flatten()
            .filter_map(|n| {
                return Some((n,cap.name(n)?.as_str().to_string()));
            })
            .take(1).next().unwrap();
        //println!("tok={:?}", tok);
        let t: Option<Token> = match tok.0 {
            "int" => Some( Token::new( Kind::IntNumber(tok.1.parse::<i32>().unwrap())) ),
            "float" => Some( Token::new( Kind::FloatNumber(tok.1.parse::<f32>().unwrap())) ),
            "letter" => if tok.1 == "fn" {
                Some( Token::new( Kind::Keyword(tok.1)) )
            } else {
                Some( Token::new( Kind::Letter(tok.1)) )
            },
            "ops" | "fn_op" => Some( Token::new( Kind::Op(tok.1)) ),
            _ => None,
        };
        //let v: String = cap[0].trim().into();
        if t.is_some() {
            toks.push_back(t.unwrap());
        }
    }

    return toks;
}

impl Interpreter {
    fn new() -> Self {
        Interpreter { 
            vars: HashMap::new(), 
            func: HashMap::new(), 
            ast: Vec::new(),
            line: 0,
        }
    }

    fn input(&mut self, input: &str) -> Result<Option<f32>, String> {
        self.line += 1;
        println!("tokenizing={} @ line{}", input, self.line);
        let tokens = lexer(input);
        println!("tokens: {:?}", tokens);

        let mut parser = Parser::new();

        parser.parse( tokens, self )?;

        return Ok(None);
    }

    pub fn evaluate(&mut self, ast: &Node) -> Result<(),String> {
        println!("AST=>>>>{:?}", ast);
        println!("result(AST)={:?}", self.visit(ast));
        println!("(fn)={:?}", self.func);
        return Ok(());
    }

    fn visit(&mut self, n: &Node) -> Result<Value,String> {
        match n {
            Node::BinOp{left, op, right} => { 
                let a = self.visit(left)?;
                let b = self.visit(right)?;
                println!("DEBUG(BinOp) = {:?}, {:?}, op={:?}", a, b, op);
                let r = match op.as_str() {
                    "+" => a.plus(&b),
                    "-" => a.minus(&b),
                    "/" => a.divide(&b),
                    "*" => a.multiply(&b),
                    "%" => a.modulus(&b),
                    "=" => {
                        let var_value: Option<Value> = match a {
                            Value::String(var_name) =>  {
                                println!("DEBUG(var table) = {:?}, {:?}", var_name, b);
                                self.vars.insert(var_name, b.clone());
                                Some(b)
                            },
                            _ => {
                                None
                            }
                        };
                        var_value
                    },
                    _ => {
                        None
                    },
                }.unwrap();
                return Ok(r);
            },
            Node::FunctionDef{name, params, body} => {
                self.func.insert( name.to_owned(), Rc::clone( body ) );
                return Ok(Value::None);
            },
            Node::FunctionCall{name, params} => {},
            Node::Num{value} => {return Ok(value.to_owned());},
            Node::Identifier{value} => {
                let v = self.vars.get(value);
                if v.is_some() {
                    return Ok( v.unwrap().clone() );
                }
                return Ok( Value::String(value.to_owned()) );
            },
            _ => {},
        }
        return Ok(Value::None);
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

#[test]
fn test_basic_arithmetic() {
    let mut i = Interpreter::new();
    //i.input("1 + 1");
    //i.input("a + b + c + 1");
    i.input("fn avg a b c => a + b + c + 1");
    //i.input("avg a b c");
    //i.input(".1 + 1");
    //i.input("2 - 1");
    //i.input("2 * 3");
    //i.input("8 + 4 / 3 + (4 *2) % 3");
    //i.input("i = 4 / 3 + (4 *2) % 3");
    //i.input("7 % 4");
}


#[test]
fn basic_arithmetic() {
    let mut i = Interpreter::new();
    assert_eq!(i.input("1 + 1"), Ok(Some(2.0)));
    assert_eq!(i.input("2 - 1"), Ok(Some(1.0)));
    assert_eq!(i.input("2 * 3"), Ok(Some(6.0)));
    assert_eq!(i.input("8 / 4"), Ok(Some(2.0)));
    assert_eq!(i.input("7 % 4"), Ok(Some(3.0)));
}

#[test]
fn variables() {
    let mut i = Interpreter::new();
    assert_eq!(i.input("x = 1"), Ok(Some(1.0)));
    assert_eq!(i.input("x"), Ok(Some(1.0)));
    assert_eq!(i.input("x + 3"), Ok(Some(4.0)));
    assert!(i.input("y").is_err());
}

#[test]
fn functions() {
    let mut i = Interpreter::new();
    assert_eq!(i.input("fn avg x y => (x + y) / 2"), Ok(None));
    assert_eq!(i.input("avg 4 2"), Ok(Some(3.0)));
    assert!(i.input("avg 7").is_err());
    assert!(i.input("avg 7 2 4").is_err());
}

#[test]
fn conflicts() {
    let mut i = Interpreter::new();
    assert_eq!(i.input("x = 1"), Ok(Some(1.0)));
    assert_eq!(i.input("fn avg x y => (x + y) / 2"), Ok(None));
    assert!(i.input("fn x => 0").is_err());
    assert!(i.input("avg = 5").is_err());
}

fn main() {

}