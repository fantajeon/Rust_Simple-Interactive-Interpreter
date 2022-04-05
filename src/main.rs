use regex::Regex;
use std::{collections::{HashMap, VecDeque}};

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
    fn op<'a>(self) -> Option<String> {
        match self {
            Kind::Op(v) => Some(v),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
enum Value {
    None,
    IntNumber(i32),
    FloatNumber(f32),
}

#[derive(Debug)]
enum Node {
    None,
    Function{ 
        name: Token, 
        params: Box<Vec<Token>>, 
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

#[derive(Debug)]
struct Token {
    pub kind: Box<Kind>,
    pub value: Value,
}

impl Token {
    fn new(v: Kind) -> Self {
        Token{kind: Box::new(v), value: Value::None}
    }

    fn take(&mut self) -> Token {
        std::mem::replace(self,Token::default())
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
struct Interpreter {
    vars: HashMap<String,i32>,
    curr_token: Token,
    input: VecDeque<Token>,
    ast: Vec<Node>,
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
            .filter_map(|n| Some((n,cap.name(n)?.as_str().to_string())))
            .take(1).next().unwrap();
        println!("tok={:?}", tok);
        let t: Option<Token> = match tok.0 {
            "int" => Some( Token::new( Kind::IntNumber(tok.1.parse::<i32>().unwrap())) ),
            "float" => Some( Token::new( Kind::FloatNumber(tok.1.parse::<f32>().unwrap())) ),
            "letter" => Some( Token::new( Kind::Letter(tok.1)) ),
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
            curr_token: Token::default(),
            input: VecDeque::new(),
            ast: Vec::new(),
        }
    }

    fn input(&mut self, input: &str) -> Result<Option<f32>, String> {
        println!("tokenizing={}", input);
        let mut tokens = lexer(input);
        println!("tokens: {:?}", tokens);
        //if tokens.len() > 2 && tokens[1] == "" {
        //    let new_var: &String = &tokens[0];
        //    self.vars.insert(new_var.into(), 1);
        //}
        //unimplemented!()
        self.parse(tokens);
        println!("AST={:?}", self.ast);
        return Err("not implemented!".to_string());
    }

    fn pop_token(&mut self) -> Token {
        let new = self.input.pop_front().unwrap_or(Token::default());
        println!("new_token! {:?}", new);
        return std::mem::replace( &mut self.curr_token, new);
    }

    fn _function(&mut self) -> Result<Node,String> {
        return Ok(Node::None);
    }

    fn _factor(&mut self) -> Result<Node,String> {
        println!("factor! {:?}", self.curr_token);
        match &*self.curr_token.kind {
            Kind::FloatNumber(v) => {
                let n = Node::Num { value: Value::FloatNumber(*v) };
                self.pop_token();
                return Ok(n);
            },
            Kind::IntNumber(v) => {
                let n = Node::Num { value: Value::IntNumber(*v) };
                self.pop_token();
                return Ok(n);
            },
            Kind::Letter(var) => {
                let n = Node::Identifier { value: var.clone() };
                self.pop_token();
                let ct = self.curr_token.take();
                if let Kind::Op(v) = &*ct.kind {
                    if v == "=" {
                        self.pop_token();
                        let expr_node = self._expression()?;
                        let node = Node::BinOp { 
                            left: Box::new(n), 
                            op: v.clone(),
                            right: Box::new(expr_node) 
                        };
                        return Ok(node);
                    } 
                } else {
                    self.curr_token = ct;
                }
                return Ok(n);
            },
            Kind::Op(v) => {
                if v == "(" {
                    self.pop_token();
                    let n = self._expression()?;
                    if let Kind::Op(v) = &*self.curr_token.kind {
                        if v != ")" {
                            return Err("Expected String: )".to_string());
                        }
                    }
                    self.pop_token();
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

    fn _expression(&mut self) -> Result<Node,String> {
        let result = self._factor()?;
        if let Some(v) = self.curr_token.take_if(|t| 
            is_enum_variant!(*t.kind, Kind::Op(_))
        ) {
            println!("BinOps: {:?}, result={:?}", v, result);
            let t = (*v.kind).op().unwrap();
            self.pop_token();
            let n = Node::BinOp { 
                left: Box::new(result), 
                op: t,
                right: Box::new(self._expression()?), 
            };

            return Ok(n);
        } 
        return Ok(result);
    }

    fn stmt(&mut self) -> Result<Node,String> {
        println!("start stmt with curr_token: {:?}, input={:?}", self.curr_token, self.input);
        self.pop_token();
        match &*self.curr_token.kind {
            Kind::Keyword(v) => {
                println!("keyword={}", v);
                if v != "fn" {
                    return Err("syntax error!".to_string());
                } 
                self.pop_token();
                return self._function();
            },
            _ => {
                return self._expression();
            },
        }
    }

    fn parse(&mut self, mut token: VecDeque<Token>) -> Result<(),String> {
        self.input = token;
        self.ast = Vec::new();
        while self.input.len() > 0 {
            let n = self.stmt()?;
            self.ast.push(n);
        }
        return Ok(());
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
    //i.input("=> fn avg a + 1");
    //i.input(".1 + 1");
    //i.input("2 - 1");
    //i.input("2 * 3");
    i.input("8 / 4 + 3");
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