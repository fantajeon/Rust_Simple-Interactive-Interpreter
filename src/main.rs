mod interpreter;
use std::{collections::{HashMap, VecDeque}, rc::Rc};

use interpreter::*;


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

#[derive(Debug)]
pub enum SimKindValue {
    None,
    Value{
        value: Rc<Value>,
    },
    Tuple{
        value: Rc<Value>,
    },
    Function{
        body: Rc<Node>,
        params: Vec<String>,
    },
}
#[derive(Debug)]
 pub struct SymValue {
    name: String,
    kind_value: SimKindValue,
}

impl SymValue {
    pub fn get_name(&self) -> &String {
        return &self.name;
    }

    pub fn get_tuple(&self) -> Option<Rc<Value>> {
        if let SimKindValue::Tuple{value} = &self.kind_value {
            return Some( Rc::clone(value) );
        }
        return None;
    }
    
    pub fn get_value(&self) -> Option<Rc<Value>> {
        if let SimKindValue::Value{value} = &self.kind_value {
            return Some( Rc::clone(value) );
        }
        return None;
    }

    pub fn new_tuple(name: &str, value: Rc<Value>) -> Self {
        SymValue{ name: name.to_string(), kind_value: SimKindValue::Tuple { value: value }}
    }

    pub fn new_value(name: &str, value: Rc<Value>) -> Self {
        SymValue{ name: name.to_string(), kind_value: SimKindValue::Value{value: value} }
    }

    pub fn new_function(name: &str, params: Vec<String>, node: Rc<Node>) -> Self {
        SymValue{ 
            name: name.to_string(), 
            kind_value: SimKindValue::Function{
                params: params,
                body: node
            } 
        }
    }
}

#[derive(Debug)]
struct ScopeSymbolTable {
    pub parent: Option<Box<ScopeSymbolTable>>,
    pub symbols: HashMap<String,Rc<SymValue>>,
}

impl ScopeSymbolTable {
    pub fn new(parent: Option<Box<ScopeSymbolTable>>) -> Self {
        ScopeSymbolTable { parent: parent, symbols: HashMap::new() }
    }

    pub fn take_parent(&mut self) -> Option<Box<ScopeSymbolTable>> {
        let old = std::mem::replace(&mut self.parent, None);
        return old;
    }

    pub fn insert(&mut self, sym: SymValue) {
        self.symbols.insert( sym.name.to_string(), Rc::new(sym) );
    }

    pub fn lookup(&self, sym: &String) -> Option<Rc<SymValue>> {
        if let Some(v) = self.symbols.get(sym) {
            return Some(Rc::clone(v));
        }

        if let Some(parent) = &self.parent {
            return parent.lookup(sym);
        }

        return None;
    }
}
#[derive(Debug)]
struct Interpreter {
    pub func: HashMap<String,Rc<Node>>,
    pub current_scope: Box<ScopeSymbolTable>,
    //ast: Vec<Rc<Node>>,
    pub line: u32,
}

impl Evaluator for Interpreter {
    fn evaluate(&mut self, ast: &Node) -> Result<Rc<Value>,String> {
        let result = self.visit(ast);
        println!("AST=>>>>{:?}", ast);
        println!("result(AST)={:?}", result);
        println!("(fn)={:?}", self.func);
        return result;
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter { 
            current_scope: Box::new(ScopeSymbolTable::new(None)),
            func: HashMap::new(), 
            //ast: Vec::new(),
            line: 0,
        }
    }

    pub fn input(&mut self, input: &str) -> Result<Option<f32>, String> {
        self.line += 1;
        println!("tokenizing={} @ line{}", input, self.line);
        let tokens = lexer(input);
        println!("tokens: {:?}", tokens);

        let mut parser = Parser::new();

        let result = parser.parse( tokens, self )?;

        return Ok(result);
    }

    fn push_stack_frame(&mut self, mut frame: ScopeSymbolTable) {
        let parent = std::mem::replace( &mut self.current_scope, Box::new(frame) );
        self.current_scope.parent = Some(parent);
    }

    fn pop_stack_frame(&mut self) {
        if let Some(parent) = self.current_scope.take_parent() {
            self.current_scope=parent;
        }
    }

    fn visit(&mut self, n: &Node) -> Result<Rc<Value>,String> {
        match n {
            Node::Assign{left, right} => {
                let var_name =left.identity_value().unwrap();
                let b = self.visit(right)?;
                println!("DEBUG(var table) = {:?}, {:?}", var_name, b);
                self.current_scope.insert(
                        SymValue::new_value(
                            &var_name, 
                            Rc::clone(&b)
                        )
                    );
                return Ok(b);
            },
            Node::BinOp{left, op, right} => { 
                let a = self.visit(left)?;
                let b = self.visit(right)?;
                println!("DEBUG(BinOp) = {:?}, {:?}, op={:?}", a, b, op);
                let r: Rc<Value> = (match op.as_str() {
                    "+" => Some(Rc::new(a.plus(&b).unwrap())),
                    "-" => Some(Rc::new(a.minus(&b).unwrap())),
                    "/" => Some(Rc::new(a.divide(&b).unwrap())),
                    "*" => Some(Rc::new(a.multiply(&b).unwrap())),
                    "%" => Some(Rc::new(a.modulus(&b).unwrap())),
                    _ => None,
                }).unwrap();
                return Ok(r);
            },
            Node::FunctionDef{name, params, body} => {
                println!("create function: {} with {:?}", name, params);
                //self.func.insert( name.to_owned(), Rc::clone( body ) );
                let params: Vec<String> = params.iter().filter_map( |t| {
                    if let Node::Identifier{value, ..} = t {
                        return Some(value.to_string());
                    }
                    None
                }).collect();
                self.current_scope.insert( SymValue::new_function(
                        name,
                        params, 
                        Rc::clone(body) ) );
                return Ok(Rc::new(Value::None));
            },
            Node::Num{value, next} => {
                // next 값이 있을 경우 tuple 처리를 해야한다.
                return Ok( Rc::new(value.to_owned()) );
            },
            Node::Identifier{value, next} => {
                let v = self.current_scope.lookup(value);
                if v.is_none() {
                    //println!("Symbol Table: {:?}", self.current_scope);
                    return Err("Cannot resolve symbol".to_string());
                }
                let next_value = if let Some(n) = next {
                        Some(self.visit(n)?)
                    } else {
                        None
                    };
                let symval = v.unwrap();
                match &symval.kind_value {
                    SimKindValue::Function { body, params } => {
                        let mut frame = ScopeSymbolTable::new(None);
                        //frame.insert(SymValue);
                        
                        println!("!!!!! call function: {:?}, params={:?}", symval, next_value);
                        if let Some(v) = next_value {
                            match &*v {
                                Value::Tuple(ref v) => {
                                    for p in params.iter().zip(v.iter()) {
                                        let sym = SymValue::new_value(p.0.as_str(), Rc::clone(p.1));
                                        frame.insert( sym );
                                    }
                                },
                                _ => {

                                },
                            }
                        }

                        self.push_stack_frame(frame);
                        let ret = self.visit(body)?;
                        self.pop_stack_frame();
                        return Ok( ret );
                    },
                    SimKindValue::Tuple { value } => {
                        if let Some(_) = next_value {
                            return Err("Impossible Tuple value".to_string());
                        }
                        return Ok( Rc::clone(&value) );
                    },
                    SimKindValue::Value { value } => {
                        if let Some(n) = next_value {
                            // Array Parameter
                            let mut varr: VecDeque<Rc<Value>> = VecDeque::new();
                            varr.push_back(Rc::clone(value));
                            if let Value::Tuple(varr_next) = &*n {
                                varr.extend( varr_next.iter().map(Rc::clone) );
                            } else {
                                varr.push_back(Rc::clone(&n));
                            }

                            println!("Building Tuple: {:?} @ symval {:?}", varr, symval);
                            return Ok( Rc::new( Value::Tuple(varr) ) );
                        }
                        return Ok( Rc::clone(value) );
                    },
                    _ => {},
                }
                return Ok( Rc::new(Value::String(value.to_owned())) );
            },
            _ => {},
        }
        return Ok(Rc::new(Value::None));
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
    //i.input("y");
    //i.input("a = 1");
    //i.input("b = 10");
    //i.input("c = 100");
    //i.input("a + 1 + b");
    //i.input("a + b + c + 1");
    //i.input("fn avg a b c => a + b + c + 1");
    //i.input("avg a b c");
    i.input("avg 1 b 2.0");
    //println!("output={:?}", i.input("avg a b avg a b c"));
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