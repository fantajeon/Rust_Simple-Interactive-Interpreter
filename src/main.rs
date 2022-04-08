mod interpreter;
use std::{collections::{HashMap}, rc::Rc};

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
enum SymKind {
    Value{value: Rc<Value>},
    Function{
        node: Rc<Node>,
        params: Vec<String>,
    },
}
#[derive(Debug)]
 struct SymValue {
    name: String,
    kind: SymKind,
}

impl SymValue {
    fn get_name(&self) -> &String {
        return &self.name;
    }
    
    fn get_value(&self) -> Option<Rc<Value>> {
        if let SymKind::Value{value} = &self.kind {
            return Some( Rc::clone(value) );
        }
        return None;
    }

    fn new_value(name: &str, value: Rc<Value>) -> Self {
        SymValue{ name: name.to_string(), kind: SymKind::Value{value: Rc::clone(&value)} }
    }

    fn new_function(name: &str, params: Vec<String>, node: Rc<Node>) -> Self {
        SymValue{ 
            name: name.to_string(), 
            kind: SymKind::Function{
                params: params,
                node: Rc::clone(&node)
            } 
        }
    }
}

#[derive(Debug)]
struct ScopeSymbolTable {
    parent: Option<Rc<ScopeSymbolTable>>,
    symbols: HashMap<String,Rc<SymValue>>,
}

impl ScopeSymbolTable {
    pub fn new(parent: Option<Rc<ScopeSymbolTable>>) -> Self {
        ScopeSymbolTable { parent: parent, symbols: HashMap::new() }
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
    func: HashMap<String,Rc<Node>>,
    current_scope: ScopeSymbolTable,
    //ast: Vec<Rc<Node>>,
    line: u32,
}

impl Evaluator for Interpreter {
    fn evaluate(&mut self, ast: &Node) -> Result<(),String> {
        println!("AST=>>>>{:?}", ast);
        println!("result(AST)={:?}", self.visit(ast));
        println!("(fn)={:?}", self.func);
        return Ok(());
    }
}

impl Interpreter {
    fn new() -> Self {
        Interpreter { 
            current_scope: ScopeSymbolTable::new(None), 
            func: HashMap::new(), 
            //ast: Vec::new(),
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



    fn visit(&mut self, n: &Node) -> Result<Rc<Value>,String> {
        match n {
            Node::Assign{left, right} => {
                let a = self.visit(left)?;
                let b = self.visit(right)?;
                let var_value: Option<Rc<Value>> = match a.as_ref() {
                    Value::String(var_name) =>  {
                        println!("DEBUG(var table) = {:?}, {:?}", var_name, b);
                        self.current_scope.insert(SymValue::new_value(&var_name, Rc::clone(&b)));
                        Some(b)
                    },
                    _ => {
                        None
                    }
                };
                return Ok(var_value.unwrap());
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
                //self.func.insert( name.to_owned(), Rc::clone( body ) );
                let params: Vec<String> = params.iter().filter_map( |t| {
                    if let Node::Identifier{value} = t {
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
            Node::FunctionCall{name, params} => {

            },
            Node::Num{value} => {return Ok( Rc::new(value.to_owned()) );},
            Node::Identifier{value} => {
                let v = self.current_scope.lookup(value);
                if v.is_some() {
                    return Ok( Rc::clone(&v.unwrap().get_value().unwrap()) );
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
    i.input("a = 1");
    //i.input("b = 10");
    //i.input("a + 1 + b");
    //i.input("a + b + c + 1");
    //i.input("fn avg a b c => a + b + c + 1");
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