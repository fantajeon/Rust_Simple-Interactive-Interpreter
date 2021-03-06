
use std::{collections::{HashMap}, rc::Rc};
use super::{basic::*, parser::*, lexer::*, symbol::*, node::*};
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
pub struct Interpreter {
    func: HashMap<String,Rc<Node>>,
    current_scope: Box<ScopeSymbolTable>,
    line: u32,
}

impl SymbolLookup for Interpreter {
    fn lookup(&self, sym_name: &str ) -> Option<Rc<SymValue>> {
        self.current_scope.lookup(sym_name)
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter { 
            current_scope: Box::new(ScopeSymbolTable::new(None)),
            func: HashMap::new(), 
            line: 0,
        }
    }

    fn evaluate(&mut self, ast: &Node) -> Result<Rc<Value>,String> {
        let result = self.visit(ast);
        println!("AST=>>>>{:?}", ast);
        println!("result(AST)={:?}", result);
        println!("(fn)={:?}", self.func);

        if result.is_err() {
            return result;
        }

        let result = result.unwrap();
        match &*result {
            Value::None | Value::IntNumber(_) | Value::FloatNumber(_) => return Ok(result),
            _ => return Err("Invalidated Result".to_string()),
        }
    }


    pub fn input(&mut self, input: &str) -> Result<Option<f32>, String> {
        self.line += 1;
        println!("########## > tokenizing={} @ line{}", input, self.line);
        let tokens = lexer(input)?;

        if tokens.len() < 2  {
            // Only have Token::None
            return Ok(None);
        }

        let mut parser = Parser::new(self, tokens);

        let ast = parser.parse()?;
        return Ok(self.evaluate(&ast.unwrap())?.get_result());
    }

    fn push_stack_frame(&mut self,  frame: ScopeSymbolTable) {
        let parent = std::mem::replace( &mut self.current_scope, Box::new(frame) );
        self.current_scope.parent = Some(parent);
    }

    fn pop_stack_frame(&mut self) {
        if let Some(parent) = self.current_scope.take_parent() {
            self.current_scope=parent;
        }
    }

    fn collect_all_symbols(&self, n: &Node, symbol_vec: &mut Vec<String>) {
        match n {
            Node::Assign{left, right} => {
                self.collect_all_symbols(left, symbol_vec);
                self.collect_all_symbols(right, symbol_vec);
            },
            Node::BinOp{left, right, ..} => {
                self.collect_all_symbols(left, symbol_vec);
                self.collect_all_symbols(right, symbol_vec);
            },
            Node::Identifier { value, ..} => {
                symbol_vec.push( value.to_string() );
            },
            Node::FunctionCall { name, ..} => {
                symbol_vec.push( name.to_string() );
            },
            _ => {},
        }
    }

    fn visit(&mut self, n: &Node) -> Result<Rc<Value>,String> {
        match n {
            Node::Assign{left, right} => {
                if let Some(var_name) = left.identity_value() {
                    let b = self.visit(right)?;
                    self.current_scope.insert(
                            SymValue::new_value(
                                &var_name, 
                                Rc::clone(&b)
                            )
                        )?;
                    println!("Result(Assign) = {:?}", b);
                    return Ok(b);
                }
                return Err(format!("Left side of Assign Problem: {:?} = {:?}", left, right));
            },
            Node::BinOp{left, op, right} => { 
                let a = self.visit(left)?;
                let b = self.visit(right)?;
                let r: Rc<Value> = (match op.as_str() {
                    "+" => Some(Rc::new(a.plus(&b).unwrap())),
                    "-" => Some(Rc::new(a.minus(&b).unwrap())),
                    "/" => Some(Rc::new(a.divide(&b).unwrap())),
                    "*" => Some(Rc::new(a.multiply(&b).unwrap())),
                    "%" => Some(Rc::new(a.modulus(&b).unwrap())),
                    _ => return Err(format!("Unknown Binary Option: {}", op.as_str())),
                }).unwrap();
                return Ok(r);
            },
            Node::FunctionDef{name, params, body} => {
                println!("create function: {} with {:?}", name, params);
                let params: Vec<String> = params.iter().filter_map( |t| {
                    if let Node::Identifier{value, ..} = t {
                        return Some(value.to_string());
                    }
                    None
                }).collect();
                let mut used_all_symbols: Vec<String>= Vec::new(); 
                self.collect_all_symbols(body, &mut used_all_symbols);
                for s in &used_all_symbols {
                    if params.iter().filter( |&a| a == s).count() == 0 {
                        return Err(format!("Unknown symbol: {} in function {}" , s, name));
                    }
                }
                self.current_scope.insert( SymValue::new_function(
                        name,
                        Rc::new(params), 
                        Rc::clone(body) ) )?;
                return Ok(Rc::new(Value::None));
            },
            Node::FunctionCall { name, body, params, params_name  } => {
                let mut frame = ScopeSymbolTable::new(None);
                
                let num_param = params.len();
                for p in params_name.iter().zip(params.iter().take(num_param)) {
                    let param_name = p.0.as_str();
                    let param_value = self.visit(p.1)?;
                    let sym = SymValue::new_value(param_name, param_value);

                    println!("Pushed symbol to Stack: {:?} under {}", sym, name);
                    frame.insert( sym )?;
                }
                self.push_stack_frame(frame);
                let ret = self.visit(body)?;
                self.pop_stack_frame();
                return Ok( ret );
            },
            Node::Num{value} => {
                return Ok( Rc::new(value.to_owned()) );
            },
            Node::Identifier{value} => {
                let v = self.current_scope.lookup(value).ok_or(format!("Unknown symbol: {}", value))?;

                match v.kind_value {
                    SimKindValue::Value{ ref value } => return Ok( Rc::clone(value) ),
                    _ => Err( format!("Unexpected symbol: {:?}", v) ),
                }
            }
        }
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}
