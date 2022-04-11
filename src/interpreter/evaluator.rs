
use std::{collections::{HashMap, VecDeque}, rc::Rc};
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
    //ast: Vec<Rc<Node>>,
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
            //ast: Vec::new(),
            line: 0,
        }
    }

    fn evaluate(&mut self, ast: &Node) -> Result<Rc<Value>,String> {
        let result = self.visit(ast, &Node::None);
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
        if input.len() == 0 {
            return Ok(None);
        }
        self.line += 1;
        println!("tokenizing={} @ line{}", input, self.line);
        let tokens = lexer(input);

        if tokens.len() == 0 {
            return Ok(None);
        }
        let mut parser = Parser::new(self, tokens);

        let ast = parser.parse()?;
        let result = self.evaluate(&ast.unwrap())?;
        println!("======> result({}) = {:?}", input, result);

        let last_value = Some(Rc::clone(&result));
        if let Some(r) = last_value {
            return Ok(r.get_result());
        }
        return Err("Unknown result".to_string());
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

    fn collect_all_symbols(&self, n: &Node, symbol_vec: &mut Vec<String>) -> Result<(), String> {
        match n {
            Node::Assign{left, right} => {
                self.collect_all_symbols(left, symbol_vec)?;
                self.collect_all_symbols(right, symbol_vec)?;
            },
            Node::BinOp{left, right, ..} => {
                self.collect_all_symbols(left, symbol_vec)?;
                self.collect_all_symbols(right, symbol_vec)?;
            },
            Node::Identifier { value, ..} => {
                symbol_vec.push( value.to_string() );
            },
            _ => {}
        }
        return Ok(());
    }

    fn visit(&mut self, n: &Node, prev_n: &Node) -> Result<Rc<Value>,String> {
        let curr_n = n;
        match n {
            Node::Assign{left, right} => {
                if let Some(var_name) = left.identity_value() {
                    let b = self.visit(right, curr_n)?;
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
                let a = self.visit(left, curr_n)?;
                let b = self.visit(right, curr_n)?;
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
                let params: Vec<String> = params.iter().filter_map( |t| {
                    if let Node::Identifier{value, ..} = t {
                        return Some(value.to_string());
                    }
                    None
                }).collect();
                let mut used_all_symbols: Vec<String>= Vec::new(); 
                self.collect_all_symbols(body, &mut used_all_symbols)?;
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
            Node::FunctionCall { value, body, params, params_name  } => {
                println!("function call:::::");
                //let mut frame = ScopeSymbolTable::new(None);
                //let mut rest_values: VecDeque<Rc<Value>> = VecDeque::new();
                
                //println!("!!!!! call function: {:?}, params={:?}", symval, next_value);
                //if let Some(v) = next_value {
                //    match &*v {
                //        Value::Tuple(ref v) => {
                //            if params.len() > v.len() {
                //                return Err(format!("function {} parameter is not matched: {}, but given {}", symval.get_name(), params.len(), v.len()));
                //            }
                //            let num_param = params.len();
                //            for p in params.iter().zip(v.iter().take(num_param)) {
                //                let sym = SymValue::new_value(p.0.as_str(), Rc::clone(p.1));
                //                frame.insert( sym )?;
                //            }
                //            for r in v.iter().skip(num_param) {
                //                rest_values.push_back( Rc::clone(r) );
                //            }
                //        },
                //        _ => {
                //            if params.len() > 1 {
                //                return Err(format!("function {} parameter is not matched: {}, but 1", symval.get_name(), params.len()));
                //            }
                //            for p in params.iter().zip(vec![v].iter()) {
                //                let sym = SymValue::new_value(p.0.as_str(), Rc::clone(p.1));
                //                frame.insert( sym )?;
                //            }
                //        },
                //    }
                //}

                //self.push_stack_frame(frame);
                //let mut ret = self.visit(body, curr_n)?;
                //// merge remaning tuple values
                //if rest_values.len() > 0 {
                //    rest_values.push_front( ret );
                //    ret = Rc::new( Value::Tuple( rest_values ) );
                //}
                //self.pop_stack_frame();
                //return Ok( ret );
            },
            Node::Num{value} => {
                return Ok( Rc::new(value.to_owned()) );
            },
            Node::Identifier{value} => {
                let v = self.current_scope.lookup(value);
                if v.is_none() {
                    return Err( format!("Cannot resolve symbol: {}", value) );
                }

                if let SimKindValue::Value{ ref value } = v.as_ref().unwrap().kind_value {
                    return Ok( Rc::clone(value) );
                }
                return Err( format!("Unexpected symbol: {:?}", v) );
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
