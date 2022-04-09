
use std::{collections::{HashMap, VecDeque}, rc::Rc};
use super::{basic::*, parser::*, lexer::*};
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
    Value{
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

    pub fn insert(&mut self, new_sym: SymValue) -> Result<Option<Rc<SymValue>>,String>{
        let old_val = self.symbols.get(&new_sym.name);
        if let Some(old_symval) = old_val {
            match (&(*old_symval).kind_value, &new_sym.kind_value) {
                (SimKindValue::Function {..}, SimKindValue::Function {..}) => {},
                (SimKindValue::Value {..}, SimKindValue::Value { ..}) => {},
                _ => {
                    return Err(format!("mis-matched symbol types:{:?}, {:?}", old_symval, new_sym));
                }
            }
            let old_ret = Rc::clone(old_symval);
            self.symbols.insert( new_sym.name.to_string(), Rc::new(new_sym) );
            return Ok(Some(old_ret));
        }
        self.symbols.insert( new_sym.name.to_string(), Rc::new(new_sym) );
        return Ok(None);
    }

    pub fn lookup(&self, sym: &String) -> Option<Rc<SymValue>> {
        if let Some(v) = self.symbols.get(sym) {
            return Some(Rc::clone(v));
        }

        //if let Some(parent) = &self.parent {
        //    return parent.lookup(sym);
        //}

        return None;
    }
}
#[derive(Debug)]
pub struct Interpreter {
    func: HashMap<String,Rc<Node>>,
    current_scope: Box<ScopeSymbolTable>,
    //ast: Vec<Rc<Node>>,
    line: u32,
}

impl Evaluator for Interpreter {
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
        if input.len() == 0 {
            return Ok(None);
        }
        self.line += 1;
        println!("tokenizing={} @ line{}", input, self.line);
        let tokens = lexer(input);

        if tokens.len() == 0 {
            return Ok(None);
        }
        let mut parser = Parser::new();

        let result = parser.parse( tokens, self )?;
        println!("======> result({}) = {:?}", input, result);

        return Ok(result);
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
            Node::Num { next, ..} => {
                if let Some(n) = next {
                    self.collect_all_symbols(n, symbol_vec)?;
                }
            },
            Node::Identifier { value, next, ..} => {
                symbol_vec.push( value.to_string() );
                if let Some(n) = next {
                    self.collect_all_symbols(n, symbol_vec)?;
                }
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
                        params, 
                        Rc::clone(body) ) )?;
                return Ok(Rc::new(Value::None));
            },
            Node::Num{value, next} => {
                println!("Scane Num - value: {:?}, next: {:?}", value, next);
                let next_value = if let Some(n) = next {
                        if is_enum_variant!(prev_n, Node::None) {
                            return Err(format!("Syntax Error!"));
                        }
                        Some(self.visit(n, curr_n)?)
                    } else {
                        None
                    };
                if next_value.is_none() {
                    return Ok( Rc::new(value.to_owned()) );
                }
                if let Some(n) = next_value {
                    let mut varr: VecDeque<Rc<Value>> = VecDeque::new();
                    varr.push_back(Rc::new(value.to_owned()));
                    if let Value::Tuple(varr_next) = &*n {
                        varr.extend( varr_next.iter().map(Rc::clone) );
                    } else {
                        varr.push_back(Rc::clone(&n));
                    }

                    println!("Building Tuple: {:?}", varr);
                    return Ok( Rc::new( Value::Tuple(varr) ) );
                }
                return Ok( Rc::new(value.to_owned()) );
            },
            Node::Identifier{value, next} => {
                let v = self.current_scope.lookup(value);
                if v.is_none() {
                    return Err( format!("Cannot resolve symbol: {}", value) );
                }
                let next_value = if let Some(n) = next {
                        Some(self.visit(n, curr_n)?)
                    } else {
                        None
                    };
                let symval = v.unwrap();
                match &symval.kind_value {
                    SimKindValue::Function { body, params } => {
                        let mut frame = ScopeSymbolTable::new(None);
                        let mut rest_values: VecDeque<Rc<Value>> = VecDeque::new();
                        
                        println!("!!!!! call function: {:?}, params={:?}", symval, next_value);
                        if let Some(v) = next_value {
                            match &*v {
                                Value::Tuple(ref v) => {
                                    if params.len() > v.len() {
                                        return Err(format!("function {} parameter is not matched: {}, but given {}", symval.get_name(), params.len(), v.len()));
                                    }
                                    let num_param = params.len();
                                    for p in params.iter().zip(v.iter().take(num_param)) {
                                        let sym = SymValue::new_value(p.0.as_str(), Rc::clone(p.1));
                                        frame.insert( sym )?;
                                    }
                                    for r in v.iter().skip(num_param) {
                                        rest_values.push_back( Rc::clone(r) );
                                    }
                                },
                                _ => {
                                    if params.len() > 1 {
                                        return Err(format!("function {} parameter is not matched: {}, but 1", symval.get_name(), params.len()));
                                    }
                                    for p in params.iter().zip(vec![v].iter()) {
                                        let sym = SymValue::new_value(p.0.as_str(), Rc::clone(p.1));
                                        frame.insert( sym )?;
                                    }
                                },
                            }
                        }

                        self.push_stack_frame(frame);
                        let mut ret = self.visit(body, curr_n)?;
                        // merge remaning tuple values
                        if rest_values.len() > 0 {
                            rest_values.push_front( ret );
                            ret = Rc::new( Value::Tuple( rest_values ) );
                        }
                        self.pop_stack_frame();
                        return Ok( ret );
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

                            return Ok( Rc::new( Value::Tuple(varr) ) );
                        }
                        return Ok( Rc::clone(value) );
                    },
                }
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
