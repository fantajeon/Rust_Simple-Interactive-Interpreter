use std::fmt::Debug;
use std::collections::HashSet;
use std::{collections::VecDeque, rc::Rc};

use super::{symbol::*, basic::*, node::*};


pub trait SymbolLookup {
    fn lookup(&self, sym_name: &str) -> Option<Rc<SymValue>>;
}

#[derive(Debug)]
pub struct Parser<'a, S>
where S: SymbolLookup + Sized + Debug
{
    curr_token: Token,
    input: VecDeque<Token>,
    symbol_table: Option<&'a S>,
}

impl<'a, S> Default for Parser<'a, S>
where S: SymbolLookup + Sized + Debug
{
    fn default() -> Self { 
        Parser { curr_token: Token::default(), input: VecDeque::new(), symbol_table: Option::None }
    }
}

impl<'a, S> Parser<'a, S> 
where S: SymbolLookup + Sized + Debug
{
    pub fn new(e: &'a S, input: VecDeque<Token>) -> Self {
        Parser { 
            curr_token: Token::default(),
            input: input,
            symbol_table: Some(e),
        }
    }

    fn shift_input(&mut self) -> Token {
        let new = self.input.pop_front().unwrap_or(Token::default());
        return std::mem::replace( &mut self.curr_token, new);
    }

    fn _function_call_parameter(&mut self, func_name: &str, num_params: usize) -> Result<Vec<Node>, String> {
        println!("parsing function call_parameters: {} with num param({})", func_name, num_params);
        let mut result_params: Vec<Node> = Vec::new();
        for _ in 0..num_params {
            if !(is_enum_variant!(&*self.curr_token.kind, Kind::Letter(_)) 
                || is_enum_variant!(&*self.curr_token.kind, Kind::IntNumber(_)) 
                || is_enum_variant!(&*self.curr_token.kind, Kind::FloatNumber(_)) ) {
                return Err( format!("Error Unexpected Token: {:?} during function call", self.curr_token) );
            }
            let ct = self.curr_token.take();
            if ct.is_letter() {
                let sym_name = ct.raw_string;
                if let Some(sym_value) = self.symbol_table.unwrap().lookup(&sym_name) {
                    if let SimKindValue::Function { ref body, ref params }  = sym_value.kind_value {
                        self.shift_input();
                        let result = self._function_call_parameter(sym_name.as_str(), (*params).len())?;

                        let node = Node::FunctionCall { 
                            name: sym_name,
                            params_name: Rc::clone(params),
                            params: result,
                            body: Rc::clone(body),
                        };

                        result_params.push( node );
                    } else {
                        let node = Node::Identifier { 
                            value: sym_name,
                        };
                        result_params.push( node );
                        self.shift_input();
                    }
                } else {
                    let node = Node::Identifier { 
                        value: sym_name,
                    };
                    result_params.push( node );
                    self.shift_input();
                }
            } else {
                let node = match *ct.kind {
                    Kind::FloatNumber(v) => Node::Num { value: v.into()},
                    Kind::IntNumber(v) => Node::Num { value: v.into()},
                    _ => unreachable!("please check your code"),
                };
                result_params.push( node );
                self.shift_input();
            }
        }

        if result_params.len() != num_params {
            return Err("Parsing error, not enough to be parameters".to_string());
        }

        return Ok(result_params);
    }

    fn _function_def_parameter(&mut self) -> Result<Vec<Node>, String> {
        let mut result: Vec<Node> = Vec::new();
        let mut params_set: HashSet<String> = HashSet::new();
        while is_enum_variant!(&*self.curr_token.kind, Kind::Letter(_)) {
            let mut ct = self.curr_token.take();
            let param_name = ct.kind.take_letter().unwrap();

            if params_set.get(&param_name).is_some() {
                return Err(format!("parameter name is duplicated! {}", param_name));
            }
            params_set.insert( param_name.to_string() );
            result.push(Node::Identifier { 
                value: param_name,
            });
            self.shift_input();
        }
        return Ok(result);
    }

    fn _function_expression(&mut self) -> Result<Node, String> {
        if is_enum_variant!(*self.curr_token.kind, Kind::FNOP) {
            self.shift_input();
            return self._expression();
        }
        return Err("Syntax Error! function Expression".to_string());
    }

    fn _function_def(&mut self) -> Result<Node,String> {
        if let Kind::Letter(_) = &*self.curr_token.kind {
            let mut ct = self.curr_token.take();
            self.shift_input();
            let result = Node::FunctionDef { 
                name: (*ct.kind).take_letter().unwrap(), 
                params: self._function_def_parameter()?, 
                body: Rc::new(self._function_expression()?), 
            };
            return Ok(result);
        }
        return Err("Syntax Error! function must have fn-name".to_string());
    }

    fn _factor(&mut self) -> Result<Node,String> {
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
                let var_name = var.clone();
                self.shift_input();
                let left: Node = if let Some(sym_val) = self.symbol_table.unwrap().lookup(&var_name) {
                                    match sym_val.kind_value {
                                        SimKindValue::Function { ref body, ref params, .. } =>
                                            Node::FunctionCall { 
                                                    name: var_name.to_string(),
                                                    body: Rc::clone(body),
                                                    params_name: Rc::clone(params),
                                                    params: self._function_call_parameter( var_name.as_str(), params.len() )?
                                                },
                                        _ => Node::Identifier { value: var_name.to_string() },
                                    }
                                } else {
                                    Node::Identifier { value: var_name.to_string() }
                                };
                return Ok(left);
            },
            Kind::LPAREN => {
                self.shift_input();
                let n = self._expression()?;
                if !is_enum_variant!(*self.curr_token.kind, Kind::RPAREN) {
                    return Err("Expected String: )".to_string());
                }
                self.shift_input();
                return Ok(n);
            },
            _ => {},
        }
        return Err("Unknown Rules!".to_string());
    }

    fn _term(&mut self) -> Result<Node,String> {
        let mut result = self._factor()?;

        loop {
            if let Some(mut tok) = self.curr_token.take_if(|v| v.kind.is_op()) {
                let v = tok.kind.op().unwrap();
                if v == "*" || v == "/" || v == "%" {
                    self.shift_input();
                    let right = self._factor()?;
                    result = Node::BinOp { 
                        left: Box::new(result),
                        op: tok.kind.take_op().unwrap(),
                        right: Box::new(right),
                    };
                } else {
                    self.curr_token.replace(tok);
                    break;
                }
            } else {
                break;
            }
        }
        return Ok(result);
    }

    fn _expression(&mut self) -> Result<Node,String> {
        let mut result = self._term()?;

        loop {
            let tok = self.curr_token.take();
            match &*tok.kind {
                Kind::ASSIGN => {
                    self.shift_input();
                    result = Node::Assign { 
                        left: Box::new(result), 
                        right: Box::new(self._expression()?), 
                    };
                },
                Kind::Op(v) => {
                    assert!( v == "+" || v == "-");
                    self.shift_input();
                    result = Node::BinOp { 
                        left: Box::new(result), 
                        op: v.clone(),
                        right: Box::new(self._term()?), 
                    };
                },
                _ => {
                    self.curr_token.replace(tok);
                    break
                },
            };
        }
        return Ok(result);
    }

    fn stmt(&mut self) -> Result<Node,String> {
        match &*self.curr_token.kind {
            Kind::Keyword(v) => {
                if v != "fn" {
                    return Err("syntax error!".to_string());
                } 
                self.shift_input();
                return self._function_def();
            },
            _ => {
                return self._expression();
            },
        }
    }

    pub fn parse(&mut self) -> Result<Option<Node>,String> 
    {
        self.shift_input();
        let ast = self.stmt()?;

        if !self.curr_token.is_none() {
            return Err(format!("Unexpectable Input Value, {:?}", self.input));
        }
        return Ok(Some(ast));
    }
}
