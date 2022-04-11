use std::collections::HashSet;
use std::{collections::VecDeque, rc::Rc};

use super::{symbol::*, basic::*, node::*};


pub trait SymbolLookup {
    fn lookup(&self, sym_name: &str) -> Option<Rc<SymValue>>;
}

#[derive(Debug)]
pub struct Parser<'a, S>
where S: SymbolLookup + Sized
{
    curr_token: Token,
    input: VecDeque<Token>,
    symbol_table: Option<&'a S>,
}

impl<'a, S> Default for Parser<'a, S>
where S: SymbolLookup + Sized
{
    fn default() -> Self { 
        Parser { curr_token: Token::default(), input: VecDeque::new(), symbol_table: Option::None }
    }
}

impl<'a, S> Parser<'a, S> 
where S: SymbolLookup + Sized
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

    fn _function_call_parameter(&mut self) -> Result<Vec<Node>, String> {
        let mut params: Vec<Node> = Vec::new();
        while is_enum_variant!(&*self.curr_token.kind, Kind::Letter(_)) 
            || is_enum_variant!(&*self.curr_token.kind, Kind::IntNumber(_)) 
            || is_enum_variant!(&*self.curr_token.kind, Kind::FloatNumber(_)) 
        {
            let mut ct = self.curr_token.take();
            if ct.is_letter() {
                println!("DEBUG===> {:?}", ct);
                let sym_name = ct.raw_string;
                if let Some(sym_value) = self.symbol_table.unwrap().lookup(&sym_name) {
                   if let SimKindValue::Function { ref body, ..} = sym_value.kind_value {
                        let result = self._function_call_parameter()?;
                        let node = Node::FunctionCall { 
                            value: sym_name,
                            params: result,
                            body: Rc::clone(body),
                        };

                        params.push( node );
                   }
                } else {
                    let node = Node::Identifier { 
                        value: sym_name,
                    };
                    params.push( node );
                }
            } else {
                let node = match *ct.kind {
                    Kind::FloatNumber(v) => Node::Num { value: v.into()},
                    Kind::IntNumber(v) => Node::Num { value: v.into()},
                    _ => unreachable!("please check your code"),
                };
                params.push( node );
            }
            self.shift_input();
        }

        return Ok(params);
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
                let n: Node = if let Some(sym_val) = self.symbol_table.unwrap().lookup(&var_name) {
                    if let SimKindValue::Function { ref body, .. } = sym_val.kind_value {
                        let n = Node::FunctionCall { 
                                value: var_name.to_string(),
                                body: Rc::clone(body),
                                params: self._function_call_parameter()?
                            };
                        
                        println!("Function CAll={:?}", n);
                        n
                    } else {
                        Node::Identifier { value: var_name.to_string() }
                    }
                } else {
                    Node::Identifier { value: var_name.to_string()}
                };
                let mut ct = self.curr_token.take();
                if let Kind::Op(v) = &*ct.kind {
                    if v == "=" {
                        self.shift_input();
                        let expr_node = self._expression()?;
                        let op_node = Node::BinOp { 
                            left: Box::new(n), 
                            op: ct.kind.take_op().unwrap(),
                            right: Box::new(expr_node) 
                        };
                        return Ok(op_node);
                    } 
                } 
                self.curr_token.replace(ct);
                return Ok(n);
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
            if let Some(_) = self.curr_token.take_if(|t| 
                is_enum_variant!(*t.kind, Kind::ASSIGN)
            ) {
                self.shift_input();
                result = Node::Assign { 
                    left: Box::new(result), 
                    right: Box::new(self._expression()?), 
                };
            } else if let Some(mut tok) = self.curr_token.take_if(|t| 
                is_enum_variant!(*t.kind, Kind::Op(_))
            ) {
                let v = (*tok.kind).op().unwrap();
                if v == "+" || v == "-" {
                    self.shift_input();
                    result = Node::BinOp { 
                        left: Box::new(result), 
                        op: tok.kind.take_op().unwrap(),
                        right: Box::new(self._term()?), 
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
        if self.curr_token.is_none() {
            return Ok(None);
        }
        let ast = self.stmt()?;
        return Ok(Some(ast));
    }
}
