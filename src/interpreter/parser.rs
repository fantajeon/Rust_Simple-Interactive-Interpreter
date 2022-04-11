use std::collections::HashSet;
use std::{collections::VecDeque, rc::Rc};

use super::{symbol::*, basic::*, node::*};


pub trait Evaluator {
    fn evaluate(&mut self, ast: &Node) -> Result<Rc<Value>,String>;
    fn lookup(&self, sym_name: &str) -> Option<Rc<SymValue>>;
}

#[derive(Debug)]
pub struct Parser<'a, E> 
where E: Evaluator + Sized
{
    curr_token: Token,
    input: VecDeque<Token>,
    evaluator: Option<&'a E>,
}

impl<'a, E> Default for Parser<'a, E>
where E: Evaluator + Sized
{
    fn default() -> Self { 
        Parser { curr_token: Token::default(), input: VecDeque::new(), evaluator: Option::None }
    }
}

impl<'a, E> Parser<'a, E> 
where E: Evaluator + Sized
{
    pub fn new(e: &'a E, input: VecDeque<Token>) -> Self {
        Parser { 
            curr_token: Token::default(),
            input: input,
            evaluator: Some(e),
        }
    }

    fn shift_input(&mut self) -> Token {
        let new = self.input.pop_front().unwrap_or(Token::default());
        return std::mem::replace( &mut self.curr_token, new);
    }

    fn _function_call_parameter(&mut self) -> Result<Node, String> {
        let mut params: Vec<KindValue> = Vec::new();
        while is_enum_variant!(&*self.curr_token.kind, Kind::Letter(_)) 
            || is_enum_variant!(&*self.curr_token.kind, Kind::IntNumber(_)) 
            || is_enum_variant!(&*self.curr_token.kind, Kind::FloatNumber(_)) 
        {
            let mut ct = self.curr_token.take();
            //if ct.is_letter() {
            //    self.evaluator.unwrap().lookup(ct.value
            //}
            params.push( ct.kind.take_value().unwrap() );
            self.shift_input();
        }

        let result:Option<Node> = params.into_iter().enumerate().rev()
            .fold(None, |prev: Option<Node>, value| -> Option<Node> {
                    let nn = if let Some(nn) = prev {
                        Some(Box::new(nn))
                    }  else {
                        None
                    };
                    let node = match value.1 {
                        KindValue::FloatNumber(v) => Node::Num { value: v.into(), next: nn},
                        KindValue::IntNumber(v) => Node::Num { value: v.into(), next: nn },
                        KindValue::String(v) => Node::Identifier { value: v, next: nn },
                    };
                    return Some(node);
                });
        return Ok(result.unwrap_or(Node::None));
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
                next: None,
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
                let n = Node::Num { value: Value::FloatNumber(*v), next: None };
                self.shift_input();
                return Ok(n);
            },
            Kind::IntNumber(v) => {
                let n = Node::Num { value: Value::IntNumber(*v), next: None };
                self.shift_input();
                return Ok(n);
            },
            Kind::Letter(var) => {
                let n = Node::Identifier { value: var.clone(), next: None };
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
                if self.curr_token.is_letter() || self.curr_token.is_numbers() {
                    if !result.is_identifier() {
                        return Err("Syntax Error".to_string());
                    }
                    result.set_next_identity( self._function_call_parameter()? );
                } else {
                    break;
                }
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
