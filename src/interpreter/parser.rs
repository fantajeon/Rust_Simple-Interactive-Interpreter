use std::{collections::VecDeque, rc::Rc};

use crate::SymValue;

use super::basic::*;


#[derive(Debug)]
pub enum Node {
    None,
    FunctionDef{ 
        name: String, 
        params: Vec<Node>,
        body: Rc<Node>,
    },
    Assign {
        left: Box<Node>, 
        right: Box<Node>,
    },
    BinOp{ 
        left: Box<Node>, 
        op: String,
        right: Box<Node>,
    },
    Num{ value: Value },
    Identifier{ 
        value: String,
        next: Option<Box<Node>>,
     },
}

impl Node {
    pub fn identity_value(self) -> Option<String> {
        match self {
            Node::Identifier { value, next } => Some(value),
            _ => None,
        }
    }

    pub fn set_next_identity(&mut self, next_value: Node) {
        if let Node::Identifier { ref mut next, .. } = self {
            *next = Some(Box::new(next_value));
        }
    }
}


pub trait Evaluator {
    fn evaluate(&mut self, ast: &Node) -> Result<Rc<Value>,String>;
}

#[derive(Debug)]
pub struct Parser {
    curr_token: Token,
    input: VecDeque<Token>,
}

impl Default for Parser {
    fn default() -> Self { 
        Parser { curr_token: Token::default(), input: VecDeque::new() }
    }
}
impl Parser {
    pub fn new() -> Self {
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

    fn _function_call_parameter(&mut self) -> Result<Node, String> {
        let mut params: Vec<String> = Vec::new();
        while is_enum_variant!(&*self.curr_token.kind, Kind::Letter(_)) {
            let mut ct = self.curr_token.take();
            params.push( ct.kind.take_letter().unwrap() );
            self.shift_input();
        }
        let num_params = params.len();

        let result:Option<Node> = params.into_iter().enumerate().rev()
            .fold(None, |prev: Option<Node>, value| -> Option<Node> {
                    if let Some(nn) = prev {
                        return Some(Node::Identifier { 
                                value: value.1, 
                                next: Some(Box::new(nn)), 
                            });
                    } 
                    return Some(Node::Identifier { 
                            value: value.1, 
                            next: None,
                        });
                });
        return Ok(result.unwrap_or(Node::None));
    }

    fn _function_def_parameter(&mut self) -> Result<Vec<Node>, String> {
        let mut result: Vec<Node> = Vec::new();
        while is_enum_variant!(&*self.curr_token.kind, Kind::Letter(_)) {
            let mut ct = self.curr_token.take();
            result.push(Node::Identifier { 
                value: ct.kind.take_letter().unwrap(),
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
        if let Kind::Letter(fn_name) = &*self.curr_token.kind {
            println!("fn-name: {}", fn_name);
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
        } else {
            if self.curr_token.is_letter() {
                result.set_next_identity( self._function_call_parameter()? );
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
                return self._function_def();
            },
            _ => {
                return self._expression();
            },
        }
    }

    pub fn parse(&mut self, token: VecDeque<Token>, e: &mut dyn Evaluator) -> Result<Option<f32>,String> 
    {
        self.input = token;
        self.shift_input();
        let mut ast: Vec<Rc<Node>> = Vec::new();
        let mut last_value = None;
        loop {
            if self.curr_token.is_none() {
                println!("EOF!");
                break;
            }
            let n = self.stmt()?;
            let result = e.evaluate(&n)?;
            ast.push(Rc::new(n));
            last_value = Some(Rc::clone(&result));
        }

        if let Some(r) = last_value {
            return Ok(r.get_result());
        }
        return Err("None".to_string());
    }
}
