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

    fn shift_input(&mut self) -> Result<Token, String> {
        if self.input.is_empty() {
            return Err("Empty Input".to_string());
        }

        assert!( !is_enum_variant!( &*self.curr_token.kind, Kind::Eof) );

        return Ok( std::mem::replace( 
                    &mut self.curr_token, 
                    self.input.pop_front().unwrap()
                )
            );
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
            let ct = self.shift_input()?;
            result_params.push( match *ct.kind {
                Kind::Letter(_) => {
                    let sym_name = &ct.raw_string;
                    self.symbol_table.unwrap().lookup(&sym_name).map_or(Ok(Node::Identifier { value: sym_name.to_string() }), 
                        |sym_val| -> Result<Node, String> {
                            match sym_val.kind_value {
                                SimKindValue::Function { ref body, ref params } => {
                                    let result = self._function_call_parameter(sym_name.as_str(), (*params).len())?;

                                    Ok(Node::FunctionCall { 
                                        name: sym_name.to_string(),
                                        params_name: Rc::clone(params),
                                        params: result,
                                        body: Rc::clone(body),
                                    })
                                },
                                _ => Ok(Node::Identifier { value: sym_name.to_string() }),
                            }
                        })?
                },
                Kind::FloatNumber(v) => Node::Num { value: v.into() },
                Kind::IntNumber(v) => Node::Num { value: v.into() },
                _ => unreachable!("please check your code"),
            })
        }

        if result_params.len() != num_params {
            return Err(format!("Parsing error, not enough to be parameters of function {}", func_name));
        }

        return Ok(result_params);
    }

    fn _function_def_parameter(&mut self) -> Result<Vec<Node>, String> {
        let mut result: Vec<Node> = Vec::new();
        let mut params_set: HashSet<String> = HashSet::new();
        while is_enum_variant!(&*self.curr_token.kind, Kind::Letter(_)) {
            let mut ct = self.shift_input()?;
            let param_name = ct.kind.take_letter().unwrap();

            if params_set.get(&param_name).is_some() {
                return Err(format!("parameter name is duplicated! {}", param_name));
            }
            params_set.insert( param_name.to_string() );
            result.push(Node::Identifier { value: param_name, });
        }
        return Ok(result);
    }

    fn _function_expression(&mut self) -> Result<Node, String> {
        if is_enum_variant!(*self.curr_token.kind, Kind::FNOP) {
            self.shift_input()?;
            return self._expression();
        }
        return Err("Syntax Error! function Expression".to_string());
    }

    fn _function_def(&mut self) -> Result<Node,String> {
        if let Kind::Letter(_) = &*self.curr_token.kind {
            let mut ct = self.curr_token.take();
            self.shift_input()?;
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
                self.shift_input()?;
                return Ok(n);
            },
            Kind::IntNumber(v) => {
                let n = Node::Num { value: Value::IntNumber(*v) };
                self.shift_input()?;
                return Ok(n);
            },
            Kind::Letter(var) => {
                let var_name = var.clone();
                self.shift_input()?;
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
                self.shift_input()?;
                let n = self._expression()?;
                if !is_enum_variant!(*self.curr_token.kind, Kind::RPAREN) {
                    return Err("Expected String: )".to_string());
                }
                self.shift_input()?; // FEED RPAREN
                return Ok(n);
            },
            _ => {},
        }
        return Err("Unknown Rules!".to_string());
    }

    fn _term(&mut self) -> Result<Node,String> {
        let mut result = self._factor()?;

        while self.curr_token.kind.is_op(|v| v == "*" || v == "/" || v == "%") {
            let tok = self.shift_input()?;
            let right = self._factor()?;
            result = Node::BinOp { 
                left: Box::new(result),
                op: tok.kind.op().unwrap().clone(),
                right: Box::new(right),
            };
        }
        return Ok(result);
    }

    fn _expression(&mut self) -> Result<Node,String> {
        let mut result = self._term()?;

        while is_enum_variant!(&*self.curr_token.kind, Kind::ASSIGN) 
            || is_enum_variant!(&*self.curr_token.kind, Kind::Op(_)) {
            let tok = self.shift_input()?;
            match &*tok.kind {
                Kind::ASSIGN => {
                    result = Node::Assign { 
                        left: Box::new(result), 
                        right: Box::new(self._expression()?), 
                    };
                },
                Kind::Op(v) => {
                    assert!( v == "+" || v == "-");
                    result = Node::BinOp { 
                        left: Box::new(result), 
                        op: v.clone(),
                        right: Box::new(self._term()?), 
                    };
                },
                _ => {},
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
                self.shift_input()?;
                return self._function_def();
            },
            _ => {
                return self._expression();
            },
        }
    }

    pub fn parse(&mut self) -> Result<Option<Node>,String> 
    {
        self.shift_input()?;
        let ast = self.stmt()?;

        if !self.curr_token.is_eof() {
            return Err(format!("Unexpectable Input Value, {:?}", self.input));
        }
        return Ok(Some(ast));
    }
}
