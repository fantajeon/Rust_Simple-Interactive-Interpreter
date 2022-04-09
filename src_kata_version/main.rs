
use std::{collections::{HashMap, VecDeque, HashSet}, rc::Rc};
use regex::Regex;

macro_rules! is_enum_variant {
    ($v:expr, $p:pat) => (
        if let $p = $v { true } else { false }
    );
}

pub enum KindValue {
    String(String),
    IntNumber(i32),
    FloatNumber(f32),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Kind {
    None,
    FloatNumber(f32),
    IntNumber(i32),
    Letter(String),
    Op(String),
    Keyword(String),
    LPAREN,
    RPAREN,
    ASSIGN,
    FNOP,
}

impl Kind {
    pub fn op<'a>(&self) -> Option<&String> {
        match self {
            Kind::Op(v) => Some(v),
            _ => None,
        }
    }
    
    pub fn take_value(&mut self) -> Option<KindValue> {
        match self {
            Kind::Letter(v) => {
                Some(KindValue::String(std::mem::replace(v, "".to_string())))
            },
            Kind::IntNumber(v) => Some(KindValue::IntNumber(*v)),
            Kind::FloatNumber(v) => Some(KindValue::FloatNumber(*v)),
            _ => None,
        }

    }

    pub fn take_letter(&mut self) -> Option<String> {
        match self {
            Kind::Letter(v) => {
                Some(std::mem::replace(v, "".to_string()))
            },
            _ => None,
        }
    }

    pub fn take_op(&mut self) -> Option<String> {
        match self {
            Kind::Op(v) => {
                Some(std::mem::replace(v, "".to_string()))
            },
            _ => None,
        }
    }

    pub fn is_op(&self) -> bool {
        is_enum_variant!(self, Kind::Op(_))
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    None,
    Tuple(VecDeque<Rc<Value>>),
    IntNumber(i32),
    FloatNumber(f32),
    String(String),
}

impl From<&str> for Value {
    fn from(v: &str) -> Self {
        Self::String(v.to_string())
    }
}

impl From<f32> for Value {
    fn from(v: f32) -> Self {
        Self::FloatNumber(v)
    }
}

impl From<i32> for Value {
    fn from(v: i32) -> Self {
        Self::IntNumber(v)
    }
}

impl Value {
    pub fn get_tuple(&mut self) -> Option<&mut VecDeque<Rc<Value>>> {
        if let Value::Tuple(v) = self {
            return Some(v);
        }
        return None;
    }

    pub fn get_result(&self) -> Option<f32> {
        match self {
            Value::IntNumber(v) => return Some(*v as f32),
            Value::FloatNumber(v) => return Some(*v),
            _ => return None,
        }
    }

    pub fn plus(&self, b: &Value) -> Option<Value> {
        match (self, b) {
            (Value::IntNumber(x), Value::IntNumber(y)) => Some(Value::IntNumber( x+y )),
            (Value::FloatNumber(x), Value::IntNumber(y)) => Some(Value::FloatNumber( x+(*y as f32) )),
            (Value::FloatNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( x+y )),
            (Value::IntNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( (*x as f32)+y )),
            _ => None,
        }
    }

    pub fn modulus(&self, b: &Value) -> Option<Value> {
        match (self, b) {
            (Value::IntNumber(x), Value::IntNumber(y)) => Some(Value::IntNumber( x%y )),
            (Value::FloatNumber(x), Value::IntNumber(y)) => Some(Value::FloatNumber( x%(*y as f32) )),
            (Value::FloatNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( x%y )),
            (Value::IntNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( (*x as f32)%y )),
            _ => None,
        }
    }

    pub fn multiply(&self, b: &Value) -> Option<Value> {
        match (self, b) {
            (Value::IntNumber(x), Value::IntNumber(y)) => Some(Value::IntNumber( x*y )),
            (Value::FloatNumber(x), Value::IntNumber(y)) => Some(Value::FloatNumber( x*(*y as f32) )),
            (Value::FloatNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( x*y )),
            (Value::IntNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( (*x as f32)*y )),
            _ => None,
        }
    }

    pub fn divide(&self, b: &Value) -> Option<Value> {
        match (self, b) {
            (Value::IntNumber(x), Value::IntNumber(y)) => Some(Value::FloatNumber( (*x as f32)/(*y as f32) )),
            (Value::FloatNumber(x), Value::IntNumber(y)) => Some(Value::FloatNumber( x/(*y as f32) )),
            (Value::FloatNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( x/y )),
            (Value::IntNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( (*x as f32)/y )),
            _ => None,
        }
    }

    pub fn minus(&self, b: &Value) -> Option<Value> {
        match (self, b) {
            (Value::IntNumber(x), Value::IntNumber(y)) => Some(Value::IntNumber( x-y )),
            (Value::FloatNumber(x), Value::IntNumber(y)) => Some(Value::FloatNumber( x-(*y as f32) )),
            (Value::FloatNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( x-y )),
            (Value::IntNumber(x), Value::FloatNumber(y)) => Some(Value::FloatNumber( (*x as f32)-y )),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct Token {
    pub kind: Box<Kind>,
    pub value: Value,
}

impl Token {
    pub fn new(v: Kind) -> Self {
        Token{kind: Box::new(v), value: Value::None}
    }

    pub fn is_none(&self) -> bool {
        is_enum_variant!(*self.kind, Kind::None)
    }

    pub fn is_op(&self) -> bool {
        is_enum_variant!(*self.kind, Kind::Op(_))
    }

    pub fn is_letter(&self) -> bool {
        is_enum_variant!(*self.kind, Kind::Letter(_))
    }
    
    pub fn is_numbers(&self) -> bool {
        is_enum_variant!(*self.kind, Kind::IntNumber(_)) || is_enum_variant!(*self.kind, Kind::FloatNumber(_))
    }

    pub fn take(&mut self) -> Token {
        std::mem::replace(self,Token::default())
    }

    pub fn replace(&mut self, tok: Token) -> Token {
        std::mem::replace(self, tok)
    }

    pub fn take_if<F: FnOnce(&Self) -> bool>(&mut self, predicate: F) -> Option<Self> {
        if predicate(self) {
            Some(std::mem::replace(self,Token::default()))
        } else {
            None
        }
    }
}

impl Default for Token {
    fn default() -> Self { 
        Token{kind: Box::new(Kind::None), value: Value::None}
    }
}


pub fn lexer(express: &str) -> VecDeque<Token> {
    let re = Regex::new( r"(?x)
        (?P<fn_op>=>)              # fn_operator
        |(?P<special>[=\(\)])    # special
        |(?P<ops>[\-+*/%\(\)])    # ops
        |(?P<letter>[A-Za-z_][A-Za-z0-9_]*) # letter
        |(?P<float>[+\-]?[0-9]*\.[0-9]+)  # float
        |(?P<int>[+\-]?[0-9]+)          # digital
        |(?P<whitespace>\s+)    # whitespace
        ").unwrap();
    let mut toks: VecDeque<Token> = VecDeque::new();
    for cap in re.captures_iter(express) {
        let tok = re.capture_names()
            .flatten()
            .filter_map(|n| {
                return Some((n,cap.name(n)?.as_str().to_string()));
            })
            .take(1).next().unwrap();
        let t: Option<Token> = match tok.0 {
            "int" => Some( Token::new( Kind::IntNumber(tok.1.parse::<i32>().unwrap())) ),
            "float" => Some( Token::new( Kind::FloatNumber(tok.1.parse::<f32>().unwrap())) ),
            "letter" => if tok.1 == "fn" {
                Some( Token::new( Kind::Keyword(tok.1)) )
            } else {
                Some( Token::new( Kind::Letter(tok.1)) )
            },
            "special" => match tok.1.as_str() {
                "=" => Some( Token::new( Kind::ASSIGN ) ),
                "(" => Some( Token::new( Kind::LPAREN ) ),
                ")" => Some( Token::new( Kind::RPAREN ) ),
                _ => None,
            },
            "fn_op" => Some( Token::new( Kind::FNOP ) ),
            "ops" => Some( Token::new( Kind::Op(tok.1)) ),
            _ => None,
        };
        if t.is_some() {
            toks.push_back(t.unwrap());
        }
    }

    return toks;
}

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
    Num{ 
        value: Value,
        next: Option<Box<Node>>,
    },
    Identifier{ 
        value: String,
        next: Option<Box<Node>>,
     },
}

impl Node {
    pub fn is_identifier(&self) -> bool {
        return is_enum_variant!(self, Node::Identifier{..});
    }

    pub fn identity_value(&self) -> Option<&String> {
        match self {
            Node::Identifier { value, .. } => Some(value),
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
        return std::mem::replace( &mut self.curr_token, new);
    }

    fn _function_call_parameter(&mut self) -> Result<Node, String> {
        let mut params: Vec<KindValue> = Vec::new();
        while is_enum_variant!(&*self.curr_token.kind, Kind::Letter(_)) 
            || is_enum_variant!(&*self.curr_token.kind, Kind::IntNumber(_)) 
            || is_enum_variant!(&*self.curr_token.kind, Kind::FloatNumber(_)) 
        {
            let mut ct = self.curr_token.take();
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
        if let Kind::Letter(fn_name) = &*self.curr_token.kind {
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
            _ => { },
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

    pub fn parse(&mut self, token: VecDeque<Token>, e: &mut dyn Evaluator) -> Result<Option<f32>,String> 
    {
        self.input = token;
        self.shift_input();
        let mut ast: Vec<Rc<Node>> = Vec::new();
        let mut last_value = None;
        loop {
            if self.curr_token.is_none() {
                break;
            }
            let n = self.stmt()?;
            let result = e.evaluate(&n)?;
            ast.push(Rc::new(n));
            last_value = Some(Rc::clone(&result));
        }

        if ast.len() > 1 {
            return Err("Syntax Error".to_string());
        }

        if let Some(r) = last_value {
            return Ok(r.get_result());
        }
        return Err("None".to_string());
    }
}


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

    pub fn insert(&mut self, new_sym: SymValue) -> Result<Option<Rc<SymValue>>,String>{
        let old_val = self.symbols.get(&new_sym.name);
        if let Some(old_symval) = old_val {
            match (&(*old_symval).kind_value, &new_sym.kind_value) {
                (SimKindValue::Function {..}, SimKindValue::Function {..}) => {},
                (SimKindValue::Value {..}, SimKindValue::Value { ..}) => {},
                (SimKindValue::Tuple {..}, SimKindValue::Tuple { ..}) => {},
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
struct Interpreter {
    pub func: HashMap<String,Rc<Node>>,
    pub current_scope: Box<ScopeSymbolTable>,
    //ast: Vec<Rc<Node>>,
    pub line: u32,
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

    fn push_stack_frame(&mut self, mut frame: ScopeSymbolTable) {
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
    //i.input("avg 1 3 2.0");
    //i.input("4 / 2 * 3");
    i.input("x = y = 713");
    //i.input("1 + 2 * 3 / 4 + 5");
    //println!("output={:?}", i.input("avg a b avg a b c"));
    //i.input(".1 + 1");
    //i.input("2 - 1");
    //i.input("2 * 3");
    //i.input("8 + 4 / 3 + (4 *2) % 3");
    //i.input("i = 4 / 3 + (4 *2) % 3");
    //i.input("7 % 4");
}

#[test]
fn test_empty() {
    let mut i = Interpreter::new();
    assert_eq!(i.input("  "), Ok(None));
}

#[test]
fn test_case5() {
    let mut i = Interpreter::new();
    assert_eq!(i.input("x=7"), Ok(Some(7.0)));
    assert_eq!(i.input("y = x + 5"), Ok(Some(12.0)));
    assert_eq!(i.input("y"), Ok(Some(12.0)));
}

#[test]
fn test_case3() {
    let mut i = Interpreter::new();
    assert_eq!(i.input("x = y = 713"), Ok(Some(713.0)));
}

#[test]
fn test_case1() {
    let mut i = Interpreter::new();
    //assert!(i.input("42 = 3.14").is_err()); // 숫자에 할당 
    //assert!(i.input("42 3").is_err()); // 숫자에 할당 
    assert_eq!(i.input("x = 29 + (y = 11)"), Ok(Some(40.0))); // 숫자에 할당 
    //assert_eq!(i.input("x"), Ok(Some(40.0))); // 숫자에 할당 
}

#[test]
fn test_case2() {
    let mut i = Interpreter::new();
    assert!(i.input("fn add x x => x + x").is_err());   // 인자 이름이 중복
}

#[test]
fn it_should_throw_an_error_when_function_contains_contains_duplicate_arguments() {
    let mut i = Interpreter::new();
    assert!(i.input("fn add x x => x + x").is_err());   // 인자 이름이 중복
    assert!(i.input("fn add x y => x + z").is_err()); // symbol이 없음
    assert!(i.input("42 = 3.14").is_err()); // 숫자에 할당 
    assert!(i.input("1 2").is_err()); // 문법에 없음
}

#[test]
fn it_should_parse_nested_functions() {
    let mut i = Interpreter::new();
    assert_eq!(i.input("fn f a b => a * b"), Ok(None));
    assert_eq!(i.input("fn g a b c => a * b * c"), Ok(None));
    assert_eq!(i.input("g g 1 2 3 f 4 5 f 6 7"), Ok(Some(5040.0)));
}

#[test]
fn it_should_call_chained_functions() {
    let mut i = Interpreter::new();
    assert_eq!(i.input("fn avg x y => (x + y) / 2"), Ok(None));
    assert_eq!(i.input("fn echo x => x"), Ok(None));
    assert_eq!(i.input("avg echo 4 echo 2"), Ok(Some(3.0)));
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
    println!("Hello, world!");
}
