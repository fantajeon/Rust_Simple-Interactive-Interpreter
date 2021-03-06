
use std::rc::Rc;

use super::basic::Value;

#[derive(Debug)]
pub enum Node {
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
    },
    Identifier{ 
        value: String,
    },
    FunctionCall {
       name: String,
       params_name: Rc<Vec<String>>,
       params: Vec<Node>,
       body: Rc<Node>,
    },
}

impl Node {
    pub fn identity_value(&self) -> Option<&String> {
        match self {
            Node::Identifier { value, .. } => Some(value),
            _ => None,
        }
    }
}
