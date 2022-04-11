
use std::rc::Rc;

use super::basic::Value;

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
    },
    Identifier{ 
        value: String,
    },
    FunctionCall {
       value: String,
       params: Vec<Node>,
       body: Rc<Node>,
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
}
