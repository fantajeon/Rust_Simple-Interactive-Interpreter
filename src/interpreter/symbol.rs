
use std::collections::HashMap;
use std::{rc::Rc};
use super::{basic::*, node::*};

#[derive(Debug)]
pub enum SimKindValue {
    Value{
        value: Rc<Value>,
    },
    Function{
        body: Rc<Node>,
        params: Rc<Vec<String>>,
    },
}
#[derive(Debug)]
 pub struct SymValue {
    pub name: String,
    pub kind_value: SimKindValue,
}

impl SymValue {
    //pub fn get_name(&self) -> &String {
    //    return &self.name;
    //}

    pub fn new_value(name: &str, value: Rc<Value>) -> Self {
        SymValue{ name: name.to_string(), kind_value: SimKindValue::Value{value: value} }
    }

    pub fn new_function(name: &str, params: Rc<Vec<String>>, node: Rc<Node>) -> Self {
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
pub struct ScopeSymbolTable {
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

    pub fn lookup(&self, sym: &str) -> Option<Rc<SymValue>> {
        if let Some(v) = self.symbols.get(sym) {
            return Some(Rc::clone(v));
        }

        //if let Some(parent) = &self.parent {
        //    return parent.lookup(sym);
        //}

        return None;
    }
}