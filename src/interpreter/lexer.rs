
use regex::Regex;
use std::collections::{VecDeque};

use super::basic::*;


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
            "int" => Some( Token::new( Kind::IntNumber(tok.1.parse::<i32>().unwrap()), &tok.1) ),
            "float" => Some( Token::new( Kind::FloatNumber(tok.1.parse::<f32>().unwrap()), &tok.1) ),
            "letter" => if tok.1 == "fn" {
                Some( Token::new( Kind::Keyword(tok.1.clone()), &tok.1) )
            } else {
                Some( Token::new( Kind::Letter(tok.1.clone()), &tok.1) )
            },
            "special" => match tok.1.as_str() {
                "=" => Some( Token::new( Kind::ASSIGN, &tok.1 ) ),
                "(" => Some( Token::new( Kind::LPAREN, &tok.1 ) ),
                ")" => Some( Token::new( Kind::RPAREN, &tok.1 ) ),
                _ => None,
            },
            "fn_op" => Some( Token::new( Kind::FNOP, &tok.1 ) ),
            "ops" => Some( Token::new( Kind::Op(tok.1.clone()), &tok.1) ),
            _ => None,
        };
        if t.is_some() {
            toks.push_back(t.unwrap());
        }
    }

    return toks;
}