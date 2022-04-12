#[macro_use]
mod interpreter;

use interpreter::*;


#[test]
fn test_basic_arithmetic() {
    let mut i = Interpreter::new();
    //i.input("y");
    i.input("a = 1").unwrap();
    //i.input("b = 10");
    //i.input("c = 100");
    //i.input("a + 1 + b");
    //i.input("a + b + c + 1");
    i.input("fn avg a b c => a + b + c + 1").unwrap();
    //i.input("avg a b c");
    i.input("avg a 1 2.0").unwrap();
    //i.input("4 / 2 * 3");
    //i.input("x = y = 713");
    //i.input("1 + 2 * 3 / 4 + 5");
    //println!("output={:?}", i.input("avg a b avg a b c"));
    //i.input(".1 + 1");
    //i.input("2 - 1");
    //i.input("2 * 3");
    i.input("8 + 4 / 3 + (4 *2) % 3").unwrap();
    //i.input("i = 4 / 3 + (4 *2) % 3");
    //i.input("7 % 4");
}

#[test]
fn test_op_sequence() {
    let mut i = Interpreter::new();
    assert_eq!(i.input("8 + 4 / 3 + (4 *2) % 3"), Ok(Some(11.333333)));
}

#[test]
fn test_empty() {
    let mut i = Interpreter::new();
    assert_eq!(i.input("  "), Ok(None));
    assert_eq!(i.input(""), Ok(None));
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
    let mut i = Interpreter::new();
    i.input("a = 1").unwrap();
    i.input("fn avg a b c => a + b + c + 1").unwrap();
    i.input("avg a 1 2.0").unwrap();
}