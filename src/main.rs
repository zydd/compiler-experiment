use std::io::Read;

mod parser;
// mod interpreter;
mod compiler;
mod bytecode;

fn main() {
    let path = std::env::args().nth(1).expect("path");
    let mut file = std::fs::File::open(path).expect("open file");

    let mut code = String::new();
    file.read_to_string(&mut code).expect("read");

    let ast = parser::parse(code).expect("parse error");

    // println!("{}", interpreter::execute(&ast));

    // use bytecode::BC::*;
    // use bytecode::{Value, List};
    // use bytecode::Value::Int;
    // let mut prog = vec![
    //     Push(0),
    //     JumpZ(1),

    //     Label(0),
    //     // Debug(100),
    //     AddA(0, 1),
    //     // Debug(100),
    //     StdReturn,

    //     Label(1),
    //     StdCall,
    //     Push(2),
    //     Push(2),
    //     Call(0),
    //     Pop(1),

    //     Push(4),
    //     Car(0),
    //     Cdr(0),
    // ];
    // bytecode::link(&mut prog);

    // let mut data: Vec<Value> = Vec::new();
    // data.insert(0, Int(0));
    // data.insert(1, Int(1));
    // data.insert(2, Int(2));
    // data.insert(3, Int(3));
    // data.insert(4, Value::List(List::from([Int(1), Int(2)])));

    // bytecode::execute(&prog, data);

    for el in &ast {
        println!("{}", el);
    }

    let mut ast = ast;
    let mut comp = compiler::compile(&mut ast);
    println!("{:?}", comp);

    bytecode::link(&mut comp.0);

    bytecode::execute(&comp.0, comp.1);
}
