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
    //     Jump(2),
    //     Label(1),
    //     LtA(1,2),
    //     JumpZ(3),
    //     PushFrame,
    //     AddA(0,1),
    //     Push(0),
    //     Arg(1),
    //     Add,
    //     Arg(2),
    //     Call(1),
    //     Jump(4),
    //     Label(3),
    //     Arg(0),
    //     Label(4),
    //     Return,

    //     Label(2),
    //     PushFrame,
    //     Push(1),
    //     Push(2),
    //     Push(3),
    //     Call(1),
    // ];
    // bytecode::link(&mut prog);

    // let mut data: Vec<Value> = Vec::from([
    //     Int(1),
    //     Int(0),
    //     Int(0),
    //     Int(10000001)
    // ]);

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
