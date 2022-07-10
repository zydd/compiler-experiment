use std::io::Read;

mod parser;
mod compiler;
mod bytecode;

fn main() {
    println!("BC: {}", std::mem::size_of::<bytecode::BC>());

    let path = std::env::args().nth(1).expect("path");
    let mut file = std::fs::File::open(path).expect("open file");

    let mut code = String::new();
    file.read_to_string(&mut code).expect("read");

    let ast = parser::parse(code).expect("parse error");

    // for topnode in &ast {
    //     println!("{}", topnode);
    // }
    let comp = compiler::compile(ast);
    println!("\n{:?}\n", comp.1);

    let orig_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |panic_info| {
        orig_hook(panic_info);
        std::process::exit(1);
    }));

    const STACK_SIZE: usize = 1024 * 1024 * 1024;
    let thread = std::thread::Builder::new()
        .name("interpreter".to_string())
        .stack_size(STACK_SIZE)
        .spawn(|| bytecode::execute(comp.0, comp.1))
        .unwrap();

    thread.join().unwrap_or(());
}
