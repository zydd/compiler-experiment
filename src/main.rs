use std::io::Read;

mod config;
mod parser;
mod compiler;
mod bytecode;

use config::CONFIG;

fn main() -> std::result::Result<(), Box<dyn std::error::Error>> {
    // println!("BC: {}", std::mem::size_of::<bytecode::BC>());

    let mut file = std::fs::File::open(&CONFIG.input).expect("open file");

    let mut code = String::new();
    file.read_to_string(&mut code).expect("read");

    let mut ast = parser::parse(code).expect("parse error");

    let runtime = bytecode::runtime::Runtime::new();
    let comp = compiler::compile(&runtime, &mut ast);

    let orig_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |panic_info| {
        orig_hook(panic_info);
        std::process::exit(1);
    }));

    if !CONFIG.compile_only {
        const STACK_SIZE: usize = 1024 * 1024 * 1024;
        let thread = std::thread::Builder::new()
            .name("interpreter".to_string())
            .stack_size(STACK_SIZE)
            .spawn(|| bytecode::execute(runtime, comp.0, comp.1))
            .unwrap();

        thread.join().unwrap_or(());
    }

    Ok(())
}
