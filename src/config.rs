use clap::Parser;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
pub struct Cli {
    #[arg(help = "Input file")]
    pub input: String,

    #[cfg(feature = "debug_output")]
    #[arg(long, help = "Print each instruction executed + stack")]
    pub debug_vm: bool,

    #[cfg(feature = "debug_output")]
    #[arg(long, help = "Print generated asm")]
    pub print_asm: bool,

    #[arg(short = 'c', long, help = "Do not execute program")]
    pub compile_only: bool,
}

lazy_static::lazy_static! {
    pub static ref CONFIG: Cli = Cli::parse();
}
