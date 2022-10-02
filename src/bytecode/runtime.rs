use super::*;

use std::collections::HashMap;

type Builtin = fn(arch: &mut Arch);

#[derive(Debug)]
pub(crate) struct BuiltinInfo {
    pub index: usize,
    pub arity: usize,
}

pub struct Runtime {
    builtin: Vec<Builtin>,
    pub(crate) functioninfo: HashMap<String, BuiltinInfo>,
}

impl Runtime {
    pub fn new() -> Runtime {
        let mut runtime = Runtime {
            builtin: Vec::new(),
            functioninfo: HashMap::new(),
        };

        macro_rules! builtin {
            ($func_name:ident, $arity:expr) => {
                runtime.register(stringify!($func_name), Arch::$func_name, $arity);
            };

            ($func_name:ident, $arity:expr, $operator:literal) => {
                runtime.register(concat!("__builtin_", stringify!($func_name)), Arch::$func_name, $arity);
                runtime.register($operator, Arch::$func_name, $arity);
            };
        }

        runtime.register("__builtin_nop", |_arch| { }, 0);
        runtime.register("__builtin_pop", |arch| arch.pop(1), 0);
        runtime.register("__builtin_except", Arch::except, 0);
        runtime.register("__builtin_invoke", Arch::invoke, 0);
        runtime.register("__builtin_undefer", Arch::undefer, 0);

        builtin!(car,       1);
        builtin!(cdr,       1);
        builtin!(cons,      2);

        builtin!(debug,     1);
        builtin!(print,     1);
        builtin!(println,   1);

        builtin!(add,   2,  "+");
        builtin!(div,   2,  "/");
        builtin!(mul,   2,  "*");
        builtin!(sub,   2,  "-");

        builtin!(eq,    2,  "==");
        builtin!(geq,   2,  ">=");
        builtin!(gt,    2,  ">" );
        builtin!(leq,   2,  "<=");
        builtin!(lt,    2,  "<" );
        builtin!(neq,   2,  "!=");

        return runtime
    }

    fn register(&mut self, name: &str, func: Builtin, arity: usize) {
        self.functioninfo.insert(
            name.to_string(),
            BuiltinInfo {
                index: self.builtin.len(),
                arity: arity,
            }
        );
        self.builtin.push(func);
    }
}


impl std::fmt::Debug for Runtime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Runtime")
         .finish()
    }
}

impl Arch {
    pub(super) fn builtin(&mut self, index: usize) {
        self.runtime.builtin[index](self);
    }

    fn debug(&mut self) {
        let n = self.pop_undefer().as_int();
        let i = std::cmp::max(0, self.stack.len() as isize - n as isize) as usize;
        println!("Debug: ip: {} fp: {} stack: [{}..] {:?}", self.ip, self.fp, i, &self.stack[i..])
    }

    fn print(self: &mut Arch) {
        print!("{}", self.pop_undefer());
    }

    fn println(self: &mut Arch) {
        println!("{}", self.pop_undefer());
    }

    fn except(self: &mut Arch) {
        panic!("Builtin(Except) at {}", self.ip-1);
    }
}
