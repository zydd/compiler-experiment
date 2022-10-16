use super::*;

use std::collections::HashMap;
use std::io::Write;

type Builtin = fn(&mut Arch);

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
            ($func_name:expr, $function:expr, $arity:expr) => {
                runtime.register(concat!("__builtin_", $func_name), $function, $arity);
                runtime.register($func_name, $function, $arity);
            };
        }

        macro_rules! builtin1 {
            ($func_name:expr, $function:expr) => {
                {
                    let arch_fn = |arch: &mut Arch| {
                        let a = arch.pop_undefer();
                        let ret = $function(a);
                        arch.stack.push(ret.into());
                    };

                    builtin!($func_name, arch_fn, 1);
                }
            };
        }

        macro_rules! builtin2 {
            ($func_name:expr, $operator:expr, $function:expr) => {
                {
                    let arch_fn = |arch: &mut Arch| {
                        let a = arch.pop_undefer();
                        let b = arch.pop_undefer();
                        let ret = $function(a, b);
                        arch.stack.push(ret.into());
                    };

                    builtin!($func_name, arch_fn, 2);
                    runtime.register($operator, arch_fn, 2);
                }
            };
        }

        runtime.register("__builtin_nop",           |_arch| { },        0);
        runtime.register("__builtin_pop",           |arch| arch.pop(1), 0);
        runtime.register("__builtin_except",        Arch::except,       0);
        runtime.register("__builtin_invoke",        Arch::invoke,       1);
        runtime.register("__builtin_undefer",       Arch::undefer,      1);
        runtime.register("__builtin_undefer_once",  Arch::undefer_once, 1);
        runtime.register("__builtin_debug",         Arch::debug,        1);

        builtin!("car",     Arch::car,      1);
        builtin!("cdr",     Arch::cdr,      1);
        builtin!("cons",    Arch::cons,     2);

        builtin!("print",   Arch::print,    1);
        builtin!("println", Arch::println,  1);
        builtin!("putchar", Arch::putchar,  1);

        builtin2!("add",    "+",    |a, b| a + b);
        builtin2!("div",    "/",    |a, b| a / b);
        builtin2!("mul",    "*",    |a, b| a * b);
        builtin2!("sub",    "-",    |a, b| a - b);
        builtin2!("lt",     "<",    |a, b| a <  b);
        builtin2!("leq",    "<=",   |a, b| a <= b);
        builtin2!("gt",     ">",    |a, b| a >  b);
        builtin2!("geq",    ">=",   |a, b| a >= b);
        builtin2!("eq",     "==",   |a, b| a == b);
        builtin2!("neq",    "!=",   |a, b| a != b);

        builtin1!("sqrt", |x: Value| Value::Float(x.as_float().sqrt()));

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

impl Arch<'_> {
    pub(super) fn builtin(&mut self, index: usize) {
        self.runtime.builtin[index](self);
    }

    pub(super) fn invoke(arch: &mut Arch) {
        match arch.stack.pop().unwrap() {
            Value::Function(addr)   => arch.call(addr),
            Value::Builtin(addr)    => arch.builtin(addr as usize),
            Value::Deferred(stack)  => {
                arch.stack.extend(stack);
                Arch::invoke(arch)
            },

            other => panic!("not invokable: {:?}", other),
        }
    }

    fn undefer(arch: &mut Arch) {
        while let Value::Deferred(_) = arch.stack.last().unwrap() {
            let call = arch.stack.pop().unwrap().as_deferred();
            arch.stack.extend(call);
            Arch::invoke(arch);
        }
    }

    fn car(arch: &mut Arch) {
        let head = arch.pop_undefer().list().unwrap()[0].clone();
        arch.stack.push(head);
    }

    fn cdr(arch: &mut Arch) {
        let mut tail = arch.pop_undefer().as_list_mut();
        // tail.pop_front();
        tail.remove(0);
        arch.stack.push(Value::List(tail));
    }

    fn cons(arch: &mut Arch) {
        let value = arch.pop_undefer();
        let mut list = arch.pop_undefer().as_list_mut();
        list.push_front(value);
        arch.stack.push(Value::List(list));
    }

    fn debug(arch: &mut Arch) {
        let n = arch.pop_undefer().as_int();
        let i = std::cmp::max(0, arch.stack.len() as isize - n as isize) as usize;
        println!("Debug: ip: {} fp: {} stack: [{}..] {:?}", arch.ip, arch.fp, i, &arch.stack[i..])
    }

    fn print(arch: &mut Arch) {
        print!("{}", arch.pop_undefer());
        arch.stack.push(Value::None);
    }

    fn println(arch: &mut Arch) {
        println!("{}", arch.stack.pop().unwrap());
        arch.stack.push(Value::None);
    }

    fn except(arch: &mut Arch) {
        panic!("Builtin(Except) at {}", arch.ip-1);
    }

    fn putchar(arch: &mut Arch) {
        let n = arch.pop_undefer().as_int();
        std::io::stdout().write(&[n as u8]).unwrap();
        arch.stack.push(Value::None);
    }

    fn undefer_once(arch: &mut Arch) {
        if let Value::Deferred(_) = arch.stack.last().unwrap() {
            let call = arch.stack.pop().unwrap().as_deferred();
            arch.stack.extend(call);
            Arch::invoke(arch);
        }
    }
}
