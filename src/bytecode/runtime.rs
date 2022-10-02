use super::*;

use std::collections::HashMap;

use crate::bytecode::*;

type Syscall = fn(arch: &mut Arch);

#[derive(Debug)]
pub(crate) struct SyscallInfo {
    pub index: usize,
    pub arity: usize,
}

pub(crate) struct Runtime {
    syscall: Vec<Syscall>,
    pub callinfo: HashMap<String, SyscallInfo>,
}

impl Runtime {
    pub fn new() -> Runtime {
        let mut runtime = Runtime {
            syscall: Vec::new(),
            callinfo: HashMap::new(),
        };

        macro_rules! register_call {
            ($func_name:ident, $arity:expr) => {
                runtime.callinfo.insert(
                    stringify!($func_name).to_string(),
                    SyscallInfo {
                        index: runtime.syscall.len(),
                        arity: $arity,
                    }
                );
                runtime.syscall.push(Arch::$func_name);
            };
        }

        register_call!(debug,   1);
        register_call!(print,   1);
        register_call!(println, 1);

        return runtime
    }
}


impl std::fmt::Debug for Runtime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Runtime")
         .finish()
    }
}

impl Arch {
    pub(super) fn syscall(&mut self, index: usize) {
        self.runtime.syscall[index](self);
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
}
