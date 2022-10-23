use super::*;

#[allow(unused_imports)]
use crate::CONFIG;


impl FunctionCall {
    fn compile(&self, ctx: &mut Context) -> Vec<BC> {
        let mut out = Vec::new();

        let func = self.function.as_ref().unwrap();

        for arg in self.args.iter().rev() {
            out.extend(Function::compile(ctx, arg.clone()));
        }

        if self.deferred {
            if let Function::Definition(fndef) = func {
                if ! ctx.used_definitions.iter().any(|x| RcRc::ptr_eq(x, func.definition().unwrap())) {
                    ctx.used_definitions.push(fndef.clone());
                }
            }

            out.extend(func.compile_as_value());
            out.extend([
                BC::Defer((self.args.len() + 1) as Argc)
            ]);
        } else {
            match func {
                Function::ArgRef(arg) => {
                    out.extend([
                        BC::Arg(arg.borrow().addr),
                    ]);
                    if let Some(move_args) = self.tail_call {
                        out.extend([
                            BC::MoveArgs(move_args),
                        ]);
                    }
                    out.extend([
                        BC::Builtin(ctx.builtin_addr(&"__builtin_invoke".to_string())),
                    ]);
                    if self.tail_call.is_some() {
                        out.extend([
                            BC::Return(0),
                        ]);
                    }
                }

                Function::Builtin(func_data) => {
                    if let Some(move_args) = self.tail_call {
                        out.extend([
                            BC::MoveArgs(move_args),
                        ]);
                    }
                    out.extend([
                        BC::Builtin(func_data.borrow().addr.clone()),
                    ]);
                    if self.tail_call.is_some() {
                        out.extend([
                            BC::Return(0),
                        ]);
                    }
                }

                Function::Definition(func_data) => {
                    if !ctx.used_definitions.iter().any(|x| RcRc::ptr_eq(x, func_data)) {
                        ctx.used_definitions.push(func_data.clone());
                    }
                    let excess_args = self.args.len() > func_data.borrow().arity as usize;

                    if !excess_args && self.tail_call.is_some() {
                        out.extend([
                            BC::MoveArgs(self.tail_call.unwrap()),
                        ]);
                    }

                    if !excess_args && self.tail_call.is_some() {
                        out.extend([
                            BC::Jump(func_data.borrow().label),
                        ]);
                    } else {  // excess_args || !tail_call
                        out.extend([
                            BC::Call(func_data.borrow().label),
                        ]);
                    }

                    if excess_args {
                        out.extend([
                            BC::ReturnCall(),
                        ]);

                        if self.tail_call.is_some() {
                            out.extend([
                                BC::Return(0),
                            ]);
                        }
                    }
                }

                Function::FunctionRef(func_def) => {
                    if !ctx.used_definitions.iter().any(|x| RcRc::ptr_eq(x, func_def)) {
                        ctx.used_definitions.push(func_def.clone());
                    }

                    if let Some(move_args) = self.tail_call {
                        out.extend([
                            BC::MoveArgs(move_args),
                        ]);
                    }

                    out.extend(func.compile_as_value());

                    out.extend([
                        BC::Builtin(ctx.builtin_addr(&"__builtin_invoke".to_string())),
                    ]);

                    if self.tail_call.is_some() {
                        out.extend([
                            BC::Return(0),
                        ]);
                    }
                }

                _ => panic!()
            }
        }

        return out
    }
}


impl FunctionCond {
    fn compile(&self, ctx: &mut Context) -> Vec<BC> {
        let mut out = Vec::new();
        let else_label = ctx.new_label();
        let if_end = ctx.new_label();

        out.extend(Function::compile(ctx, self.condition.clone()));

        out.extend([
            BC::Beqz(else_label),
        ]);

        out.extend(Function::compile(ctx, self.case_true.clone()));

        if self.case_true.is_value() && self.tail_call.is_some() {
            out.extend([
                BC::Return(self.tail_call.unwrap()),
            ]);
        } else {
            out.extend([
                BC::Jump(if_end),
            ]);
        }

        out.extend([
            BC::Label(else_label),
        ]);

        out.extend(Function::compile(ctx, self.case_false.clone()));

        if self.case_false.is_value() && self.tail_call.is_some() {
            out.extend([
                BC::Return(self.tail_call.unwrap()),
            ]);
        }

        out.extend([
            BC::Label(if_end),
        ]);

        return out
    }
}


impl FunctionDefinition {
    fn compile(&self, ctx: &mut Context) -> Vec<BC> {
        let mut out = Vec::new();

        out.extend([
            BC::Label(self.label),
        ]);

        for local in &self.locals {
            out.extend(Function::compile(ctx, local.clone()));
        }

        for arg in &self.args {
            let arg = arg.borrow();
            if arg.strict {
                out.extend([
                    BC::Branch(arg.addr),
                ]);
            }
        }

        out.extend(Function::compile(ctx, self.body.clone()));

        if self.body.is_value() {
            out.extend([
                BC::Return(self.args.len() as Argc),
            ]);
        }

        return out
    }
}


impl FunctionMatch {
    fn compile(&self, ctx: &mut Context) -> Vec<BC> {
        let mut out = Vec::new();
        let match_end = ctx.new_label();

        for (pattern, body) in &self.cases {
            let case_end = ctx.new_label();

            for (pat, arg) in pattern.iter().zip(&self.args) {
                match pat {
                    Function::Unknown(ukn) => {
                        if ukn.strict {
                            out.extend([
                                BC::Branch(arg.borrow().addr),
                            ]);
                        }
                    },

                    Function::Literal(_) => {
                        out.extend(Function::compile(ctx, Function::ArgRef(arg.clone())));
                        out.extend(pat.compile_as_value());
                        out.extend([
                            BC::Bne(case_end),
                        ]);
                    }

                    _ => todo!()
                }
            }

            out.extend(Function::compile(ctx, body.clone()));

            if body.is_value() && self.tail_call.is_some() {
                out.extend([
                    BC::Return(self.tail_call.unwrap()),
                ]);
            } else {
                out.extend([
                    BC::Jump(match_end),
                ]);
            }

            out.extend([
                BC::Label(case_end),
            ]);
        }

        out.extend([
            BC::Builtin(ctx.builtin_addr(&"__builtin_except".to_string())),
            BC::Label(match_end),
        ]);

        return out
    }
}


impl FunctionSeq {
    fn compile(&self, ctx: &mut Context) -> Vec<BC> {
        let mut out = Vec::new();

        out.extend([
            BC::Builtin(ctx.builtin_addr(&"__builtin_push_empty_list".to_string())),
        ]);

        for arg in self.args.iter() {
            out.extend(Function::compile(ctx, arg.clone()));

            out.extend([
                BC::Builtin(ctx.builtin_addr(&"__builtin_cons".to_string())),
            ]);
        }

        // if let Some(argc) = self.tail_call {
        //     out.extend([
        //         BC::Return(argc),
        //     ]);
        // }

        return out
    }
}


impl Function {
    fn compile(ctx: &mut Context, function: Function) -> Vec<BC> {
        let mut out = Vec::new();

        match &function {
            Function::Arg(_)            => panic!(),
            Function::ArgRef(data)      => out.push(BC::Arg(data.borrow().addr)),
            Function::Builtin(_)        => out.extend(function.compile_as_value()),
            Function::Call(data)        => out.extend(data.borrow().compile(ctx)),
            Function::Conditional(data) => out.extend(data.borrow().compile(ctx)),
            Function::Definition(data)  => out.extend(data.borrow().compile(ctx)),
            Function::FunctionRef(_)    => out.extend(function.compile_as_value()),
            Function::Literal(_)        => out.extend(function.compile_as_value()),
            // Function::Local(_)          => panic!(),
            Function::Match(data)       => out.extend(data.borrow().compile(ctx)),
            Function::Seq(data)         => out.extend(data.borrow().compile(ctx)),
            Function::Unknown(_)        => panic!(),
        }

        return out
    }

    fn compile_as_value(&self) -> Vec<BC> {
        match self {
            Function::Arg(arg_data) | Function::ArgRef(arg_data) => {
                return vec![
                    BC::Arg(arg_data.borrow().addr),
                ];
            }

            Function::Builtin(func_data) => {
                return vec![
                    BC::PushIns(func_data.borrow().addr.clone()),
                ];
            }

            Function::Definition(func_data) => {
                return vec![
                    BC::PushFn(func_data.borrow().label as Addr),
                ];
            }

            Function::FunctionRef(func_data) => {
                return vec![
                    BC::PushFn(func_data.borrow().label as Addr),
                ];
            }

            Function::Literal(literal) => {
                if let Value::Int(i) = literal.borrow().value {
                    return vec![
                        BC::PushInt(i as i32),
                    ];
                } else {
                    return vec![
                        BC::Load(literal.borrow().data_label),
                    ];
                }
            }

            Function::Call(_)
            | Function::Conditional(_)
            | Function::Match(_)
            | Function::Seq(_)
            | Function::Unknown(_)
            => panic!(),
        }
    }
}


fn link(program: &mut Vec<BC>) {
    let mut map_label_addr: HashMap<Label, Addr> = HashMap::new();
    let mut addr = 0;
    for i in 0..program.len() {
        if let BC::Label(index) = program[i] {
            map_label_addr.insert(index, addr);
        } else {
            program[addr as usize] = program[i].clone();
            addr += 1;
        }
    }
    program.truncate(addr as usize);
    for instr in program {
        match instr {
            BC::Jump(index)     => *instr = BC::Jump(map_label_addr[index]),
            BC::Bne(index)      => *instr = BC::Bne(map_label_addr[index]),
            BC::Beqz(index)      => *instr = BC::Beqz(map_label_addr[index]),
            BC::Call(index)     => *instr = BC::Call(map_label_addr[index]),
            BC::PushFn(index)   => *instr = BC::PushFn(map_label_addr[index]),
            _ => ()
        }
    }
}


pub fn compile(runtime: &Runtime, ast: &mut Vec<Function>) -> (Vec<BC>, Vec<Value>) {
    // let mut ast = ast.into_iter().map(|x| x.to_ref()).collect::<Vec<_>>();

    let mut ctx = Context::new(runtime);

    for mut el in ast.iter_mut() {
        Function::annotate(&mut ctx, &mut el);
        // println!("{}\n", el);
    }

    let mut out: Vec<BC> = Vec::new();
    for el in ast.iter_mut() {
        if el.definition().is_none() {
            out.extend(Function::compile(&mut ctx, el.clone()));
        }
    }

    out.extend([
        BC::Return(0),
    ]);

    // let mut def = 0;
    // while def < ctx.used_definitions.len() {
    //     let func = ctx.used_definitions[def].clone();
    //     out.extend(Function::compile(&mut ctx, Function::Definition(func)));
    //     def += 1;
    // }
    for el in ast.iter_mut() {
        if el.definition().is_some() {
            out.extend(Function::compile(&mut ctx, el.clone()));
        }
    }

    #[cfg(feature = "debug_output")]
    if CONFIG.print_asm {
        println!("\n{:?}\n", out);
    }

    link(&mut out);

    return (out, ctx.data)
}
