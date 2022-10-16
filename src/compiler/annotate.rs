use super::*;


impl FunctionCall {
    fn annotate(ctx: &mut Context, function: &mut Function) {
        let mut call = &mut *function.call().unwrap().borrow_mut();

        for mut arg in &mut call.args {
            // Propagate down `deferred` tag
            if call.deferred {
                if let Function::Call(arg_call) = &mut arg {
                    arg_call.borrow_mut().deferred = true;
                }
            }

            Function::annotate(ctx, &mut arg);
        }

        if call.function.is_none() {
            let mut func_ref = ctx.get(&call.name).expect(&call.name);

            // if func_ref.try_borrow().is_err() {
            //     call.recursive = true;
            // } else
            if let Function::Arg(arg_data) = func_ref {
                let arg_ref = Function::ArgRef(arg_data.clone());
                arg_data.borrow_mut().refs += 1;

                func_ref = arg_ref;
            }

            call.function = Some(func_ref);
        } else {
            let mut callee_ref = call.function.as_mut().unwrap();
            if callee_ref.arg().is_some() {
                Function::annotate(ctx, &mut callee_ref);
            }
        }

        // if !call.recursive {
        //     if let Some(inlined) = call.inline(ctx) {
        //         *function_mut = inlined;
        //         continue;
        //     }
        // }
    }

    // fn inline(&mut self, ctx: &mut Context) -> Option<Function> {
    //     let function_ref = self.function.as_ref().unwrap();
    //     let callee = match &*function_ref.borrow() {
    //         Function::Definition(_)     => function_ref.clone(),
    //         Function::Function(func)    => func.clone(),
    //         Function::Builtin(_)        => return None,
    //         Function::ArgRef(_)         => return None,
    //         _ => panic!("{}", self.name)
    //     };

    //     let callee = callee.borrow();

    //     if callee.definition().is_some() {
    //         let callee = callee.definition().unwrap();

    //         for (argi, arg) in self.args.iter_mut().enumerate() {
    //             if let Function::ArgRef(arg_ref) = &*arg.borrow() {
    //                 let mut arg_ref = arg_ref.borrow_mut();
    //                 let arg_ref = arg_ref.arg_mut().unwrap();
    //                 arg_ref.refs.retain(|x| !RcRc::ptr_eq(x, arg));
    //             }
    //             // if !arg.borrow().is_value() {
    //             //     let callee_arg = &callee.args[argi].borrow();
    //             //     let callee_arg = callee_arg.arg().unwrap();
    //             //     let use_count = callee_arg.refs.len();

    //             //     // if an arg is not a value (e.g. function call), add it to a
    //             //     // local variable instead of instantiating multiple times
    //             //     if use_count > 1 {
    //             //         ctx.locals.last_mut().unwrap().push(arg.clone());
    //             //         *arg = Function::Local(FunctionArg {
    //             //             name: callee_arg.name.clone(),
    //             //             index: -(ctx.locals.last_mut().unwrap().len() as Argc),
    //             //             refs: Vec::new(),
    //             //         }).to_ref();
    //             //     }
    //             // }
    //         }

    //         let mut inlined = callee.clone().inline(&self.args);
    //         Function::flag_tail_call(&mut inlined, self.tail_call);
    //         return Some(inlined)
    //     } else {
    //         todo!();
    //     }

    //     return None
    // }
}


impl FunctionDefinition {
    fn annotate(ctx: &mut Context, function: &mut Function) {
        let mut def = function.definition().unwrap().borrow_mut();

        def.label = ctx.new_label();

        for arg in def.args.iter() {
            ctx.set(arg.borrow().name.clone(), Function::Arg(arg.clone()));
        }

        let arity = def.arity;
        Function::flag_tail_call(&mut def.body, Some(arity));

        Function::annotate(ctx, &mut def.body);  // WARNING: body borrowed here
    }

    // fn inline(self, argv: &Vec<Function>) -> Function {
    //     assert_eq!(argv.len(), self.args.len());

    //     for (arg, argv) in self.args.iter().zip(argv) {
    //         match &*arg.borrow() {
    //             Function::Arg(data) => {
    //                 for arg_ref in &data.refs {
    //                     let mut arg_use = arg_ref.borrow_mut();
    //                     *arg_use = argv.borrow().clone();

    //                     if let Function::ArgRef(parent_fn_arg) = &mut *arg_use {
    //                         let mut parent_fn_arg = parent_fn_arg.borrow_mut();
    //                         parent_fn_arg.arg_mut().unwrap().refs.push(arg_ref.clone());
    //                     }
    //                 }
    //             }

    //             _ => ()
    //         }
    //     }

    //     return self.body.borrow().clone()
    // }
}


impl FunctionLiteral {
    fn annotate(ctx: &mut Context, function: &mut Function) {
        let mut lit = function.literal().unwrap().borrow_mut();
        lit.data_label = ctx.data.len() as Label;
        ctx.data.push(lit.value.clone());
    }
}


impl FunctionMatch {
    fn annotate(ctx: &mut Context, function: &mut Function) {
        let fnmatch = &mut *function.fnmatch().unwrap().borrow_mut();

        for arg in &fnmatch.args {
            arg.borrow_mut().refs += 1;
            Function::annotate(ctx, &mut Function::Arg(arg.clone()));
        }

        for (pattern, body) in &mut fnmatch.cases {
            ctx.enter(function.clone());

            for (i, arg) in pattern.iter_mut().enumerate() {
                match &arg {
                    Function::Unknown(ukn) => if ukn.name != "_" {
                        ctx.set(ukn.name.clone(), Function::Arg(fnmatch.args[i].clone()));
                    }

                    Function::Literal(_) => FunctionLiteral::annotate(ctx, arg),

                    _ => todo!()
                }
            }

            Function::flag_tail_call(body, fnmatch.tail_call);

            Function::annotate(ctx, body);
            ctx.exit();
        }
    }
}


impl Function {
    pub(crate) fn annotate(ctx: &mut Context, function: &mut Function) {
        match &function {
            Function::Arg(_)        => (),
            Function::ArgRef(_)     => (),
            Function::Builtin(_)    => (),
            // Function::Local(_)      => (),
            Function::FunctionRef(_) => (),

            Function::Call(_)       => FunctionCall::annotate(ctx, function),
            Function::Match(_)      => FunctionMatch::annotate(ctx, function),
            Function::Literal(_)    => FunctionLiteral::annotate(ctx, function),

            Function::Definition(fndef) => {
                ctx.set(fndef.borrow().name.clone(), function.clone());
                ctx.enter(function.clone());

                FunctionDefinition::annotate(ctx, function);

                ctx.exit();
            }

            Function::Unknown(ukn) => {
                let func_ref = ctx.get(&ukn.name).expect(&ukn.name);

                // // if func_ref is already borrowed, assume it's a function definition
                // // referencing itself
                // if func_ref.try_borrow().is_err() {
                //     *function_mut = Function::Function(func_ref.clone());
                //     return
                // }

                match &func_ref {
                    Function::Arg(arg_data) => {
                        *function = Function::ArgRef(arg_data.clone());
                        arg_data.borrow_mut().refs += 1;
                    },

                    // Function::ArgRef(arg_data) => {
                    //     *function = Function::ArgRef(arg_data.clone());
                    //     arg_data.borrow_mut().refs += 1;
                    // },

                    Function::Definition(func_def) => {
                        *function = Function::FunctionRef(func_def.clone());
                    }

                    Function::Builtin(_) => {
                        *function = func_ref.clone();
                    }

                    _ => panic!("Unkn: {} {} {:?}", ukn.name, function, func_ref),
                }
            }
        }
    }

    fn flag_tail_call(func: &Function, tail_call: Option<Argc>) {
        match func {
            Function::Call(call) => call.borrow_mut().tail_call = tail_call,
            Function::Match(margs) => margs.borrow_mut().tail_call = tail_call,
            _ => ()
        }
    }
}
