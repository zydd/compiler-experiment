use std::collections::HashMap;

use crate::parser::*;
use crate::bytecode::*;


trait Compile {
    fn compile(&self, ctx: &mut Context) -> Vec<BC>;
}


#[derive(Debug)]
struct Function {
    label: usize,
    name: String,
    args: Vec<String>,
    body: S,
}

impl Function {
    fn from(label: usize, el: &S) -> Function {
        let list = el.as_expr();
        let args = list[1].as_expr().iter().skip(1).map(|x| x.as_token().clone()).collect::<Vec<_>>();

        return Function{
            label: label,
            name: list[1].as_expr()[0].as_token().clone(),
            args: args,
            body: list[2].clone(),
        };
    }
}

impl Compile for Function {
    fn compile(&self, ctx: &mut Context) -> Vec<BC> {
        let mut out = Vec::new();

        let label = ctx.new_label();
        let arg_instrs = (0..self.args.len()).map(|i| BC::Arg(i)).collect::<Vec<_>>();

        ctx.set(self.name.clone(), BC::Call(label));
        ctx.enter(&self.args, &arg_instrs);

        let after = ctx.new_label();
        out.extend([
            BC::Jump(after),
            BC::Label(label),
        ]);
        out.extend(self.body.compile(ctx));
        out.extend([
            BC::Return,
            BC::Label(after),
        ]);

        ctx.exit();

        return out
    }
}


#[derive(Debug)]
struct Conditional {
    condition: S,
    true_case: S,
    false_case: S,
}

impl Conditional {
    fn from(el: &S) -> Conditional {
        let expr = el.as_expr();

        return Conditional{
            condition: expr[1].clone(),
            true_case: expr[2].clone(),
            false_case: expr[3].clone(),
        };
    }
}

impl Compile for Conditional {
    fn compile(&self, ctx: &mut Context) -> Vec<BC> {
        let mut out = Vec::new();

        let label_else = ctx.new_label();
        let label_end = ctx.new_label();

        out.extend(self.condition.compile(ctx));
        out.extend([
            BC::JumpZ(label_else)
        ]);
        out.extend(self.true_case.compile(ctx));
        out.extend([
            BC::Jump(label_end),
            BC::Label(label_else),
        ]);
        out.extend(self.false_case.compile(ctx));
        out.extend([
            BC::Label(label_end),
        ]);
        return out
    }
}


struct Context {
    label_index: usize,
    data: Vec<Value>,
    scope: Vec<HashMap<String, BC>>
}

impl Context {
    fn new() -> Context {
        return Context{
            label_index: 0,
            data: Vec::new(),
            scope: vec![HashMap::new()],
        }
    }

    fn new_label(&mut self) -> usize {
        let label = self.label_index;
        self.label_index += 1;
        return label
    }

    fn get(&self, key: &String) -> Option<&BC> {
        for s in self.scope.iter().rev() {
            if let Some(instr) = s.get(key) {
                return Some(instr)
            }
        }
        return None
    }


    fn set(&mut self, var: String, instr: BC) {
        let current = self.scope.last_mut().unwrap();
        current.insert(var, instr);
    }

    fn enter(&mut self, vars: &[String], instrs: &[BC]) {
        if vars.len() != instrs.len() {
            panic!("Context.enter array mismatch: {:?} {:?}", vars, instrs)
        }

        self.scope.push(HashMap::new());
        let current = self.scope.last_mut().unwrap();

        for i in 0..vars.len() {
            // println!("{:?} = {:?}", keys[i], values[i]);
            current.insert(vars[i].clone(), instrs[i].clone());
        }
    }

    fn exit(&mut self) {
        self.scope.pop();
    }
}


impl Compile for S {
    fn compile(&self, ctx: &mut Context) -> Vec<BC> {
        let mut out = Vec::new();
        match self {
            S::S(expr) => {
                // if expr.is_empty() {
                //     let addr = ctx.data.len();
                //     ctx.data.push(Value::List(List::new()));
                //     out.push(BC::Push(addr));
                // } else 
                if let S::Token(name) = &expr[0] {
                    match name.as_str() {
                        "def" => {
                            let fn_def = Function::from(ctx.new_label(), self);
                            // println!("{:?}", &fn_def);
                            out.extend(fn_def.compile(ctx));
                        },
                        "if" => {
                            let cond = Conditional::from(self);
                            // println!("{:?}", cond);
                            out.extend(cond.compile(ctx));
                        },
                        "+" => {
                            out.extend(expr[1].compile(ctx));
                            out.extend(expr[2].compile(ctx));
                            out.push(BC::Add);
                        },
                        "<" => {
                            out.extend(expr[1].compile(ctx));
                            out.extend(expr[2].compile(ctx));
                            out.push(BC::Lt);
                        },
                        _ => { // generic function call
                            // TODO: assert paramenter count
                            out.push(BC::PushFrame);
                            for arg in expr.iter().skip(1) {
                                out.extend(arg.compile(ctx));
                            }
                            // function call
                            out.push(ctx.get(name).expect(name).clone());
                        },
                    }
                }
            },
            S::Int(n) => {
                let addr = ctx.data.len();
                ctx.data.push(Value::Int(n.clone()));
                out.push(BC::Push(addr));
            },
            S::List(_) => {
                let addr = ctx.data.len();
                // TODO: support list
                ctx.data.push(Value::List(List::new()));
                out.push(BC::Push(addr));
            },
            S::Token(name) => {
                out.push(ctx.get(name).expect(name).clone())
            },
            _ => {
                println!("{:?}", self);
                out.push(BC::Debug(137));
            }
        }
        return out
    }
}


pub fn compile(ast: &mut [S]) -> (Vec<BC>, Vec<Value>) {
    let mut out: Vec<BC> = Vec::new();

    let mut ctx = Context::new();

    // // first pass match only global functions
    // for el in ast.iter_mut() {
    //     if let S::S(list) = el {
    //         if list.is_empty() {
    //             continue
    //         }

    //         if let S::Token(name) = &list[0] {
    //             if name == "def" {
    //                 let fn_def = Function::from(ctx.new_label(), el);
    //                 // println!("{:?}", &fn_def);
    //                 out.extend(fn_def.compile(&mut ctx));
    //                 *el = S::Empty;
    //             }
    //         }
    //     }
    // }

    for el in ast {
        out.extend(el.compile(&mut ctx));
    }
    return (out, ctx.data)
}
