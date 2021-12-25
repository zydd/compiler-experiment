use std::collections::HashMap;

use crate::parser::*;
use crate::bytecode::*;


impl From<&S> for Value {
    fn from(s: &S) -> Self {
        match s {
            S::Int(n) => Value::Int(n.clone()),
            S::Float(n) => Value::Float(n.clone()),
            S::Str(s) => Value::Str(s.clone()),
            S::List(list) => Value::List(list.iter().map(|x| Value::from(x)).collect()),
            _ => panic!("unimplemented type conversion"),
        }
    }
}

trait Compile {
    fn compile(&self, ctx: &mut Context) -> Vec<BC>;
}


struct Context {
    label_index: usize,
    data: Vec<Value>,
    scope: Vec<HashMap<String, Vec<BC>>>,
    instr: HashMap<String, (BC, usize)>,
}

impl Context {
    fn new() -> Context {
        let mut new = Context{
            label_index: 0,
            data: Vec::new(),
            scope: vec![HashMap::new()],
            instr: HashMap::new(),
        };
        new.instr.insert("+".to_string(), (BC::Add, 2));
        new.instr.insert("<".to_string(), (BC::Lt, 2));
        new.instr.insert("car".to_string(), (BC::Car, 1));
        new.instr.insert("cdr".to_string(), (BC::Cdr, 1));
        return new
    }

    fn new_label(&mut self) -> usize {
        let label = self.label_index;
        self.label_index += 1;
        return label
    }

    fn get(&mut self, key: &String) -> Option<Vec<BC>> {
        for s in self.scope.iter().rev() {
            if let Some(instr) = s.get(key) {
                return Some(instr.clone())
            }
        }

        return None
    }

    fn set(&mut self, var: String, instr: &[BC]) {
        let current = self.scope.last_mut().unwrap();
        current.insert(var, Vec::from(instr));
    }

    fn enter(&mut self) {
        self.scope.push(HashMap::new());
    }

    fn exit(&mut self) {
        self.scope.pop();
    }
}

fn compile_function(ctx: &mut Context, expr: &S) -> Vec<BC> {
    let expr = expr.as_expr();
    let name = expr[1].as_expr()[0].as_token();
    let args = expr[1].as_expr().iter().skip(1).map(|x| x.as_token().clone());
    let body = expr[2].clone();
    let mut out = Vec::new();

    let label = ctx.new_label();

    ctx.set(name.clone(), &[BC::Call(label)]);
    ctx.enter();

    for (i, arg) in args.enumerate() {
        ctx.set(arg, &[BC::Arg(i)]);
    }

    let after = ctx.new_label();
    out.extend([
        BC::Jump(after),
        BC::Label(label),
    ]);
    out.extend(body.compile(ctx));
    out.extend([
        BC::Return,
        BC::Label(after),
    ]);

    ctx.exit();

    return out
}

fn compile_if(ctx: &mut Context, expr: &S) -> Vec<BC> {
    let expr = expr.as_expr();
    let condition = &expr[1];
    let true_case = &expr[2];
    let false_case = &expr[3];

    let mut out = Vec::new();

    let label_else = ctx.new_label();
    let label_end = ctx.new_label();

    out.extend(condition.compile(ctx));
    out.extend([
        BC::JumpZ(label_else)
    ]);
    out.extend(true_case.compile(ctx));
    out.extend([
        BC::Jump(label_end),
        BC::Label(label_else),
    ]);
    out.extend(false_case.compile(ctx));
    out.extend([
        BC::Label(label_end),
    ]);
    return out
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
                        "def" => out.extend(compile_function(ctx, self)),
                        "if" => out.extend(compile_if(ctx, self)),
                        _ => {
                            if ctx.instr.contains_key(name) {
                                let (ins, _argc) = ctx.instr[name].clone();
                                // assert_eq!(argc, expr.len() - 1);
                                for arg in expr.iter().skip(1) {
                                    out.extend(arg.compile(ctx));
                                }
                                out.push(ins);
                            } else {
                                // TODO: assert paramenter count
                                out.push(BC::PushFrame);

                                for arg in expr.iter().skip(1) {
                                    out.extend(arg.compile(ctx));
                                }
                                // function call
                                out.extend(ctx.get(name).expect(name));
                            }
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
                ctx.data.push(Value::from(self));
                out.push(BC::Push(addr));
            },
            S::Token(name) => {
                out.extend(ctx.get(name).expect(name))
            },
            _ => {
                panic!("unimplemented")
            }
        }
        return out
    }
}


pub fn compile(ast: &mut [S]) -> (Vec<BC>, Vec<Value>) {
    let mut out: Vec<BC> = Vec::new();

    let mut ctx = Context::new();

    for el in ast {
        out.extend(el.compile(&mut ctx));
    }
    return (out, ctx.data)
}
