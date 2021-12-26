use std::collections::HashMap;

use crate::parser::*;
use crate::bytecode::*;

type Label = usize;

struct Context {
    label_index: Label,
    data: Vec<Value>,
    scope: Vec<HashMap<String, Code>>,
}


struct Function {
    // name: String,
    // args: Vec<String>,
    label: Label,
    // code: Vec<BC>,
}


enum Code {
    Arg(usize),
    Fn(Function),
    Instr(BC, usize)
}


impl From<&S> for Value {
    fn from(s: &S) -> Self {
        match s {
            S::Int(n)       => Value::Int(n.clone()),
            S::Float(n)     => Value::Float(n.clone()),
            S::Str(s)       => Value::Str(s.clone()),
            S::List(list)   => Value::List(list.iter().map(|x| Value::from(x)).collect()),
            _ => panic!("unimplemented type conversion"),
        }
    }
}


impl Context {
    fn new() -> Context {
        let mut new = Context{
            label_index: 0,
            data: Vec::new(),
            scope: vec![HashMap::new()],
        };
        new.set("+".to_string(), Code::Instr(BC::Add, 2));
        new.set("<".to_string(), Code::Instr(BC::Lt, 2));
        new.set("car".to_string(), Code::Instr(BC::Car, 1));
        new.set("cdr".to_string(), Code::Instr(BC::Cdr, 1));
        return new
    }

    fn new_label(&mut self) -> Label {
        let label = self.label_index;
        self.label_index += 1;
        return label
    }

    fn get(&self, key: &String) -> Option<&Code> {
        for scope in self.scope.iter().rev() {
            if let Some(v) = scope.get(key) {
                return Some(v)
            }
        }

        return None
    }

    fn set(&mut self, var: String, code: Code) {
        let current = self.scope.last_mut().unwrap();
        current.insert(var, code);
    }

    fn enter(&mut self) {
        self.scope.push(HashMap::new());
    }

    fn exit(&mut self) {
        self.scope.pop();
    }
}


impl Code {
    fn value(&self) -> Vec<BC> {
        match self {
            // Code::Instr(bc, _args) => vec![bc.clone()],
            Code::Arg(i) => vec![BC::Arg(*i)],
            Code::Fn(func) => vec![BC::PushFn(func.label)],
            _ => panic!()
        }
    }
    fn invoke(&self) -> Vec<BC> {
        match self {
            Code::Instr(bc, _args) => vec![bc.clone()],
            Code::Arg(i) => vec![BC::Arg(*i), BC::Invoke],
            Code::Fn(func) => vec![BC::Call(func.label)],
        }
    }
}


fn compile_function(ctx: &mut Context, expr: &S) -> Vec<BC> {
    let expr = expr.as_expr();
    let name = expr[1].as_expr()[0].as_token();
    let args = expr[1].as_expr().iter().skip(1).map(|x| x.as_token().clone());
    let argc = args.len();
    let body = expr[2].clone();
    let mut out = Vec::new();

    let label = ctx.new_label();

    ctx.set(name.clone(), Code::Fn(Function{
        // name: name.clone(),
        // args: Vec::new(),
        label: label,
        // code: Vec::new(),
    }));
    ctx.enter();

    for (i, arg) in args.enumerate() {
        ctx.set(arg, Code::Arg(i));
    }

    let after = ctx.new_label();
    out.extend([
        BC::Jump(after),
        BC::Label(label),
    ]);
    out.extend(body.compile(ctx));
    out.extend([
        BC::Return(argc),
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


impl S {
    fn compile(&self, ctx: &mut Context) -> Vec<BC> {
        let mut out = Vec::new();
        match self {
            S::S(expr) => {
                if let S::Token(name) = &expr[0] {
                    match name.as_str() {
                        "def" => out.extend(compile_function(ctx, self)),
                        "if" => out.extend(compile_if(ctx, self)),
                        _ => {
                            let mut args = Vec::new();
                            for el in expr.iter().skip(1).rev() {
                                args.extend(el.compile(ctx));
                            }

                            if let Some(func) = ctx.get(name) {
                                out.extend(args);
                                out.extend(func.invoke());
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
                out.extend(ctx.get(name).unwrap().value())
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
