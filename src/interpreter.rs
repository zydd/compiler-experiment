use std::collections::HashMap;

use crate::parser::*;


#[derive(Clone)]
enum Function {
    Sys(fn(&mut Context, &[S]) -> S),
    Def(Vec<S>, S)
}


struct Context {
    stack: Vec<HashMap<String, Function>>
}

impl Context {
    fn new() -> Context {
        let mut new = Context{stack: vec![HashMap::new()]};
        let scope = new.stack.last_mut().unwrap();
        scope.insert("+".to_string(),
                Function::Sys(|ctx, list| { S::Int(list[0].eval(ctx).as_int() + list[1].eval(ctx).as_int()) }));
        scope.insert("seq".to_string(), Function::Sys(seq));
        scope.insert("def".to_string(), Function::Sys(def));
        scope.insert("if".to_string(),  Function::Sys(cond));
        scope.insert("car".to_string(), Function::Sys(car));
        scope.insert("cdr".to_string(), Function::Sys(cdr));
        return new
    }

    fn get(&self, key: &String) -> Option<&Function> {
        for scope in self.stack.iter().rev() {
            if let Some(value) = scope.get(key) {
                return Some(value)
            }
        }
        return None
    }

    fn push(&mut self, keys: &[S], values: &[S]) {
        if keys.len() != values.len() {
            panic!("Context.push array mismatch: {:?} {:?}", keys, values)
        }

        self.stack.push(HashMap::new());
        let scope = self.stack.last_mut().unwrap();

        for i in 0..keys.len() {
            // println!("{:?} = {:?}", keys[i], values[i]);
            scope.insert(keys[i].as_token().clone(), Function::Def(vec![], values[i].clone()));
        }
    }

    fn pop(&mut self) {
        self.stack.pop();
    }
}


fn call(ctx: &mut Context, list: &[S]) -> S {
    let name = list[0].eval(ctx);
    let func = ctx.get(name.as_token()).expect(name.as_token()).clone();
    return match func {
        Function::Sys(f) => f(ctx, &list[1..]),
        Function::Def(a, s) => {
            let values = list[1..].iter().map(|x| x.eval(ctx)).collect::<Vec<_>>();
            ctx.push(&a, &values);
            // println!("call: {:?}", list);
            let ret = s.eval(ctx);
            ctx.pop();
            return ret
        }
    }
}

fn cond(ctx: &mut Context, list: &[S]) -> S {
    return match list[0].eval(ctx) {
        S::Empty | S::Int(0) => list[2].eval(ctx),
        S::List(lst) =>
            if lst.is_empty() {
                return list[2].eval(ctx)
            } else {
                return list[1].eval(ctx)
            }
        _ => list[1].eval(ctx),
    }
}

fn car(ctx: &mut Context, list: &[S]) -> S {
    return list[0].eval(ctx).as_list()[0].clone();
}

fn cdr(ctx: &mut Context, list: &[S]) -> S {
    S::List(list[0].eval(ctx).as_list()[1..].to_vec())
}

fn seq(ctx: &mut Context, list: &[S]) -> S {
    let mut ret: Vec<S> = Vec::new();
    for el in list {
        ret.push(el.eval(ctx));
    }
    return S::S(ret)
}

fn def(ctx: &mut Context, list: &[S]) -> S {
    ctx.stack[0].insert(list[0].as_expr()[0].as_token().clone(), Function::Def(list[0].as_expr()[1..].to_vec(), list[1].clone()));
    return S::Empty
}


impl S {
    fn eval(&self, ctx: &mut Context) -> S {
        // println!("eval: {:?}", self);
        match self {
            S::Token(name) => {
                match ctx.get(name) {
                    Some(Function::Def(args, value)) => {
                        if args.is_empty() {
                            return value.clone().eval(ctx)
                        } else {
                            return self.clone()
                        }
                    }
                    _ => return self.clone()
                }
            }
            S::S(list)  => {
                if list.len() > 0 && matches!(list[0], S::Token(_)) {
                    return call(ctx, &list[..]);
                } else {
                    panic!("invalid call")
                }
            },
            any => {
                any.clone()
            }
        }
    }
}

pub fn execute(ast: &[S]) -> S {
    let mut ctx = Context::new();
    let ret = seq(&mut ctx, ast);

    // println!("{:?}", ctx.stack[0].keys());

    return ret
}
