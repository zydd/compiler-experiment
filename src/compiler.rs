use std::collections::HashMap;
use num_traits::FromPrimitive;

use crate::parser::*;
use crate::bytecode::*;

type Label = usize;


#[derive(Debug)]
enum CodeGen {
    Arg(usize),
    Fn(Function),
    // Builtin(BuiltinFunction),
}


#[derive(Clone, Debug)]
struct Function {
    label: usize,
    name: String,
    builtin: bool,
    code: Vec<BC>,
    arity: usize,
}


struct Context {
    label_index: Label,
    data: Vec<Value>,
    scope: Vec<HashMap<String, std::rc::Rc<Box<CodeGen>>>>,
}


impl Context {
    fn new() -> Context {
        let mut new = Context{
            label_index: 0,
            data: Vec::new(),
            scope: vec![HashMap::new()],
        };

        macro_rules! builtin {
            ($instr: ident, $arity: expr, $func: literal) => {
                let long_name = concat!("__builtin_", stringify!($instr));
                new.set($func.to_string(), CodeGen::Fn(Function::builtin(BuiltinFunction::$instr, $arity, $func.to_string())));
                new.set(long_name.to_string(), CodeGen::Fn(Function::builtin(BuiltinFunction::$instr, $arity, long_name.to_string())));
            };
            ($instr: ident, $arity: expr) => {
                let long_name = concat!("__builtin_", stringify!($instr));
                new.set(long_name.to_string(), CodeGen::Fn(Function::builtin(BuiltinFunction::$instr, $arity, long_name.to_string())));
            };
        }

        builtin!(Nop,       0);
        builtin!(Except,    0);
        builtin!(Invoke,    0);
        builtin!(Pop,       0);
        builtin!(Undefer,   0);

        builtin!(Add,   2,  "+");
        builtin!(Div,   2,  "/");
        builtin!(Mul,   2,  "*");
        builtin!(Sub,   2,  "-");

        builtin!(Eq,    2,  "==");
        builtin!(Geq,   2,  ">=");
        builtin!(Gt,    2,  ">" );
        builtin!(Leq,   2,  "<=");
        builtin!(Lt,    2,  "<" );
        builtin!(Neq,   2,  "!=");

        builtin!(Car,   1,  "car");
        builtin!(Cdr,   1,  "cdr");

        return new
    }

    fn new_label(&mut self) -> Label {
        let label = self.label_index;
        self.label_index += 1;
        return label
    }

    fn get(&self, key: &String) -> Option<std::rc::Rc<Box<CodeGen>>> {
        for scope in self.scope.iter().rev() {
            if let Some(v) = scope.get(key) {
                return Some(v.clone())
            }
        }

        println!("name not found in scope: {}", key);

        return None
    }

    fn set(&mut self, var: String, code: CodeGen) {
        let current = self.scope.last_mut().unwrap();
        current.insert(var, std::rc::Rc::new(Box::new(code)));
    }

    fn enter(&mut self) {
        self.scope.push(HashMap::new());
    }

    fn exit(&mut self) {
        self.scope.pop();
    }
}


impl CodeGen {
    fn value(&self) -> Vec<BC> {
        match self {
            CodeGen::Arg(i)         => vec![BC::Arg(i.clone())],
            CodeGen::Fn(func)       => func.value(),
        }
    }

    fn invoke(&self, ctx: &mut Context, expr: &[S]) -> Vec<BC> {
        match self {
            CodeGen::Arg(i)         => vec![BC::Arg(i.clone()), BC::Builtin(BuiltinFunction::Invoke)],
            CodeGen::Fn(func)       => func.call(ctx, expr, false, 0)
        }
    }

    fn function(&self) -> Option<&Function> {
        if let CodeGen::Fn(func) = self {
            return Some(func)
        } else {
            panic!("not a function: {:?}", self)
        }
    }
}


impl Function {
    fn builtin(id: BuiltinFunction, arity: usize, name: String) -> Function {
        let new = Function{
            builtin: true,
            label: id as usize,
            name: name,
            code: Vec::new(),
            arity: arity,
        };

        return new
    }

    fn compile(ctx: &mut Context, expr: &S) -> Function {
        let expr = expr.as_expr();

        let mut out = Vec::new();
        let mut func = Function{
            label: ctx.new_label(),
            name: expr[1].as_expr()[0].as_token().clone(),
            code: Vec::new(),
            arity: expr[1].as_expr().len() - 1,
            builtin: false,
        };

        assert_eq!(expr.len() % 2, 1); // 1 + (args, body) pairs

        let fn_end = ctx.new_label();
        out.extend([
            BC::Jump(fn_end),
            BC::Label(func.label),
        ]);

        ctx.enter();
        ctx.set(func.name.clone(), CodeGen::Fn(func.clone()));

        for def in expr[1..].chunks(2) {
            assert_eq!(def[0].as_expr().len(), func.arity + 1);
            assert_eq!(def[0].as_expr()[0].as_token(), &func.name);

            let case_end = ctx.new_label();
            ctx.enter();

            for (i, arg) in def[0].as_expr().iter().skip(1).enumerate() {
                match arg {
                    S::Token(argname) => if argname != "_" {
                        ctx.set(argname.clone(), CodeGen::Arg(i))
                    },
                    other => {
                        let addr = ctx.data.len();
                        ctx.data.push(Value::try_from(other).unwrap());

                        out.extend([
                            BC::Load(addr),
                            BC::Arg(i),
                            BC::Bne(case_end),
                        ]);
                    },
                }
            }

            if let S::S(expr) = &def[1] {
                let next = ctx.get(expr[0].as_token()).unwrap();
                let next = next.function().unwrap();
                out.extend(next.call(ctx, expr, false, func.arity));
            } else {
                out.extend(def[1].compile(ctx));
                out.push(BC::Return(func.arity));
            }
            out.push(BC::Label(case_end));

            ctx.exit();
        }

        ctx.exit();

        out.push(BC::Builtin(BuiltinFunction::Except));
        out.push(BC::Label(fn_end));

        func.code = out;
        return func
    }

    fn call(&self, ctx: &mut Context, expr: &[S], deferred: bool, move_args: usize) -> Vec<BC> {
        let mut out = Vec::new();
        let argc = expr.len() - 1;

        assert_eq!(expr[0].as_token(), &self.name);

        // compute args
        for el in expr.iter().skip(1).rev() {
            out.extend(el.compile(ctx));
        }

        if deferred {
            out.extend(self.value());
            out.push(BC::Defer(argc + 1));
        } else {
            if self.builtin {
                out.push(BC::Builtin(BuiltinFunction::from_usize(self.label).unwrap()));

                if move_args > 0 {
                    out.push(BC::Return(move_args));
                }
            } else if move_args > 0 {
                // tail call
                out.extend([
                    BC::MoveArgs(move_args),
                    BC::Jump(self.label),
                ]);
            } else {
                out.push(BC::Call(self.label));
            }

            if argc > self.arity {
                out.push(BC::ReturnCall(argc - self.arity));
            }
        }

        return out
    }

    fn value(&self) -> Vec<BC> {
        if self.builtin {
            return vec![BC::PushIns(self.label)]
        } else {
            return vec![BC::PushFn(self.label)]
        }
    }
}


impl TryFrom<&S> for Value {
    type Error = String;
    fn try_from(s: &S) -> Result<Self, Self::Error> {
        match s {
            S::Int(n)       => Ok(Value::Int(n.clone())),
            S::Float(n)     => Ok(Value::Float(n.clone())),
            S::Str(s)       => Ok(Value::Str(s.clone())),
            S::List(list)   =>
                match list.iter().map(|x| Value::try_from(x)).collect() {
                    Ok(conv_list) => Ok(Value::List(conv_list)),
                    Err(err) => Err(err)
                },
            _ => Err("unimplemented type conversion".to_string()),
        }
    }
}


impl S {
    fn compile(&self, ctx: &mut Context) -> Vec<BC> {
        let mut out = Vec::new();
        match self {
            S::S(expr) => {
                if expr.is_empty() {
                    out.push(BC::Builtin(BuiltinFunction::Nop));
                } else if let S::Token(name) = &expr[0] {
                    match name.as_str() {
                        "def" => {
                            let func = Function::compile(ctx, self);
                            out.extend(func.code.clone());
                            ctx.set(func.name.clone(), CodeGen::Fn(func));
                        },
                        _ => {
                            let func = ctx.get(name).unwrap();
                            out.extend(func.invoke(ctx, expr));
                        },
                    }
                } else {
                    panic!("not a function call: {:?}", expr)
                }
            },
            S::Int(n) => {
                let addr = ctx.data.len();
                ctx.data.push(Value::Int(n.clone()));
                out.push(BC::Load(addr));
            },
            S::Float(n) => {
                let addr = ctx.data.len();
                ctx.data.push(Value::Float(n.clone()));
                out.push(BC::Load(addr));
            },
            S::Str(n) => {
                let addr = ctx.data.len();
                ctx.data.push(Value::Str(n.clone()));
                out.push(BC::Load(addr));
            },
            S::List(_) => {
                let addr = ctx.data.len();
                ctx.data.push(Value::try_from(self).unwrap());
                out.push(BC::Load(addr));
            },
            S::Token(name) => {
                out.extend(ctx.get(name).unwrap_or_else(|| panic!("could not find name '{}'", name)).value());
            },
            S::Deferred(expr) => {
                if let S::Token(name) = &expr.as_expr()[0] {
                    match name.as_str() {
                        "def" => {
                            let func = Function::compile(ctx, self);
                            out.extend(func.code.clone());
                            ctx.set(func.name.clone(), CodeGen::Fn(func));
                        },
                        _ => {
                            let func = ctx.get(name).unwrap();
                            out.extend(func.function().unwrap().call(ctx, expr.as_expr(), true, 0));
                        },
                    }
                } else {
                    panic!("not a function call: {:?}", expr)
                }
            },
            _ => {
                panic!("unimplemented: {}", self)
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
