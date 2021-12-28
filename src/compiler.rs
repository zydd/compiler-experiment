use std::collections::HashMap;

use crate::parser::*;
use crate::bytecode::*;

type Label = usize;


enum CodeGen {
    Arg(usize),
    Fn(usize),
    Builtin(BuiltinFunction),
}


struct Context {
    label_index: Label,
    data: Vec<Value>,
    scope: Vec<HashMap<String, CodeGen>>,
}


impl Context {
    fn new() -> Context {
        let mut new = Context{
            label_index: 0,
            data: Vec::new(),
            scope: vec![HashMap::new()],
        };

        macro_rules! builtin {
            ($instr: ident, $func: literal) => {
                new.set($func.to_string(), CodeGen::Builtin(BuiltinFunction::$instr));
                new.set(concat!("__builtin_", stringify!($instr)).to_string(), CodeGen::Builtin(BuiltinFunction::$instr));
            };
            ( $instr: ident) => {
                new.set(concat!("__builtin_", stringify!($instr)).to_string(), CodeGen::Builtin(BuiltinFunction::$instr));
            };
        }

        builtin!(Nop);
        builtin!(Except);
        builtin!(Invoke);
        builtin!(Undefer);

        builtin!(Add, "+");
        builtin!(Div, "/");
        builtin!(Mul, "*");
        builtin!(Sub, "-");

        builtin!(Eq,  "==");
        builtin!(Geq, ">=");
        builtin!(Gt,  ">");
        builtin!(Leq, "<=");
        builtin!(Lt,  "<");
        builtin!(Neq, "!=");

        builtin!(Car, "car");
        builtin!(Cdr, "cdr");

        return new
    }

    fn new_label(&mut self) -> Label {
        let label = self.label_index;
        self.label_index += 1;
        return label
    }

    fn get(&self, key: &String) -> Option<&CodeGen> {
        for scope in self.scope.iter().rev() {
            if let Some(v) = scope.get(key) {
                return Some(v)
            }
        }

        println!("name not found in scope: {}", key);

        return None
    }

    fn set(&mut self, var: String, code: CodeGen) {
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


impl CodeGen {
    fn value(&self) -> Vec<BC> {
        match self {
            CodeGen::Builtin(id)    => vec![BC::PushIns(id.clone() as usize)],
            CodeGen::Arg(i)         => vec![BC::Arg(*i)],
            CodeGen::Fn(label)      => vec![BC::PushFn(*label)],
        }
    }
    fn invoke(&self) -> Vec<BC> {
        match self {
            CodeGen::Builtin(bc)    => vec![BC::Builtin(bc.clone())],
            CodeGen::Arg(i)         => vec![BC::Arg(*i), BC::Builtin(BuiltinFunction::Invoke)],
            CodeGen::Fn(label)      => vec![BC::Call(*label)],
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


fn compile_function(ctx: &mut Context, expr: &S) -> Vec<BC> {
    let expr = expr.as_expr();

    assert_eq!(expr.len() % 2, 1); // 1 + (args, body) pairs

    let name = expr[1].as_expr()[0].as_token();
    let argc = expr[1].as_expr().len() - 1;
    let mut out = Vec::new();

    let label = ctx.new_label();
    let fn_end = ctx.new_label();
    out.extend([
        BC::Jump(fn_end),
        BC::Label(label),
    ]);

    ctx.set(name.clone(), CodeGen::Fn(label));

    for def in expr[1..].chunks(2) {
        assert_eq!(def[0].as_expr().len(), argc + 1);
        assert_eq!(def[0].as_expr()[0].as_token(), name);

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
                        BC::Push(addr),
                        BC::Arg(i),
                        BC::Builtin(BuiltinFunction::Eq),
                        BC::JumpZ(case_end),
                    ]);
                },
            }
        }

        out.extend(def[1].compile(ctx));
        out.extend([
            BC::Return(argc),
            BC::Label(case_end)
        ]);

        ctx.exit();
    }

    out.push(BC::Builtin(BuiltinFunction::Except));
    out.push(BC::Label(fn_end));

    return out
}


fn compile_call(ctx: &mut Context, expr: &[S], deferred: bool) -> Vec<BC> {
    let mut out = Vec::new();
    let name = expr[0].as_token();
    let mut args = Vec::new();

    for el in expr.iter().skip(1).rev() {
        args.extend(el.compile(ctx));
    }

    let func = ctx.get(name).unwrap();
    out.extend(args);
    if deferred {
        out.extend(func.value());
        out.push(BC::Defer(expr.len()));
    } else {
        out.extend(func.invoke());
    }

    return out
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
                        "def"   => out.extend(compile_function(ctx, self)),
                        _       => out.extend(compile_call(ctx, expr, false)),
                    }
                } else {
                    panic!("not a function call: {:?}", expr)
                }
            },
            S::Int(n) => {
                let addr = ctx.data.len();
                ctx.data.push(Value::Int(n.clone()));
                out.push(BC::Push(addr));
            },
            S::Float(n) => {
                let addr = ctx.data.len();
                ctx.data.push(Value::Float(n.clone()));
                out.push(BC::Push(addr));
            },
            S::Str(n) => {
                let addr = ctx.data.len();
                ctx.data.push(Value::Str(n.clone()));
                out.push(BC::Push(addr));
            },
            S::List(_) => {
                let addr = ctx.data.len();
                ctx.data.push(Value::try_from(self).unwrap());
                out.push(BC::Push(addr));
            },
            S::Token(name) => {
                out.extend(ctx.get(name).unwrap_or_else(|| panic!("could not find name '{}'", name)).value());
            },
            S::Deferred(expr) => {
                out.extend(compile_call(ctx, expr.as_expr(), true));
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
