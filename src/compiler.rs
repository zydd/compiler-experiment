use std::collections::HashMap;

use crate::bytecode::*;

type Label = Addr;
type FunctionRef = std::rc::Rc<std::cell::RefCell<Function>>;

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionArg {
    name: String,
    index: Argc,
    // deferred: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionBuiltin {
    name: String,
    arity: Argc,
    opcode: BuiltinFunction,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCall {
    name: String,
    argc: Argc,
    label: Label,
    args: Vec<FunctionRef>,
    tail_call: Option<Argc>,
    pub deferred: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDefinition {
    name: String,
    arity: Argc,
    label: Label,
    defs: Vec<(Vec<FunctionRef>, FunctionRef)>
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionLiteral {
    data_label: Addr,
    pub value: Value,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Function {
    Arg(FunctionArg),
    Builtin(FunctionBuiltin),
    Call(FunctionCall),
    Definition(FunctionDefinition),
    Literal(FunctionLiteral),
    Unknown(String),
}


pub struct Context {
    label_index: Label,
    data: Vec<Value>,
    scope: Vec<HashMap<String, FunctionRef>>,
}


impl Context {
    pub fn new() -> Context {
        let mut new = Context{
            label_index: 1,
            data: Vec::new(),
            scope: vec![HashMap::new()],
        };

        macro_rules! builtin {
            ($instr: ident, $arity: expr, $func: literal) => {
                let long_name = concat!("__builtin_", stringify!($instr));
                new.set($func.to_string(), FunctionBuiltin::new(BuiltinFunction::$instr, $arity, $func.to_string()).to_ref());
                new.set($func.to_string(), FunctionBuiltin::new(BuiltinFunction::$instr, $arity, long_name.to_string()).to_ref());
            };
            ($instr: ident, $arity: expr) => {
                let long_name = concat!("__builtin_", stringify!($instr));
                new.set(long_name.to_string(), FunctionBuiltin::new(BuiltinFunction::$instr, $arity, long_name.to_string()).to_ref());
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

    fn get(&self, key: &String) -> Option<FunctionRef> {
        for scope in self.scope.iter().rev() {
            if let Some(v) = scope.get(key) {
                return Some(v.clone())
            }
        }

        println!("name not found in scope: {}", key);

        return None
    }

    fn set(&mut self, name: String, function: FunctionRef) {
        let current = self.scope.last_mut().unwrap();
        current.insert(name, function.clone());
    }

    fn enter(&mut self) {
        self.scope.push(HashMap::new());
    }

    fn exit(&mut self) {
        self.scope.pop();
    }
}


impl FunctionArg {
    pub fn new(index: Argc, name: String) -> Function {
        return Function::Arg(FunctionArg{
            name: name,
            index: index,
            // deferred: false,
        })
    }
}

impl FunctionBuiltin {
    pub fn new(id: BuiltinFunction, arity: Argc, name: String) -> Function {
        return Function::Builtin(FunctionBuiltin{
            opcode: id,
            name: name,
            arity: arity,
        })
    }
}

impl FunctionDefinition {
    pub fn new(expr: Vec<Function>) -> Function {
        assert_eq!(expr.len() >= 3, true);
        assert_eq!(expr.len() % 2, 1); // def + (args, body) pairs
        assert_eq!(matches!(&expr[0], Function::Unknown(name) if name == "def"), true);

        let call = expr[1].call().unwrap();
        let mut func = FunctionDefinition{
            name: call.name.clone(),
            arity: call.args.len() as Argc,
            label: 0,
            defs: Vec::new()
        };

        for def in expr[1..].chunks(2) {
            let call = def[0].call().unwrap();
            assert_eq!(call.name, func.name);
            assert_eq!(matches!(&def[0], Function::Call{..}), true);

            func.defs.push((call.args.clone(), def[1].clone().to_ref()));
        }

        return Function::Definition(func)
    }

    fn compile(&mut self, ctx: &mut Context) -> Vec<BC> {
        let mut out = Vec::new();

        self.label = ctx.new_label();
        ctx.set(self.name.clone(), Function::Definition(self.clone()).to_ref());

        let fn_end = ctx.new_label();
        out.extend([
            BC::Jump(fn_end),
            BC::Label(self.label),
        ]);
        ctx.enter();

        for (args, def) in &self.defs {
            let case_end = ctx.new_label();
            ctx.enter();

            for (i, arg) in args.iter().enumerate() {
                match &*arg.borrow() {
                    Function::Unknown(name) => if name != "_" {
                        ctx.set(name.clone(), FunctionArg::new(i as Argc, name.clone()).to_ref());
                    }

                    Function::Literal(literal) => {
                        let addr = ctx.data.len() as Addr;
                        ctx.data.push(Value::try_from(literal.value.clone()).unwrap());

                        out.extend([
                            BC::Load(addr),
                            BC::Arg(i as Argc),
                            BC::Bne(case_end),
                        ]);
                    }

                    _ => todo!()
                }
            }

            if def.borrow().call().is_some() {
                // tail_call makes the Call responsible for handling the return
                def.borrow_mut().call_mut().unwrap().tail_call = Some(self.arity);
                out.extend(Function::compile(ctx, def.clone()));
            } else {
                out.extend(Function::compile(ctx, def.clone()));
                out.extend([
                    BC::Return(self.arity),
                ]);
            }

            out.push(BC::Label(case_end));
            ctx.exit();
        }
        out.extend([
            BC::Builtin(BuiltinFunction::Except),
            BC::Label(fn_end),
        ]);

        return out
    }
}

impl FunctionCall {
    pub fn new(expr: Vec<Function>) -> Function {
        assert_eq!(expr.len() > 0, true);
        assert_eq!(matches!(expr[0], Function::Unknown(_)), true);

        if let Function::Unknown(name) = &expr[0] {
            return Function::Call(FunctionCall{
                name: name.clone(),
                argc: (expr.len() - 1) as Argc,
                label: 0,
                args: expr[1..].iter().map(|x| x.clone().to_ref()).collect(),
                tail_call: None,
                deferred: false,
            })
        } else {
            panic!()
        }
    }

    fn compile(&mut self, ctx: &mut Context) -> Vec<BC> {
        let mut out = Vec::new();

        let func = ctx.get(&self.name).expect(&self.name);
        let func = func.borrow();

        // compute args
        for arg in self.args.iter().rev() {
            out.extend(Function::compile(ctx, arg.clone()));
        }

        if self.deferred {
            out.extend(Function::fn_as_value(&func));
            out.push(BC::Defer(self.args.len() as Argc + 1));
        } else {
            match &*func {
                Function::Arg(arg_data) => {
                    out.extend([
                        BC::Arg(arg_data.index),
                    ]);
                    if let Some(move_args) = self.tail_call {
                        out.extend([
                            BC::MoveArgs(move_args),
                        ]);
                    }
                    out.extend([
                        BC::Builtin(BuiltinFunction::Invoke),
                    ]);
                    if self.tail_call.is_some() {
                        out.extend([
                            BC::Return(0),
                        ]);
                    }
                }

                Function::Builtin(func_data) => {
                    out.extend([
                        BC::Builtin(func_data.opcode.clone()),
                    ]);

                    if let Some(move_args) = self.tail_call {
                        out.extend([
                            BC::Return(move_args),
                        ]);
                    }
                }

                Function::Definition(func_data) => {
                    if let Some(move_args) = self.tail_call {
                        out.extend([
                            BC::MoveArgs(move_args),
                        ]);
                    }

                    if self.args.len() > func_data.arity as usize {
                        // do not Jump if "over-currying"
                        out.extend([
                            BC::Call(func_data.label),
                            BC::ReturnCall(self.args.len() as Argc - func_data.arity),
                        ]);
                        if let Some(_) = self.tail_call {
                            out.extend([
                                // if tail_call is set, the return of the callee is handled here
                                BC::Return(0),
                            ]);
                        }
                    } else if let Some(_) = self.tail_call {
                        out.extend([
                            BC::Jump(func_data.label),
                        ]);
                    } else {
                        out.extend([
                            BC::Call(func_data.label),
                        ]);
                    }
                }

                _ => panic!()
            }
        }

        return out
    }
}

impl FunctionLiteral {
    pub fn new(value: Value) -> Function {
        return Function::Literal(FunctionLiteral{
            data_label: 0,
            value: value,
        })
    }
}


impl Function {
    pub fn new(name: String) -> Function {
        return Function::Unknown(name)
    }

    pub fn call(&self) -> Option<&FunctionCall> {
        match self {
            Function::Call(data) => Some(data),
            _ => None
        }
    }

    pub fn call_mut(&mut self) -> Option<&mut FunctionCall> {
        match self {
            Function::Call(data) => Some(data),
            _ => None
        }
    }

    pub fn literal(&self) -> Option<&FunctionLiteral> {
        match self {
            Function::Literal(data) => Some(data),
            _ => None
        }
    }

    // pub fn definition_mut(&mut self) -> Option<&mut FunctionDefinition> {
    //     match self {
    //         Function::Definition(data) => Some(data),
    //         _ => None
    //     }
    // }

    fn to_ref(self) -> FunctionRef {
        std::rc::Rc::new(std::cell::RefCell::new(self))
    }

    fn compile(ctx: &mut Context, function: FunctionRef) -> Vec<BC> {
        let mut out = Vec::new();

        match &mut *function.borrow_mut() {
            Function::Definition(fndef) => out.extend(fndef.compile(ctx)),
            Function::Call(call)        => out.extend(call.compile(ctx)),

            Function::Literal(literal)  => {
                literal.data_label = ctx.data.len() as Label;
                ctx.data.push(literal.value.clone());
                out.push(BC::Load(literal.data_label));
            }

            Function::Unknown(name)     => {
                let func = ctx.get(&name).unwrap();
                let func = func.borrow();

                out.extend(Function::fn_as_value(&func));
            }

            _ => ()
        }
        return out
    }

    fn fn_as_value(data: &Function) -> Vec<BC> {
        match data {
            Function::Arg(arg_data) => {
                return vec![
                    BC::Arg(arg_data.index),
                ];
            }

            Function::Builtin(func_data) => {
                return vec![
                    BC::PushIns(func_data.opcode.clone()),
                ];
            }

            Function::Definition(func_data) => {
                return vec![
                    BC::PushFn(func_data.label as Addr),
                ];
            }

            _ => panic!()
        }
    }
}


impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self {
            Function::Call(data) => {
                write!(f, "({}", data.name)?;
                for arg in &data.args {
                    write!(f, " {}", arg.borrow())?;
                }
                write!(f, ")")
            },

            Function::Arg(data) => write!(f, "{}", data.name),
            Function::Unknown(name) => write!(f, "{}", name),

            Function::Definition(data) => {
                write!(f, "(def")?;
                for (args, body) in &data.defs {
                    write!(f, "\n  ({}", data.name)?;
                    for a in args {
                        write!(f, " {}", a.borrow())?;
                    }
                    write!(f, ") {}", body.borrow())?;
                }
                write!(f, ")")
            }

            Function::Literal(data) => write!(f, "{}", data.value),

            _ => write!(f, "{:?}", self)
        }
    }
}


impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Bool(v)      => write!(f, "{}", if *v { "True" } else { "False" }),
            Value::Int(i)       => write!(f, "{}", i),
            Value::Float(i)     => write!(f, "{}", i),
            Value::Str(s)       => write!(f, "{:?}", s),
            Value::Deferred(s)  => write!(f, "'{:?}", s),
            Value::List(list) => {
                write!(f, "[")?;
                for (i, el) in list.iter().enumerate() {
                    if i != 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", el)?;
                }
                write!(f, "]")
            },
            _ => write!(f, "'{:?}", self)
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
            BC::Call(index)     => *instr = BC::Call(map_label_addr[index]),
            BC::PushFn(index)   => *instr = BC::PushFn(map_label_addr[index]),
            _ => ()
        }
    }
}


pub fn compile(mut ast: Vec<Function>) -> (Vec<BC>, Vec<Value>) {
    let mut out: Vec<BC> = Vec::new();

    let mut ctx = Context::new();

    for el in ast.drain(..) {
        out.extend(Function::compile(&mut ctx, el.to_ref()));
    }
    println!("\n{:?}\n", out);

    link(&mut out);

    return (out, ctx.data)
}
