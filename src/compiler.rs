use std::collections::HashMap;

use crate::bytecode::*;

type Label = Addr;
type FunctionRef = std::rc::Rc<std::cell::RefCell<Function>>;

// #[derive(Clone, Debug)]
pub struct FunctionArg {
    name: String,
    index: Argc,
    // deferred: bool,
    refs: Vec<FunctionRef>,
}

#[derive(Clone)]
pub struct FunctionBuiltin {
    name: String,
    arity: Argc,
    opcode: BuiltinFunction,
}

// #[derive(Clone, Debug)]
pub struct FunctionCall {
    name: String,
    argc: Argc,
    function: Option<FunctionRef>,
    args: Vec<FunctionRef>,
    tail_call: Option<Argc>,
    recursive: bool,
    pub deferred: bool,
}

// #[derive(Clone, Debug)]
pub struct FunctionDefinition {
    name: String,
    arity: Argc,
    label: Label,
    args: Vec<FunctionRef>,
    body: FunctionRef,
    // use_count: usize,
    locals: Vec<FunctionRef>,
}

#[derive(Clone)]
pub struct FunctionLiteral {
    data_label: Addr,
    pub value: Value,
}

// #[derive(Clone, Debug)]
pub struct FunctionMatch {
    args: Vec<FunctionRef>,
    cases: Vec<(Vec<FunctionRef>, FunctionRef)>,
    tail_call: Option<Argc>,
}

// #[derive(Debug)]
pub enum Function {
    // TODO: test Arg(std::rc::Rc<std::cell::RefCell<FunctionArg>>),
    Arg(FunctionArg),
    ArgRef(FunctionRef),
    Builtin(FunctionBuiltin),
    Call(FunctionCall),
    Definition(FunctionDefinition),
    FunctionRef(FunctionRef),
    Literal(FunctionLiteral),
    Local(FunctionArg),
    Match(FunctionMatch),
    Unknown(String),
}


pub struct Context {
    label_index: Label,
    data: Vec<Value>,
    scope: Vec<HashMap<String, FunctionRef>>,
    args: Vec<Vec<FunctionRef>>,
    locals: Vec<Vec<FunctionRef>>,
    used_definitions: Vec<FunctionRef>,
}


impl Context {
    pub fn new() -> Context {
        let mut new = Context {
            label_index: 1,
            data: Vec::new(),
            scope: vec![HashMap::new()],
            args: vec![Vec::new()],
            locals: vec![Vec::new()],
            used_definitions: Vec::new(),
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
        return Function::Arg(FunctionArg {
            name: name,
            index: index,
            // deferred: false,
            refs: Vec::new(),
        })
    }
}

impl FunctionBuiltin {
    pub fn new(id: BuiltinFunction, arity: Argc, name: String) -> Function {
        return Function::Builtin(FunctionBuiltin {
            opcode: id,
            name: name,
            arity: arity,
        })
    }
}

impl FunctionDefinition {
    pub fn new(mut expr: Vec<Function>) -> Function {
        assert!(expr.len() >= 3);
        assert_eq!(expr.len() % 2, 1); // def + (args, body) pairs
        assert!(matches!(&expr[0], Function::Unknown(name) if name == "def"));

        let call = expr[1].call().unwrap();
        let mut func = FunctionDefinition {
            name: call.name.clone(),
            arity: call.args.len() as Argc,
            label: 0,
            args: Vec::new(),
            body: Function::new("undefined".to_string()).to_ref(),
            // use_count: 0,
            locals: Vec::new(),
        };

        assert!(expr.iter().skip(1).step_by(2).all(|x| x.call().unwrap().name == func.name));
        expr.remove(0);

        if expr.len() == 2 {
            if expr[0].call().unwrap().args.iter()
                    .all(|x| matches!(*x.borrow(), Function::Unknown(_))) {

                let mut expr = expr.into_iter();
                let call = expr.next().unwrap();
                let body = expr.next().unwrap();

                func.args = call.call().unwrap().args.clone();
                func.body = body.to_ref();

                return Function::Definition(func)
            }
        }

        func.args = (0..func.arity).map(|i| FunctionArg::new(i, std::format!("arg{}", i)).to_ref()).collect();

        let mut arg_refs = Vec::new();
        for arg in &mut func.args {
            let arg_ref = Function::ArgRef(arg.clone()).to_ref();
            arg.borrow_mut().arg_mut().unwrap().refs.push(arg_ref.clone());
            arg_refs.push(arg_ref);
        }
        func.body = FunctionMatch::new(arg_refs, expr).to_ref();

        return Function::Definition(func)
    }

    fn annotate(&mut self, ctx: &mut Context) {
        self.label = ctx.new_label();
        ctx.args.push(self.args.clone());
        ctx.locals.push(Vec::new());
        ctx.enter();

        for (i, arg_ref) in self.args.iter().enumerate() {
            let mut arg = arg_ref.borrow_mut();
            match &*arg {
                Function::Unknown(name) => {
                    let name = name.clone();
                    *arg = FunctionArg::new(i as Argc, name.clone());
                    ctx.set(name, arg_ref.clone());
                }

                Function::Arg(arg) => {
                    ctx.set(arg.name.clone(), arg_ref.clone());
                }

                _ => panic!("{}", arg)
            }
        }

        Function::flag_tail_call(&mut self.body.borrow_mut(), Some(self.arity));

        Function::annotate(ctx, self.body.clone());

        ctx.exit();
        self.locals.extend(ctx.locals.pop().unwrap());
    }

    fn inline(self, argv: &Vec<FunctionRef>) -> Function {
        assert_eq!(argv.len(), self.args.len());

        for (arg, argv) in self.args.iter().zip(argv) {
            match &*arg.borrow() {
                Function::Arg(data) => {
                    for arg_ref in &data.refs {
                        let mut arg_use = arg_ref.borrow_mut();
                        *arg_use = argv.borrow().clone();

                        if let Function::ArgRef(parent_fn_arg) = &mut *arg_use {
                            let mut parent_fn_arg = parent_fn_arg.borrow_mut();
                            parent_fn_arg.arg_mut().unwrap().refs.push(arg_ref.clone());
                        }
                    }
                }

                _ => ()
            }
        }

        return self.body.borrow().clone()
    }

    fn compile(&self, ctx: &mut Context) -> Vec<BC> {
        let mut out = Vec::new();

        out.extend([
            BC::Label(self.label),
        ]);

        for local in &self.locals {
            out.extend(Function::compile(ctx, local.clone()));
        }

        out.extend(Function::compile(ctx, self.body.clone()));

        if self.body.borrow().is_value() {
            out.extend([
                BC::Return(self.args.len() as Argc),
            ]);
        }

        return out
    }
}

impl FunctionCall {
    pub fn new(expr: Vec<Function>) -> Function {
        assert_eq!(expr.len() > 0, true);
        assert_eq!(matches!(expr[0], Function::Unknown(_)), true);

        if let Function::Unknown(name) = &expr[0] {
            return Function::Call(FunctionCall {
                name: name.clone(),
                argc: (expr.len() - 1) as Argc,
                function: None,
                args: expr.into_iter().skip(1).map(|x| x.to_ref()).collect(),
                tail_call: None,
                recursive: false,
                deferred: false,
            })
        } else {
            panic!()
        }
    }

    fn inline(&mut self, ctx: &mut Context) -> Option<Function> {
        let function_ref = self.function.as_ref().unwrap();
        let callee = match &*function_ref.borrow() {
            Function::Definition(_)     => function_ref.clone(),
            Function::FunctionRef(func) => func.clone(),
            Function::Builtin(_)        => return None,
            Function::ArgRef(_)         => return None,
            _ => panic!("{}", self.name)
        };

        let callee = callee.borrow();

        if callee.definition().is_some() {
            let callee = callee.definition().unwrap();

            for (argi, arg) in self.args.iter_mut().enumerate() {
                if let Function::ArgRef(arg_ref) = &*arg.borrow() {
                    let mut arg_ref = arg_ref.borrow_mut();
                    let arg_ref = arg_ref.arg_mut().unwrap();
                    arg_ref.refs.retain(|x| !FunctionRef::ptr_eq(x, arg));
                }
                // if !arg.borrow().is_value() {
                //     let callee_arg = &callee.args[argi].borrow();
                //     let callee_arg = callee_arg.arg().unwrap();
                //     let use_count = callee_arg.refs.len();

                //     // if an arg is not a value (e.g. function call), add it to a
                //     // local variable instead of instantiating multiple times
                //     if use_count > 1 {
                //         ctx.locals.last_mut().unwrap().push(arg.clone());
                //         *arg = Function::Local(FunctionArg {
                //             name: callee_arg.name.clone(),
                //             index: -(ctx.locals.last_mut().unwrap().len() as Argc),
                //             refs: Vec::new(),
                //         }).to_ref();
                //     }
                // }
            }

            let mut inlined = callee.clone().inline(&self.args);
            Function::flag_tail_call(&mut inlined, self.tail_call);
            return Some(inlined)
        } else {
            todo!();
        }

        return None
    }

    fn compile(&self, ctx: &mut Context) -> Vec<BC> {
        let mut out = Vec::new();

        let func = self.function.as_ref().unwrap();

        for arg in self.args.iter().rev() {
            out.extend(Function::compile(ctx, arg.clone()));
        }

        if self.deferred {
            out.extend(func.borrow().compile_as_value());
            out.extend([
                BC::Defer(self.args.len() as Argc + 1)
            ]);
        } else {
            match &*func.borrow() {
                Function::ArgRef(arg) => {
                    out.extend([
                        BC::Arg(arg.borrow().arg().unwrap().index),
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
                    if let Some(move_args) = self.tail_call {
                        out.extend([
                            BC::MoveArgs(move_args),
                        ]);
                    }
                    out.extend([
                        BC::Builtin(func_data.opcode.clone()),
                    ]);
                    if self.tail_call.is_some() {
                        out.extend([
                            BC::Return(0),
                        ]);
                    }
                }

                Function::Definition(func_data) => {
                    if !ctx.used_definitions.iter().any(|x| FunctionRef::ptr_eq(x, func)) {
                        ctx.used_definitions.push(func.clone());
                    }
                    let excess_args = self.args.len() > func_data.arity as usize;

                    if !excess_args && self.tail_call.is_some() {
                        out.extend([
                            BC::MoveArgs(self.tail_call.unwrap()),
                        ]);
                    }

                    if !excess_args && self.tail_call.is_some() {
                        out.extend([
                            BC::Jump(func_data.label),
                        ]);
                    } else {  // excess_args || !tail_call
                        out.extend([
                            BC::Call(func_data.label),
                        ]);
                    }

                    if excess_args {
                        out.extend([
                            BC::ReturnCall(self.args.len() as Argc - func_data.arity),
                        ]);

                        if self.tail_call.is_some() {
                            out.extend([
                                BC::Return(0),
                            ]);
                        }
                    }
                }

                Function::FunctionRef(func_ref) => {
                    if !ctx.used_definitions.iter().any(|x| FunctionRef::ptr_eq(x, func_ref)) {
                        ctx.used_definitions.push(func_ref.clone());
                    }

                    if let Some(move_args) = self.tail_call {
                        out.extend([
                            BC::MoveArgs(move_args),
                        ]);
                    }

                    out.extend(func_ref.borrow_mut().compile_as_value());

                    out.extend([
                        BC::Builtin(BuiltinFunction::Invoke),
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

impl FunctionLiteral {
    pub fn new(value: Value) -> Function {
        return Function::Literal(FunctionLiteral {
            data_label: 0,
            value: value,
        })
    }

    fn annotate(&mut self, ctx: &mut Context) {
        self.data_label = ctx.data.len() as Label;
        ctx.data.push(self.value.clone());
    }
}

impl FunctionMatch {
    pub fn new(args: Vec<FunctionRef>, defs: Vec<Function>) -> Function {
        let mut func = FunctionMatch {
            args: args,
            cases: Vec::new(),
            tail_call: None,
        };

        let mut defs = defs.into_iter();
        while let (Some(pat), Some(body)) = (defs.next(), defs.next()) {
            let pattern = pat.call().unwrap().args.clone();
            assert_eq!(func.args.len(), pattern.len());

            func.cases.push((pattern, body.to_ref()));
        }

        return Function::Match(func)
    }

    fn compile(&self, ctx: &mut Context) -> Vec<BC> {
        let mut out = Vec::new();
        let match_end = ctx.new_label();

        for (pattern, body) in &self.cases {
            let case_end = ctx.new_label();

            for (pat, arg) in pattern.iter().zip(&self.args) {
                match &*pat.borrow() {
                    Function::Unknown(_) => (),

                    Function::Literal(_) => {
                        out.extend(Function::compile(ctx, arg.clone()));
                        out.extend(pat.borrow().compile_as_value());
                        out.extend([
                            BC::Bne(case_end),
                        ]);
                    }

                    _ => todo!()
                }
            }

            out.extend(Function::compile(ctx, body.clone()));

            if body.borrow().is_value() && self.tail_call.is_some() {
                let move_args = self.tail_call.unwrap();
                out.extend([
                    BC::Return(move_args),
                ]);
            } else {
                out.extend([
                    BC::Jump(match_end),
                ]);
            }

            out.extend([
                BC::Label(case_end),
            ]);
            ctx.exit();
        }

        out.extend([
            BC::Builtin(BuiltinFunction::Except),
            BC::Label(match_end),
        ]);

        return out
    }
}


impl Function {
    pub fn new(name: String) -> Function {
        return Function::Unknown(name)
    }

    pub fn arg(&self) -> Option<&FunctionArg> {
        match self {
            Function::Arg(data) => Some(data),
            _ => None
        }
    }

    pub fn arg_mut(&mut self) -> Option<&mut FunctionArg> {
        match self {
            Function::Arg(data) => Some(data),
            _ => None
        }
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

    pub fn definition(&self) -> Option<&FunctionDefinition> {
        match self {
            Function::Definition(data) => Some(data),
            _ => None
        }
    }

    // pub fn definition_mut(&mut self) -> Option<&mut FunctionDefinition> {
    //     match self {
    //         Function::Definition(data) => Some(data),
    //         _ => None
    //     }
    // }

    pub fn is_value(&self) -> bool {
        match self {
            Function::Arg(_)        => true,
            Function::ArgRef(_)     => true,
            Function::Builtin(_)    => true,
            Function::FunctionRef(_)=> true,
            Function::Literal(_)    => true,
            Function::Local(_)      => true,

            Function::Call(_)       => false,
            Function::Definition(_) => panic!(),
            Function::Match(_)      => false,

            Function::Unknown(_)    => panic!(),
        }
    }

    pub fn is_executable(&self) -> bool {
        match self {
            Function::Call(_)   => true,
            Function::Match(_)  => true,

            _ => false,
        }
    }

    fn flag_tail_call(func: &mut Function, tail_call: Option<Argc>) {
        match func {
            Function::Call(call) => call.tail_call = tail_call,
            Function::Match(margs) => margs.tail_call = tail_call,
            _ => ()
        }
    }

    fn to_ref(self) -> FunctionRef {
        std::rc::Rc::new(std::cell::RefCell::new(self))
    }

    fn annotate(ctx: &mut Context, function: FunctionRef) {
        loop {
            let mut function_mut = &mut *function.borrow_mut();
            match &mut function_mut {
                Function::Arg(arg) => {
                    assert!(arg.index >= 0);
                    let arg_ref = ctx.args.last().unwrap()[arg.index as usize].clone();
                    arg_ref.borrow_mut().arg_mut().unwrap().refs.push(function.clone());
                    *function_mut = Function::ArgRef(arg_ref);
                }

                Function::ArgRef(_)     => (),
                Function::Builtin(_)    => (),
                Function::Local(_)      => (),

                Function::Call(call) => {
                    for arg in &call.args {
                        Function::annotate(ctx, arg.clone());
                    }

                    if call.function.is_none() {
                        let mut func_ref = ctx.get(&call.name).expect(&call.name);

                        if func_ref.try_borrow().is_err() {
                            call.recursive = true;
                        } else if func_ref.borrow().arg().is_some() {
                            let arg_ref = Function::ArgRef(func_ref.clone()).to_ref();

                            if let Function::Arg(arg_data) = &mut *func_ref.borrow_mut() {
                                arg_data.refs.push(arg_ref.clone());
                            }

                            func_ref = arg_ref;
                        }

                        call.function = Some(func_ref.clone());
                    } else if call.function.as_ref().unwrap().borrow().arg().is_some() {
                        Function::annotate(ctx, call.function.as_ref().unwrap().clone());
                    }

                    if !call.recursive {
                        if let Some(inlined) = call.inline(ctx) {
                            *function_mut = inlined;
                            continue;
                        }
                    }
                }

                Function::Definition(fndef) => {
                    ctx.set(fndef.name.clone(), function.clone());
                    fndef.annotate(ctx);
                }

                Function::Match(fnmatch) => {
                    for (pattern, body) in &fnmatch.cases {
                        ctx.enter();

                        for arg in &mut fnmatch.args {
                            Function::annotate(ctx, arg.clone());
                        }

                        for (i, arg) in pattern.iter().enumerate() {
                            match &mut *arg.borrow_mut() {
                                Function::Unknown(name) => if name != "_" {
                                    ctx.set(name.clone(), fnmatch.args[i].clone());
                                }

                                Function::Literal(literal) => literal.annotate(ctx),

                                _ => todo!()
                            }
                        }

                        Function::flag_tail_call(&mut body.borrow_mut(), fnmatch.tail_call);

                        Function::annotate(ctx, body.clone());
                        ctx.exit();
                    }
                }

                Function::Literal(literal)  => literal.annotate(ctx),

                Function::Unknown(name) => {
                    let func_ref = ctx.get(&name).unwrap();

                    // if func_ref is already borrowed, assume it's a function definition
                    // referencing itself
                    if func_ref.try_borrow().is_err() {
                        *function_mut = Function::FunctionRef(func_ref.clone());
                        return
                    }

                    let mut func = &mut *func_ref.borrow_mut();
                    match &mut func {
                        Function::Arg(arg_data) => {
                            *function_mut = Function::ArgRef(func_ref.clone());
                            arg_data.refs.push(function.clone());
                        },

                        Function::ArgRef(arg_ref) => {
                            *function_mut = Function::ArgRef(arg_ref.clone());
                            arg_ref.borrow_mut().arg_mut().unwrap().refs.push(function.clone());
                        },

                        Function::Definition(_) => {
                            *function_mut = Function::FunctionRef(func_ref.clone());
                        }

                        Function::Builtin(func) => {
                            *function_mut = Function::Builtin(func.clone());
                        }

                        _ => panic!("Unkn: {} {}", name, func),
                    }
                }

                other => todo!("{}", other)
            }

            break;
        }
    }


    fn compile(ctx: &mut Context, function: FunctionRef) -> Vec<BC> {
        let mut out = Vec::new();

        let function_ref = &*function.borrow();
        match function_ref {
            Function::ArgRef(arg)       => out.push(BC::Arg(arg.borrow().arg().unwrap().index)),
            // Function::Arg(arg)          => out.push(BC::Arg(arg.index)),
            Function::Call(call)        => out.extend(call.compile(ctx)),
            Function::Builtin(_)        => out.extend(function_ref.compile_as_value()),
            Function::Definition(fndef) => out.extend(fndef.compile(ctx)),
            Function::Literal(_)        => out.extend(function_ref.compile_as_value()),
            Function::Match(fnmatch)    => out.extend(fnmatch.compile(ctx)),
            _ => panic!("{}", function_ref),
        }

        return out
    }

    fn compile_as_value(&self) -> Vec<BC> {
        match self {
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

            Function::Literal(literal) => {
                if let Value::Int(i) = literal.value {
                    return vec![
                        BC::PushInt(i as i32),
                    ];
                } else {
                    return vec![
                        BC::Load(literal.data_label),
                    ];
                }
            }

            _ => panic!()
        }
    }
}


impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        fn fmt_fn_name(f: &mut std::fmt::Formatter, func: &Function) -> std::fmt::Result {
            match func {
                Function::Definition(func_data) =>
                    write!(f, "{}[id:{}]", func_data.name, func_data.label)?,
                Function::Builtin(func_data) =>
                    write!(f, "#{}[op:{}]", func_data.name, func_data.opcode.clone() as usize)?,
                other =>
                    write!(f, "{}", other)?,
            }
            return std::fmt::Result::Ok(())
        }

        match &self {
            Function::Arg(data) => write!(f, "${}[use:{}]", data.name, data.refs.len()),
            Function::ArgRef(data) => write!(f, "&{}", data.borrow().arg().unwrap().name),
            Function::Builtin(_) => fmt_fn_name(f, self),

            Function::Call(data) => {
                write!(f, "(")?;

                if data.function.is_some() {
                    fmt_fn_name(f, &*data.function.as_ref().unwrap().borrow())?;
                } else {
                    write!(f, "{}[?]", data.name)?;
                }

                if data.recursive {
                    write!(f, "[r]")?;
                }

                if data.tail_call.is_some() {
                    write!(f, "[t]")?;
                }

                for arg in &data.args {
                    write!(f, " {}", arg.borrow())?;
                }
                write!(f, ")")
            }

            Function::Definition(data) => {
                write!(f, "(def")?;

                if !data.locals.is_empty() {
                    write!(f, " [local:")?;
                    for a in &data.locals {
                        write!(f, " {}", a.borrow())?;
                    }
                    write!(f, "]")?;
                }

                write!(f, "\n  (")?;
                fmt_fn_name(f, self)?;
                for a in &data.args {
                    write!(f, " {}", a.borrow())?;
                }
                write!(f, ") {}", data.body.borrow())?;

                write!(f, ")")
            }

            Function::FunctionRef(data) => {
                write!(f, "#")?;
                fmt_fn_name(f, &*data.borrow())
            }

            Function::Literal(data) => write!(f, "{}", data.value),
            Function::Local(data) => write!(f, "$[loc:{}]", data.index),

            Function::Match(data) => {
                write!(f, "\n match")?;

                if data.tail_call.is_some() {
                    write!(f, "[t]")?;
                }

                for a in &data.args {
                    write!(f, " {}", a.borrow())?;
                }
                for (pat, body) in &data.cases {
                    write!(f, "\n")?;
                    for p in pat {
                        write!(f, "  {}", p.borrow())?;
                    }
                    write!(f, " => {}", body.borrow())?;
                }
                write!(f, ")")
            }

            Function::Unknown(name) => write!(f, "{{{}}}", name),

            // _ => write!(f, "{:?}", self)
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
            Value::List(list)   => {
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


impl Clone for FunctionArg {
    fn clone(&self) -> Self {
        return FunctionArg {
            index: self.index.clone(),
            name: self.name.clone(),
            refs: Vec::new(),
        }
    }
}

impl Clone for Function {
    fn clone(&self) -> Self {
        match self {
            Function::ArgRef(data)      => Function::Arg(data.borrow().arg().unwrap().clone()),
            Function::Arg(data)         => Function::Arg(data.clone()),
            Function::Call(data)        => Function::Call(data.clone()),
            Function::Builtin(data)     => Function::Builtin(data.clone()),
            Function::Definition(data)  => Function::Definition(data.clone()),
            Function::Literal(data)     => Function::Literal(data.clone()),
            Function::Match(data)       => Function::Match(data.clone()),
            Function::Unknown(data)     => Function::Unknown(data.clone()),
            Function::Local(data)       => Function::Local(data.clone()),
            Function::FunctionRef(data) => Function::FunctionRef(data.clone()),
        }
    }
}


impl Clone for FunctionCall {
    fn clone(&self) -> Self {
        let mut call = FunctionCall {
            name: self.name.clone(),
            argc: self.argc.clone(),
            function: self.function.clone(),
            args: self.args.iter().map(|x| x.borrow().clone().to_ref()).collect(),
            tail_call: self.tail_call.clone(),
            recursive: self.recursive.clone(),
            deferred: self.deferred.clone(),
        };

        if let Some(func) = &mut call.function {
            if matches!(&*func.borrow(), Function::ArgRef(_)) {
                let mut func_ref = func.borrow_mut();
                // if the callee is an ArgRef, convert it to Arg so it can be relinked by annotate
                *func_ref = func_ref.clone();
            }
        }

        return call
    }
}

impl Clone for FunctionDefinition {
    fn clone(&self) -> Self {
        let mut func = FunctionDefinition {
            name: self.name.clone(),
            arity: self.arity.clone(),
            label: 0,
            args: self.args.iter().map(|x| x.borrow().clone().to_ref()).collect(),
            body: self.body.borrow().clone().to_ref(),
            // use_count: 0,
            locals: self.locals.iter().map(|x| x.borrow().clone().to_ref()).collect(),
        };

        func.annotate(&mut Context::new());

        return func
    }
}

impl Clone for FunctionMatch {
    fn clone(&self) -> Self {
        return FunctionMatch {
            args: self.args.iter().map(|x| x.borrow().clone().to_ref()).collect(),
            cases: self.cases.iter().map(|(pat, body)| (
                pat.iter().map(|x| x.borrow().clone().to_ref()).collect(),
                body.borrow().clone().to_ref())
            ).collect(),
            tail_call: self.tail_call.clone(),
        };
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


pub fn compile(ast: Vec<Function>) -> (Vec<BC>, Vec<Value>) {
    let mut ast = ast.into_iter().map(|x| x.to_ref()).collect::<Vec<_>>();

    let mut ctx = Context::new();

    for el in ast.iter_mut() {
        Function::annotate(&mut ctx, el.clone());
        println!("{}\n", el.borrow());
    }

    for el in ast.iter_mut() {
        println!("{}\n", el.borrow());
    }

    let mut out: Vec<BC> = Vec::new();
    for el in ast.iter_mut() {
        if el.borrow().is_executable() {
            out.extend(Function::compile(&mut ctx, el.clone()));
        }
    }

    out.extend([
        BC::Return(0),
    ]);

    let mut def = 0;
    while def < ctx.used_definitions.len() {
        let func = ctx.used_definitions[def].clone();
        out.extend(Function::compile(&mut ctx, func));
        def += 1;
    }

    println!("\n{:?}\n", out);

    link(&mut out);

    return (out, ctx.data)
}
