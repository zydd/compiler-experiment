use std::collections::HashMap;

use crate::bytecode::*;
use crate::bytecode::runtime::Runtime;

type Label = Addr;

#[derive(Debug)]
pub struct FunctionArg {
    name: String,
    addr: Argc,
    // deferred: bool,
    refs: usize,
}

#[derive(Clone, Debug)]
pub struct FunctionBuiltin {
    name: String,
    // arity: Argc,
    addr: Addr,
}

#[derive(Debug)]
pub struct FunctionCall {
    name: String,
    // argc: Argc,
    function: Option<Function>,
    args: Vec<Function>,
    tail_call: Option<Argc>,
    recursive: bool,
    pub deferred: bool,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    name: String,
    arity: Argc,
    label: Label,
    args: Vec<RcRc<FunctionArg>>,
    body: Function,
    // use_count: usize,
    locals: Vec<Function>,
}

#[derive(Clone, Debug)]
pub struct FunctionLiteral {
    data_label: Addr,
    pub value: Value,
}

#[derive(Debug)]
pub struct FunctionMatch {
    args: Vec<RcRc<FunctionArg>>,
    cases: Vec<(Vec<Function>, Function)>,
    tail_call: Option<Argc>,
}


type RcRc<T> = std::rc::Rc<std::cell::RefCell<T>>;

#[derive(Clone, Debug)]
pub enum Function {
    Arg(RcRc<FunctionArg>),
    ArgRef(RcRc<FunctionArg>),
    Builtin(RcRc<FunctionBuiltin>),
    Call(RcRc<FunctionCall>),
    Definition(RcRc<FunctionDefinition>),
    FunctionRef(RcRc<FunctionDefinition>),
    Literal(RcRc<FunctionLiteral>),
    // Local(RcRc<FunctionArg>),
    Match(RcRc<FunctionMatch>),
    Unknown(String),
}

pub trait ToRcRc {
    fn to_rcrc(self) -> RcRc<Self>;
}

impl<T> ToRcRc for T {
    fn to_rcrc(self) -> RcRc<T> {
        std::rc::Rc::new(std::cell::RefCell::new(self))
    }
}


pub struct CompileStack {
    // func: Function,
    scope: HashMap<String, Function>
}

impl CompileStack {
    pub fn new(_func: Function) -> CompileStack {
        CompileStack {
            // func: func,
            scope: HashMap::new(),
        }
    }
}


pub struct Context {
    label_index: Label,
    data: Vec<Value>,
    stack: Vec<CompileStack>,
    used_definitions: Vec<RcRc<FunctionDefinition>>,
}


impl Context {
    fn new(runtime: &Runtime) -> Context {
        let mut new = Context {
            label_index: 1,
            data: Vec::new(),
            stack: vec![CompileStack::new(Function::new("main".to_string()))],
            used_definitions: Vec::new(),
        };

        for (name, info) in runtime.functioninfo.iter() {
            new.set(name.clone(), FunctionBuiltin::new(info.index as Addr, info.arity as Argc, name.clone()));
        }

        return new
    }

    fn new_label(&mut self) -> Label {
        let label = self.label_index;
        self.label_index += 1;
        return label
    }

    fn get(&self, key: &String) -> Option<Function> {
        for frame in self.stack.iter().rev() {
            if let Some(v) = frame.scope.get(key) {
                return Some(v.clone())
            }
        }

        return None
    }

    fn builtin_addr(&self, key: &String) -> Addr {
        return self.stack[0].scope[key].as_builtin().borrow().addr;
    }

    fn set(&mut self, name: String, function: Function) {
        let current = &mut self.stack.last_mut().unwrap().scope;
        current.insert(name, function.clone());
    }

    fn enter(&mut self, func: Function) {
        self.stack.push(CompileStack::new(func));
    }

    fn exit(&mut self) {
        assert!(self.stack.len() > 1);
        self.stack.pop();
    }
}


impl FunctionArg {
    pub fn new(addr: Argc, name: String) -> FunctionArg {
        return FunctionArg {
            name: name,
            addr: addr,
            // deferred: false,
            refs: 0,
        }
    }
}

impl FunctionBuiltin {
    pub fn new(addr: Addr, _arity: Argc, name: String) -> Function {
        return Function::Builtin(FunctionBuiltin {
            name: name,
            addr: addr,
            // arity: arity,
        }.to_rcrc())
    }
}

impl FunctionDefinition {
    pub fn new(mut expr: Vec<Function>) -> Function {
        assert!(expr.len() >= 3);
        assert_eq!(expr.len() % 2, 1); // def + (args, body) pairs
        assert!(matches!(&expr[0], Function::Unknown(name) if name == "def"));

        let mut func = FunctionDefinition {
            name: expr[1].call().unwrap().borrow().name.clone(),
            arity: expr[1].call().unwrap().borrow().args.len() as Argc,
            label: 0,
            args: Vec::new(),
            body: Function::new("undefined".to_string()),
            // use_count: 0,
            locals: Vec::new(),
        };

        assert!(expr.iter().skip(1).step_by(2).all(|x| x.call().unwrap().borrow().name == func.name));
        expr.remove(0);

        if expr.len() == 2 {
            if expr[0].call().unwrap().borrow().args.iter().all(|x| matches!(x, Function::Unknown(_))) {
                let mut expr = expr.into_iter();
                let call = expr.next().unwrap();
                let body = expr.next().unwrap();

                func.args = call.call().unwrap().borrow().args.iter().enumerate().map(
                            |(i, x)| FunctionArg::new((i + 1) as Argc, x.unknown_string().unwrap().clone()).to_rcrc()
                        ).collect();
                func.body = body;

                return Function::Definition(func.to_rcrc())
            }
        }

        func.args = (0..func.arity).map(|i| FunctionArg::new(i + 1, std::format!("arg{}", i)).to_rcrc()).collect();

        func.body = FunctionMatch::new(func.args.clone(), expr);

        return Function::Definition(func.to_rcrc())
    }

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

    fn compile(&self, ctx: &mut Context) -> Vec<BC> {
        let mut out = Vec::new();

        out.extend([
            BC::Label(self.label),
        ]);

        for local in &self.locals {
            out.extend(Function::compile(ctx, local.clone()));
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

impl FunctionCall {
    pub fn new(expr: Vec<Function>) -> Function {
        assert_eq!(expr.len() > 0, true);
        assert!(matches!(expr[0], Function::Unknown(_)), "expected identifier");

        if let Function::Unknown(name) = &expr[0] {
            return Function::Call(FunctionCall {
                name: name.clone(),
                // argc: (expr.len() - 1) as Argc,
                function: None,
                args: expr.into_iter().skip(1).collect(),
                tail_call: None,
                recursive: false,
                deferred: false,
            }.to_rcrc())
        } else {
            panic!()
        }
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

impl FunctionLiteral {
    pub fn new(value: Value) -> Function {
        return Function::Literal(FunctionLiteral {
            data_label: 0,
            value: value,
        }.to_rcrc())
    }

    fn annotate(ctx: &mut Context, function: &mut Function) {
        let mut lit = function.literal().unwrap().borrow_mut();
        lit.data_label = ctx.data.len() as Label;
        ctx.data.push(lit.value.clone());
    }
}

impl FunctionMatch {
    pub fn new(args: Vec<RcRc<FunctionArg>>, defs: Vec<Function>) -> Function {
        let mut func = FunctionMatch {
            args: args,
            cases: Vec::new(),
            tail_call: None,
        };

        let mut defs = defs.into_iter();
        while let (Some(pat), Some(body)) = (defs.next(), defs.next()) {
            let pattern = pat.call().unwrap().borrow().args.clone();
            assert_eq!(func.args.len(), pattern.len());

            func.cases.push((pattern, body));
        }

        return Function::Match(func.to_rcrc())
    }

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
                    Function::Unknown(name) => if name != "_" {
                        ctx.set(name.clone(), Function::Arg(fnmatch.args[i].clone()));
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

    fn compile(&self, ctx: &mut Context) -> Vec<BC> {
        let mut out = Vec::new();
        let match_end = ctx.new_label();

        for (pattern, body) in &self.cases {
            let case_end = ctx.new_label();

            for (pat, arg) in pattern.iter().zip(&self.args) {
                match pat {
                    Function::Unknown(_) => (),

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
        }

        out.extend([
            BC::Builtin(ctx.builtin_addr(&"__builtin_except".to_string())),
            BC::Label(match_end),
        ]);

        return out
    }
}


impl Function {
    pub fn new(name: String) -> Function {
        return Function::Unknown(name)
    }

    pub fn arg(&self) -> Option<&RcRc<FunctionArg>> {
        match self {
            Function::Arg(fnref) => Some(fnref),
            _ => None
        }
    }

    pub fn as_builtin(&self) -> RcRc<FunctionBuiltin> {
        match self {
            Function::Builtin(builtin) => builtin.clone(),
            _ => panic!()
        }
    }

    pub fn call(&self) -> Option<&RcRc<FunctionCall>> {
        match self {
            Function::Call(fnref) => Some(fnref),
            _ => None
        }
    }

    pub fn literal(&self) -> Option<&RcRc<FunctionLiteral>> {
        match self {
            Function::Literal(fnref) => Some(fnref),
            _ => None
        }
    }

    pub fn definition(&self) -> Option<&RcRc<FunctionDefinition>> {
        match self {
            Function::Definition(fnref) => Some(fnref),
            _ => None
        }
    }

    pub fn fnmatch(&self) -> Option<&RcRc<FunctionMatch>> {
        match self {
            Function::Match(fnmatch) => Some(fnmatch),
            _ => None
        }
    }

    pub fn unknown_string(&self) -> Option<&String> {
        match self {
            Function::Unknown(name) => Some(name),
            _ => None
        }
    }

    pub fn is_value(&self) -> bool {
        match self {
            Function::Arg(_)        => true,
            Function::ArgRef(_)     => true,
            Function::Builtin(_)    => true,
            Function::FunctionRef(_) => true,
            Function::Literal(_)    => true,
            // Function::Local(_)      => true,

            Function::Call(call)    => call.borrow().deferred,
            Function::Definition(_) => panic!(),
            Function::Match(_)      => false,

            Function::Unknown(_)    => panic!(),
        }
    }

    // pub fn is_executable(&self) -> bool {
    //     match self {
    //         Function::Call(_)   => true,
    //         Function::Match(_)  => true,

    //         _ => false,
    //     }
    // }

    fn flag_tail_call(func: &Function, tail_call: Option<Argc>) {
        match func {
            Function::Call(call) => call.borrow_mut().tail_call = tail_call,
            Function::Match(margs) => margs.borrow_mut().tail_call = tail_call,
            _ => ()
        }
    }

    fn annotate(ctx: &mut Context, function: &mut Function) {
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

            Function::Unknown(name) => {
                let func_ref = ctx.get(&name).expect(name);

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

                    _ => panic!("Unkn: {} {} {:?}", name, function, func_ref),
                }
            }
        }

    }

    fn compile(ctx: &mut Context, function: Function) -> Vec<BC> {
        let mut out = Vec::new();

        match &function {
            Function::ArgRef(arg)       => out.push(BC::Arg(arg.borrow().addr)),
            Function::Call(call)        => out.extend(call.borrow().compile(ctx)),
            Function::Builtin(_)        => out.extend(function.compile_as_value()),
            Function::Definition(fndef) => out.extend(fndef.borrow().compile(ctx)),
            Function::Literal(_)        => out.extend(function.compile_as_value()),
            Function::Match(fnmatch)    => out.extend(fnmatch.borrow().compile(ctx)),
            // Function::Arg(arg)          => out.push(BC::Arg(arg.index)),
            Function::Arg(_)            => panic!(),
            Function::FunctionRef(_)    => out.extend(function.compile_as_value()),
            // Function::Local(_)          => panic!(),
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

            Function::Call(_) | Function::Match(_) | Function::Unknown(_) => panic!(),
        }
    }
}


impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        fn fmt_fn_name(f: &mut std::fmt::Formatter, func: &Function) -> std::fmt::Result {
            match func {
                Function::Definition(func_data) =>
                    write!(f, "{}[fn:{}]", func_data.borrow().name, func_data.borrow().label)?,
                Function::Builtin(func_data) =>
                    write!(f, "#{}[op:{}]", func_data.borrow().name, func_data.borrow().addr.clone() as usize)?,
                other =>
                    write!(f, "{}", other)?,
            }
            return std::fmt::Result::Ok(())
        }

        match &self {
            Function::Arg(data) => write!(f, "${}[use:{}]", data.borrow().name, data.borrow().refs),
            Function::ArgRef(data) => write!(f, "&{}", data.borrow().name),
            Function::Builtin(_) => fmt_fn_name(f, self),

            Function::Call(data) => {
                let data = data.borrow();
                write!(f, "(")?;

                if data.function.is_some() {
                    fmt_fn_name(f, data.function.as_ref().unwrap())?;
                } else {
                    write!(f, "{}[?]", data.name)?;
                }

                if data.deferred {
                    write!(f, "[d]")?;
                }

                if data.recursive {
                    write!(f, "[r]")?;
                }

                if data.tail_call.is_some() {
                    write!(f, "[t]")?;
                }

                for arg in &data.args {
                    write!(f, " {}", arg)?;
                }
                write!(f, ")")
            }

            Function::Definition(data) => {
                let data = data.borrow();
                write!(f, "(def")?;

                if !data.locals.is_empty() {
                    write!(f, " [local:")?;
                    for a in &data.locals {
                        write!(f, " {}", a)?;
                    }
                    write!(f, "]")?;
                }

                write!(f, "\n  (")?;
                fmt_fn_name(f, self)?;
                for a in &data.args {
                    write!(f, " {}", Function::Arg(a.clone()))?;
                }
                write!(f, ") {}", data.body)?;

                write!(f, ")")
            }

            Function::FunctionRef(data) => {
                write!(f, "#")?;
                fmt_fn_name(f, &Function::Definition(data.clone()))
            }

            Function::Literal(data) => write!(f, "{}", data.borrow().value),
            // Function::Local(data) => write!(f, "$[loc:{}]", data.borrow().index),

            Function::Match(data) => {
                let data = data.borrow();
                write!(f, "\n match")?;

                if data.tail_call.is_some() {
                    write!(f, "[t]")?;
                }

                for a in &data.args {
                    write!(f, " {}", Function::Arg(a.clone()))?;
                }
                for (pat, body) in &data.cases {
                    write!(f, "\n")?;
                    for p in pat {
                        write!(f, "  {}", p)?;
                    }
                    write!(f, " => {}", body)?;
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
            Value::Str(s)       => write!(f, "{}", s),
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
            _ => write!(f, "{:?}", self)
        }
    }
}


// impl Clone for FunctionArg {
//     fn clone(&self) -> Self {
//         return FunctionArg {
//             index: self.index.clone(),
//             name: self.name.clone(),
//             refs: Vec::new(),
//         }
//     }
// }

// impl Clone for Function {
//     fn clone(&self) -> Self {
//         match self {
//             Function::ArgRef(data)      => Function::Arg(data.clone()),
//             Function::Arg(data)         => Function::Arg(data.clone()),
//             Function::Call(data)        => Function::Call(data.clone()),
//             Function::Builtin(data)     => Function::Builtin(data.clone()),
//             Function::Definition(data)  => Function::Definition(data.clone()),
//             Function::Literal(data)     => Function::Literal(data.clone()),
//             Function::Match(data)       => Function::Match(data.clone()),
//             Function::Unknown(data)     => Function::Unknown(data.clone()),
//             Function::Local(data)       => Function::Local(data.clone()),
//             Function::Function(data)    => Function::Function(data.clone()),
//         }
//     }
// }


// impl Clone for FunctionCall {
//     fn clone(&self) -> Self {
//         let mut call = FunctionCall {
//             name: self.name.clone(),
//             argc: self.argc.clone(),
//             function: self.function.clone(),
//             args: self.args.iter().map(|x| x.borrow().clone().to_ref()).collect(),
//             tail_call: self.tail_call.clone(),
//             recursive: self.recursive.clone(),
//             deferred: self.deferred.clone(),
//         };

//         if let Some(func) = &mut call.function {
//             // assume it's a recursive call if already borrowed
//             if func.try_borrow().is_ok() && matches!(&*func.borrow(), Function::ArgRef(_)) {
//                 let mut func_ref = func.borrow_mut();
//                 // if the callee is an ArgRef, convert it to Arg so it can be relinked by annotate
//                 *func_ref = func_ref.clone();
//             }
//         }

//         return call
//     }
// }

// impl Clone for FunctionDefinition {
//     fn clone(&self) -> Self {
//         let mut func = FunctionDefinition {
//             name: self.name.clone(),
//             arity: self.arity.clone(),
//             label: 0,
//             args: self.args.iter().map(|x| x.borrow().clone().to_ref()).collect(),
//             body: self.body.borrow().clone().to_ref(),
//             // use_count: 0,
//             locals: self.locals.iter().map(|x| x.borrow().clone().to_ref()).collect(),
//         };

//         func.annotate(&mut Context::new());

//         return func
//     }
// }

// impl Clone for FunctionMatch {
//     fn clone(&self) -> Self {
//         return FunctionMatch {
//             args: self.args.iter().map(|x| x.borrow().clone().to_ref()).collect(),
//             cases: self.cases.iter().map(|(pat, body)| (
//                 pat.iter().map(|x| x.borrow().clone().to_ref()).collect(),
//                 body.borrow().clone().to_ref())
//             ).collect(),
//             tail_call: self.tail_call.clone(),
//         };
//     }
// }


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

    // println!("\n{:?}\n", out);

    link(&mut out);

    return (out, ctx.data)
}
