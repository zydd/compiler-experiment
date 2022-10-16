use std::collections::HashMap;

use crate::bytecode::*;
use crate::bytecode::runtime::Runtime;

pub(crate) mod annotate;
pub(crate) mod codegen;
pub(crate) mod display;

pub use codegen::compile;

type Label = Addr;
type RcRc<T> = std::rc::Rc<std::cell::RefCell<T>>;


#[derive(Debug)]
pub struct FunctionArg {
    name: String,
    addr: Argc,
    deferred: bool,
    strict: bool,
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


#[derive(Clone, Debug)]
pub struct FunctionUkn {
    pub name: String,
    pub deferred: bool,
    pub strict: bool,
}


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
    Unknown(FunctionUkn),
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
    pub fn new(addr: Argc, ukn: FunctionUkn) -> FunctionArg {
        return FunctionArg {
            name: ukn.name,
            addr: addr,
            deferred: ukn.deferred,
            strict: ukn.strict,
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


impl FunctionCall {
    pub fn new(expr: Vec<Function>) -> Function {
        assert_eq!(expr.len() > 0, true);
        assert!(matches!(expr[0], Function::Unknown(_)), "expected identifier");

        if let Function::Unknown(ukn) = &expr[0] {
            return Function::Call(FunctionCall {
                name: ukn.name.clone(),
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
}


impl FunctionDefinition {
    pub fn new(mut expr: Vec<Function>) -> Function {
        assert!(expr.len() >= 3);
        assert_eq!(expr.len() % 2, 1); // def + (args, body) pairs
        assert!(matches!(&expr[0], Function::Unknown(ukn) if ukn.name == "def"));

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

        if expr.len() == 2 && expr[0].call().unwrap().borrow().args.iter().all(|x| matches!(x, Function::Unknown(_))) {
            let mut expr = expr.into_iter();
            let call = expr.next().unwrap();
            let body = expr.next().unwrap();

            func.args = call.call().unwrap().borrow().args.iter().enumerate().map(
                        |(i, x)| FunctionArg::new((i + 1) as Argc, x.unknown().unwrap().clone()).to_rcrc()
                    ).collect();
            func.body = body;

            return Function::Definition(func.to_rcrc())
        } else {
            func.args = (0..func.arity).map(|i| FunctionArg::new(i + 1, FunctionUkn::new(std::format!("arg{}", i))).to_rcrc()).collect();

            func.body = FunctionMatch::new(func.args.clone(), expr);

            return Function::Definition(func.to_rcrc())
        }
    }
}


impl FunctionLiteral {
    pub fn new(value: Value) -> Function {
        return Function::Literal(FunctionLiteral {
            data_label: 0,
            value: value,
        }.to_rcrc())
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
}


impl FunctionUkn {
    pub fn new(name: String) -> FunctionUkn {
        return FunctionUkn {
            name: name,
            deferred: false,
            strict: false,
        }
    }
}


impl Function {
    pub fn new(name: String) -> Function {
        Function::Unknown(FunctionUkn::new(name))
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

    pub fn unknown(&self) -> Option<&FunctionUkn> {
        match self {
            Function::Unknown(ukn) => Some(&ukn),
            _ => None
        }
    }

    pub fn unknown_mut(&mut self) -> Option<&mut FunctionUkn> {
        match self {
            Function::Unknown(ref mut ukn) => Some(ukn),
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
}
