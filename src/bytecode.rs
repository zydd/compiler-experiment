use std::collections::HashMap;
use std::collections::VecDeque;


// type SString = smartstring::SmartString::<smartstring::Compact>;
type Addr = usize;
pub type List = VecDeque<Value>;


#[derive(Debug)]
struct Arch {
    ip: Addr,
    fp: Addr,
    prog: Vec<BC>,
    data: Vec<Value>,
    stack: Vec<Value>,
}


#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Int(isize),
    Float(f64),
    Str(String),
    List(List),
    Deferred(Vec<Value>),
    Function(Addr),
    Builtin(Addr),
}


#[allow(dead_code)]
#[derive(Clone, Debug)]
pub enum BC {
    Builtin(BuiltinFunction),
    AddA(Addr, Addr),
    Arg(Addr),
    CarA(Addr),
    CdrA(Addr),
    Call(Addr),
    Debug(usize),
    Defer(usize),
    Label(Addr),
    LtA(Addr, Addr),
    Jump(Addr),
    JumpZ(Addr),
    Pop(usize),
    Load(Addr),
    PushFn(Addr),
    PushIns(Addr),
    Return(usize),
    ReturnCall(usize),
}

use num_traits::FromPrimitive;
#[derive(Clone, Debug, num_derive::FromPrimitive)]
pub enum BuiltinFunction {
    Nop,
    Except,
    Invoke,
    Pop,
    Undefer,

    Add,
    Div,
    Mul,
    Sub,

    Eq,
    Geq,
    Gt,
    Leq,
    Lt,
    Neq,

    Car,
    Cdr,
}

macro_rules! enum_as {
    ($type:ty, $getter:ident, $enum:path) => {
        pub fn $getter(&self) -> Option<&$type> {
            if let $enum(n) = self {
                return Some(n)
            } else {
                return None
            }
        }
    };
}


impl Value {
    enum_as!(isize, int, Value::Int);
    enum_as!(VecDeque<Value>, list, Value::List);
    pub fn as_list_mut(self) -> List {
        if let Value::List(a) = self { a } else { panic!("not a list: {:?}", self) }
    }
    pub fn as_deferred(self) -> Vec<Value> {
        if let Value::Deferred(a) = self { a } else { panic!("not a deferred value: {:?}", self) }
    }
}

macro_rules! value_trait {
    ($trait: ident, $func: ident) => {
        impl std::ops::$trait<Value> for Value {
            type Output = Value;
            fn $func(self, rhs: Self) -> Self {
                use Value::*;
                match (&self, &rhs) {
                    (Int(a), Int(b)) => Int(a.$func(b)),
                    (Float(a), Float(b)) => Float(a.$func(b)),
                    _ => panic!("unsupported operation \"{}\" between {:?} and {:?}", stringify!($func), self, rhs),
                }
            }
        }
    };
}

value_trait!(Add, add);
value_trait!(Sub, sub);
value_trait!(Mul, mul);
value_trait!(Div, div);

impl std::cmp::PartialOrd for Value {
    fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> {
        use Value::*;
        match (self, rhs) {
            (Int(a), Int(b)) => a.partial_cmp(b),
            (Float(a), Float(b)) => a.partial_cmp(b),
            _ => None,
        }
    }
}

macro_rules! builtin {
    ($func_name:ident, $function:expr) => {
        fn $func_name(&mut self) {
            let a = self.pop_undefer();
            let b = self.pop_undefer();
            let ret = $function(a, b);
            self.stack.push(ret);
        }
    };
}

impl Arch {
    builtin!(add, |a, b| a + b);
    builtin!(div, |a, b| a / b);
    builtin!(mul, |a, b| a * b);
    builtin!(sub, |a, b| a - b);
    builtin!(lt,  |a, b| Value::Int((a <  b) as isize));
    builtin!(leq, |a, b| Value::Int((a <= b) as isize));
    builtin!(gt,  |a, b| Value::Int((a >  b) as isize));
    builtin!(geq, |a, b| Value::Int((a >= b) as isize));
    builtin!(eq,  |a, b| Value::Int((a == b) as isize));
    builtin!(neq, |a, b| Value::Int((a != b) as isize));

    fn new(prog: Vec<BC>, data: Vec<Value>) -> Arch {
        let new = Arch{
            ip: 0,
            fp: 0,
            prog: prog,
            data: data,
            stack: Vec::new(),
        };

        return new
    }

    fn is_false(&self, v: Value) -> bool {
        use Value::*;
        match v {
            Int(n)      => n == 0,
            List(list)  => list.is_empty(),
            _           => panic!("invalid test: {:?}", v)
        }
    }

    fn stdreturn(&mut self, argc: usize) {
        // assert_eq!(self.stack.len(), self.fp + 1);
        let ret = self.stack.pop().unwrap();

        self.stack.truncate(self.fp - argc);
        self.stack.push(ret);
    }

    fn returncall(&mut self, argc: usize) {
        let stack_len = self.stack.len() - argc;
        while self.stack.len() != stack_len {
            self.invoke();
        }
    }

    fn call(&mut self, addr: Addr) {
        let frame = self.stack.len();

        let ip = self.ip;
        let fp = self.fp;

        self.ip = addr;
        self.fp = frame;

        self.exec();

        self.ip = ip;
        self.fp = fp;
    }

    fn arg(&self, i: Addr) -> &Value {
        return &self.stack[self.fp-1 - i]
    }

    fn pop(&mut self, n: usize) {
        self.stack.truncate(self.stack.len() - n);
    }

    fn debug(&mut self, n: usize) {
        let i = std::cmp::max(0, self.stack.len() as isize - n as isize) as usize;
        println!("Debug: ip: {} fp: {} stack: [{}..] {:?}", self.ip, self.fp, i, &self.stack[i..])
    }

    fn add_a(&mut self, a: usize, b: usize) {
        let a = self.arg(a).int().unwrap();
        let b = self.arg(b).int().unwrap();
        let ret = Value::Int(a + b);
        self.stack.push(ret);
    }

    fn lt_a(&mut self, a: usize, b: usize) {
        let a = self.arg(a).int().unwrap();
        let b = self.arg(b).int().unwrap();
        let ret = Value::Int((a < b) as isize);
        self.stack.push(ret);
    }

    fn car_a(&mut self, list: Addr) {
        let head = self.arg(list).list().unwrap()[0].clone();
        self.stack.push(head);
    }

    fn car(&mut self) {
        let head = self.pop_undefer().list().unwrap()[0].clone();
        self.stack.push(head);
    }

    fn cdr(&mut self) {
        let mut tail = self.pop_undefer().as_list_mut();
        tail.pop_front();
        self.stack.push(Value::List(tail));
    }

    fn cdr_a(&mut self, list: Addr) {
        let mut tail = self.arg(list).list().unwrap().clone();
        tail.pop_front();
        self.stack.push(Value::List(tail));
    }

    fn invoke(&mut self) {
        match self.pop_undefer() {
            Value::Function(addr) => self.call(addr),
            Value::Builtin(addr) => self.builtin(BuiltinFunction::from_usize(addr).unwrap()),

            other => panic!("not invokable: {:?}", other),
        }
    }

    fn jumpz(&mut self, addr: Addr) {
        let top = self.pop_undefer();
        if self.is_false(top) {
            self.ip = addr;
        }
    }

    fn defer(&mut self, count: usize) {
        let tail = self.stack.split_off(self.stack.len() - count);
        self.stack.push(Value::Deferred(tail));
    }

    fn undefer(&mut self) {
        while let Value::Deferred(_) = self.stack.last().unwrap() {
            let call = self.stack.pop().unwrap().as_deferred();
            self.stack.extend(call);
            self.invoke();
        }
    }

    fn pop_undefer(&mut self) -> Value {
        let mut top = self.stack.pop().unwrap();
        while let Value::Deferred(call) = top {
            self.stack.extend(call);
            self.invoke();

            top = self.stack.pop().unwrap();
        }
        return top
    }

    fn builtin(&mut self, func: BuiltinFunction) {
        use BuiltinFunction::*;
        match func {
            Nop     => (),
            Except  => panic!("Builtin(Except) at {}", self.ip-1),
            Invoke  => self.invoke(),
            Pop     => self.pop(1),
            Undefer => self.undefer(),
            Add     => self.add(),
            Div     => self.div(),
            Mul     => self.mul(),
            Sub     => self.sub(),
            Eq      => self.eq(),
            Leq     => self.leq(),
            Lt      => self.lt(),
            Geq     => self.geq(),
            Gt      => self.gt(),
            Neq     => self.neq(),
            Car     => self.car(),
            Cdr     => self.cdr(),
        }
    }

    fn exec(&mut self) {
        while self.ip < self.prog.len() {
            let instr = self.prog[self.ip].clone();
            // println!("ip: {} {:?} {:?}", self.ip, instr, self.stack);
            self.ip += 1;

            use BC::*;
            match instr {
                Builtin(func)   => self.builtin(func),
                AddA(a, b)      => self.add_a(a, b),
                Arg(a)          => self.stack.push(self.arg(a).clone()),
                Call(addr)      => self.call(addr),
                CarA(a)         => self.car_a(a),
                CdrA(a)         => self.cdr_a(a),
                Debug(n)        => self.debug(n),
                Defer(n)        => self.defer(n),
                Jump(addr)      => self.ip = addr,
                JumpZ(addr)     => self.jumpz(addr),
                Label(_)        => (),
                LtA(a, b)       => self.lt_a(a, b),
                Pop(n)          => self.pop(n),
                Load(v)         => self.stack.push(self.data[v].clone()),
                PushFn(addr)    => self.stack.push(Value::Function(addr)),
                PushIns(addr)   => self.stack.push(Value::Builtin(addr)),
                Return(argc)    => { self.stdreturn(argc); break; },
                ReturnCall(argc) => self.returncall(argc),
            }
        }
    }
}

pub fn link(program: &mut Vec<BC>) {
    let mut map_label_addr: HashMap<Addr, Addr> = HashMap::new();
    let mut addr = 0;
    for i in 0..program.len() {
        if let BC::Label(index) = program[i] {
            map_label_addr.insert(index, addr);
        } else {
            program[addr] = program[i].clone();
            addr += 1;
        }
    }
    program.truncate(addr);
    for instr in program {
        match instr {
            BC::Jump(index)     => *instr = BC::Jump(map_label_addr[index]),
            BC::JumpZ(index)    => *instr = BC::JumpZ(map_label_addr[index]),
            BC::Call(index)     => *instr = BC::Call(map_label_addr[index]),
            BC::PushFn(index)   => *instr = BC::PushFn(map_label_addr[index]),
            _ => ()
        }
    }
}

pub fn execute(prog: Vec<BC>, data: Vec<Value>) {
    let mut arch = Arch::new(prog, data);
    arch.exec();
    println!("{:?}", arch);
}
