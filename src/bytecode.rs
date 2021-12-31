// type SString = smartstring::SmartString::<smartstring::Compact>;
pub type Addr = u32;
pub type Argc = u16;
type ListType = std::collections::VecDeque<Value>;
type IntType = isize;
type FloatType = f64;


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
    Bool(bool),
    Int(IntType),
    Float(FloatType),
    Str(String),
    List(ListType),
    Deferred(Vec<Value>),
    Function(Addr),
    Builtin(Addr),
}


#[allow(dead_code)]
#[derive(Clone, Debug)]
pub enum BC {
    Builtin(BuiltinFunction),
    AddA(Argc, Argc),
    Arg(Argc),
    Bne(Addr),
    CarA(Argc),
    CdrA(Argc),
    Call(Addr),
    Debug(Argc),
    Defer(Argc),
    Label(Addr),
    Load(Addr),
    LtA(Argc, Argc),
    MoveArgs(Argc),
    Jump(Addr),
    Pop(Argc),
    PushFn(Addr),
    PushIns(Addr),
    Return(Argc),
    ReturnCall(Argc),
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
    enum_as!(ListType, list, Value::List);
    pub fn as_list_mut(self) -> ListType {
        if let Value::List(a) = self { a } else { panic!("not a list: {:?}", self) }
    }
    pub fn as_deferred(self) -> Vec<Value> {
        if let Value::Deferred(a) = self { a } else { panic!("not a deferred value: {:?}", self) }
    }
}

impl std::convert::From<bool> for Value {
    fn from(v: bool) -> Self { Value::Bool(v) }
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
        match (self, rhs) {
            (Value::Int(a),   Value::Int(b))   => a.partial_cmp(b),
            (Value::Float(a), Value::Float(b)) => a.partial_cmp(b),
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
            self.stack.push(ret.into());
        }
    };
}

impl Arch {
    builtin!(add, |a, b| a + b);
    builtin!(div, |a, b| a / b);
    builtin!(mul, |a, b| a * b);
    builtin!(sub, |a, b| a - b);
    builtin!(lt,  |a, b| a <  b);
    builtin!(leq, |a, b| a <= b);
    builtin!(gt,  |a, b| a >  b);
    builtin!(geq, |a, b| a >= b);
    builtin!(eq,  |a, b| a == b);
    builtin!(neq, |a, b| a != b);

    fn new(prog: Vec<BC>, data: Vec<Value>) -> Arch {
        let new = Arch{
            ip: 0,
            fp: 0,
            prog: prog,
            data: data,
            stack: Vec::with_capacity(100),
        };

        return new
    }

    fn stdreturn(&mut self, argc: Argc) {
        if argc > 0 {
            let ret = self.stack.pop().unwrap();

            self.stack.truncate(self.fp as usize - argc as usize);
            self.stack.push(ret);
        }
    }

    fn returncall(&mut self, argc: Argc) {
        let stack_len = self.stack.len() - argc as usize;
        while self.stack.len() != stack_len {
            self.invoke();
        }
    }

    fn call(&mut self, addr: Addr) {
        let frame = self.stack.len();

        let ip = self.ip;
        let fp = self.fp;

        self.ip = addr;
        self.fp = frame as Addr;

        self.exec();

        self.ip = ip;
        self.fp = fp;
    }

    fn arg(&self, i: Argc) -> &Value {
        return &self.stack[self.fp as usize - 1 - i as usize]
    }

    fn move_args(&mut self, argc: Argc) {
        self.stack.drain((self.fp as usize - argc as usize)..self.fp as usize);
        self.fp = self.stack.len() as Addr;
    }

    fn pop(&mut self, n: Argc) {
        self.stack.truncate(self.stack.len() - n as usize);
    }

    fn debug(&mut self, n: Argc) {
        let i = std::cmp::max(0, self.stack.len() as isize - n as isize) as usize;
        println!("Debug: ip: {} fp: {} stack: [{}..] {:?}", self.ip, self.fp, i, &self.stack[i..])
    }

    fn add_a(&mut self, a: Argc, b: Argc) {
        let a = self.arg(a).int().unwrap();
        let b = self.arg(b).int().unwrap();
        let ret = Value::Int(a + b);
        self.stack.push(ret);
    }

    fn lt_a(&mut self, a: Argc, b: Argc) {
        let a = self.arg(a).int().unwrap();
        let b = self.arg(b).int().unwrap();
        let ret = Value::Int((a < b) as isize);
        self.stack.push(ret);
    }

    fn car_a(&mut self, list: Argc) {
        let head = self.arg(list).list().unwrap()[0].clone();
        self.stack.push(head);
    }

    fn car(&mut self) {
        let head = self.pop_undefer().list().unwrap()[0].clone();
        self.stack.push(head);
    }

    fn cdr(&mut self) {
        let mut tail = self.pop_undefer().as_list_mut();
        // tail.pop_front();
        tail.remove(0);
        self.stack.push(Value::List(tail));
    }

    fn cdr_a(&mut self, list: Argc) {
        let mut tail = self.arg(list).list().unwrap().clone();
        // tail.pop_front();
        tail.remove(0);
        self.stack.push(Value::List(tail));
    }

    fn invoke(&mut self) {
        match self.pop_undefer() {
            Value::Function(addr) => self.call(addr),
            Value::Builtin(addr) => self.builtin(BuiltinFunction::from_usize(addr as usize).unwrap()),

            other => panic!("not invokable: {:?}", other),
        }
    }

    fn bne(&mut self, addr: Addr) {
        let a = self.pop_undefer();
        let b = self.pop_undefer();
        if a != b {
            self.ip = addr;
        }
    }

    fn defer(&mut self, count: Argc) {
        let tail = self.stack.split_off(self.stack.len() - count as usize);
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
        loop {
            let instr = self.prog[self.ip as usize].clone();
            // println!("ip: {} {:?} {:?}", self.ip, instr, self.stack);
            self.ip += 1;

            use BC::*;
            match instr {
                Builtin(func)   => self.builtin(func),
                AddA(a, b)      => self.add_a(a, b),
                Arg(a)          => self.stack.push(self.arg(a).clone()),
                Bne(addr)       => self.bne(addr),
                Call(addr)      => self.call(addr),
                CarA(a)         => self.car_a(a),
                CdrA(a)         => self.cdr_a(a),
                Debug(n)        => self.debug(n),
                Defer(n)        => self.defer(n),
                Jump(addr)      => self.ip = addr,
                Label(_)        => (),
                Load(v)         => self.stack.push(self.data[v as usize].clone()),
                LtA(a, b)       => self.lt_a(a, b),
                MoveArgs(argc)  => self.move_args(argc),
                Pop(n)          => self.pop(n),
                PushFn(addr)    => self.stack.push(Value::Function(addr)),
                PushIns(addr)   => self.stack.push(Value::Builtin(addr)),
                Return(argc)    => {self.stdreturn(argc); break},
                ReturnCall(argc) => self.returncall(argc),
            }
        }
    }
}

pub fn execute(mut prog: Vec<BC>, data: Vec<Value>) {
    prog.push(BC::Return(0));

    let mut arch = Arch::new(prog, data);
    arch.exec();

    println!("{:?}", arch);
}
