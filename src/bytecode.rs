pub(crate) mod runtime;

use self::runtime::*;

// type SString = smartstring::SmartString::<smartstring::Compact>;
pub type Addr = u32;
pub type Argc = i16;
type ListType = std::collections::VecDeque<Value>;
type IntType = isize;
type FloatType = f64;


#[derive(Debug)]
struct Arch<'a> {
    runtime: &'a Runtime,
    prog: &'a Vec<BC>,
    data: &'a Vec<Value>,
    ip: Addr,
    fp: Addr,
    stack: Vec<Value>,
}


#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    None,
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
#[derive(Clone, Debug, PartialEq)]
pub enum BC {
    Builtin(Addr),
    AddA(Argc, Argc),
    Arg(Argc),
    Bne(Addr),
    Beqz(Addr),
    Branch(Argc),
    Call(Addr),
    CarA(Argc),
    CdrA(Argc),
    Defer(Argc),
    Jump(Addr),
    Label(Addr),
    Load(Addr),
    LtA(Argc, Argc),
    MoveArgs(Argc),
    Pop(Argc),
    PushFn(Addr),
    PushIns(Addr),
    PushInt(i32),
    Return(Argc),
    ReturnCall(),
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
        if let Value::List(a) = self { a } else { panic!("expected list: {:?}", self) }
    }
    pub fn as_deferred(self) -> Vec<Value> {
        if let Value::Deferred(a) = self { a } else { panic!("expected deferred: {:?}", self) }
    }
    pub fn as_int(self) -> IntType {
        if let Value::Int(a) = self { a } else { panic!("expected Int: {:?}", self) }
    }
    pub fn as_float(self) -> FloatType {
        if let Value::Float(a) = self { a } else { panic!("expected Float: {:?}", self) }
    }
    pub fn as_bool(self) -> bool {
        if let Value::Bool(a) = self { a } else { panic!("expected Bool: {:?}", self) }
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
            _ => panic!("invalid comparison: {:?} {:?}", self, rhs),
        }
    }
}

impl Arch<'_> {
    fn new<'a>(runtime: &'a Runtime, prog: &'a Vec<BC>, data: &'a Vec<Value>) -> Arch<'a> {
        return Arch {
            runtime: &runtime,
            prog: prog,
            data: data,
            ip: 0,
            fp: 0,
            stack: Vec::with_capacity(100),
        }
    }

    fn stdreturn(&mut self, argc: Argc) {
        if argc > 0 {
            let ret = self.stack.pop().unwrap();

            self.stack.truncate(self.fp as usize - argc as usize);
            self.stack.push(ret);
        }
    }

    fn returncall(&mut self) {
        while self.stack.len() > (self.fp + 1) as usize {
            Arch::invoke(self);
        }

        assert_eq!(self.stack.len(), (self.fp + 1) as usize);
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
        return &self.stack[self.fp as usize - i as usize]
    }

    fn move_args(&mut self, argc: Argc) {
        self.stack.drain((self.fp as usize - argc as usize)..self.fp as usize);
        self.fp = self.stack.len() as Addr;
    }

    fn pop(&mut self, n: Argc) {
        self.stack.truncate(self.stack.len() - n as usize);
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

    fn cdr_a(&mut self, list: Argc) {
        let mut tail = self.arg(list).list().unwrap().clone();
        // tail.pop_front();
        tail.remove(0);
        self.stack.push(Value::List(tail));
    }

    fn bne(&mut self, addr: Addr) {
        let a = self.pop_undefer();
        let b = self.pop_undefer();
        if a != b {
            self.ip = addr;
        }
    }

    fn bnt(&mut self, addr: Addr) {
        let a = self.pop_undefer();
        if let Value::Bool(result) = a {
            if !result {
                self.ip = addr;
            }
        } else {
            panic!("expected boolean value");
        }
    }

    fn branch(&mut self, addr: Argc) {
        let arg = &mut self.stack[self.fp as usize - addr as usize];

        if matches!(arg, Value::Deferred(_)) {
            let mut branch = Arch::new(self.runtime, self.prog, self.data);
            branch.stack = std::mem::replace(arg, Value::None).as_deferred();
            Arch::invoke(&mut branch);

            assert_eq!(branch.stack.len(), 1);
            *arg = branch.stack.pop().unwrap();
        }
    }

    fn defer(&mut self, count: Argc) {
        let tail = self.stack.split_off(self.stack.len() - count as usize);
        self.stack.push(Value::Deferred(tail));
    }

    fn pop_undefer(&mut self) -> Value {
        let mut top = self.stack.pop().unwrap();

        while let Value::Deferred(call) = top {
            self.stack.extend(call);
            Arch::invoke(self);

            top = self.stack.pop().unwrap();
        }

        return top
    }

    fn exec(&mut self) {
        if ! self.prog.is_empty() {
        loop {
            let instr = self.prog[self.ip as usize].clone();
            // println!("ip: {:3} {:?}\t{:?}", self.ip, instr, &self.stack[std::cmp::max(0, self.stack.len() as isize - 300) as usize..]);
            self.ip += 1;

            use BC::*;
            match instr {
                Builtin(addr)   => self.builtin(addr as usize),
                AddA(a, b)      => self.add_a(a, b),
                Arg(a)          => self.stack.push(self.arg(a).clone()),
                Bne(addr)       => self.bne(addr),
                Beqz(addr)      => self.bnt(addr),
                Branch(addr)    => self.branch(addr),
                Call(addr)      => self.call(addr),
                CarA(a)         => self.car_a(a),
                CdrA(a)         => self.cdr_a(a),
                Defer(n)        => self.defer(n),
                Jump(addr)      => self.ip = addr,
                Label(_)        => (),
                Load(v)         => self.stack.push(self.data[v as usize].clone()),
                LtA(a, b)       => self.lt_a(a, b),
                MoveArgs(argc)  => self.move_args(argc),
                Pop(n)          => self.pop(n),
                PushFn(addr)    => self.stack.push(Value::Function(addr)),
                PushIns(addr)   => self.stack.push(Value::Builtin(addr)),
                PushInt(i)      => self.stack.push(Value::Int(i as IntType)),
                Return(argc)    => {self.stdreturn(argc); break},
                ReturnCall()    => self.returncall(),
            }
        }
        }
    }
}

pub fn execute(runtime: Runtime, prog: Vec<BC>, data: Vec<Value>) {
    let mut arch = Arch::new(&runtime, &prog, &data);
    arch.exec();

    // println!("{:?}", arch);
}
