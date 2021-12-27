use std::collections::HashMap;
use std::collections::VecDeque;


// type SString = smartstring::SmartString::<smartstring::Compact>;
type Addr = usize;
pub type List = VecDeque<Value>;


#[derive(Debug)]
struct Arch {
    ip: Addr,
    fp: Addr,
    stack: Vec<Value>,
}


#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Int(isize),
    Float(f64),
    Str(String),
    List(List),
    Deferred(Vec<Value>),
    Addr(Addr),
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
    Push(Addr),
    PushFn(Addr),
    PushIns(Addr),
    Return(usize),
}

use num_traits::FromPrimitive;
#[derive(Clone, Debug, num_derive::FromPrimitive)]
pub enum BuiltinFunction {
    Nop,
    Invoke,

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


impl Value {
    pub fn as_int(&self) -> &isize {
        if let Value::Int(n) = self { n } else { panic!("not an int: {:?}", self) }
    }
    pub fn as_addr(self) -> Addr {
        if let Value::Addr(a) = self { a } else { panic!("not an addr: {:?}", self) }
    }
    pub fn as_list(&self) -> &List {
        if let Value::List(a) = self { a } else { panic!("not a list: {:?}", self) }
    }
    pub fn as_list_mut(self) -> List {
        if let Value::List(a) = self { a } else { panic!("not a list: {:?}", self) }
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

    fn new(data: Vec<Value>) -> Arch {
        let new = Arch{
            ip: 0,
            fp: data.len(),
            stack: data,
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
        assert_eq!(self.stack.len(), self.fp + 3);

        let ret     = self.stack.pop().unwrap();
        let framep  = self.stack.pop().unwrap().as_addr();
        let raddr   = self.stack.pop().unwrap().as_addr();

        self.stack.truncate(self.fp - argc);
        self.stack.push(ret);

        self.fp = framep;
        self.ip = raddr;
    }

    fn call(&mut self, addr: Addr) {
        let frame = self.stack.len();

        self.stack.push(Value::Addr(self.ip)); // return addr (fp+0)
        self.stack.push(Value::Addr(self.fp)); // previous frame (fp+1)

        self.ip = addr;
        self.fp = frame;
    }

    fn arg(&self, i: Addr) -> &Value {
        return &self.stack[self.fp-1 - i]
    }

    fn pop(&mut self, n: usize) {
        self.stack.truncate(self.stack.len() - n);
    }

    fn debug(&mut self, n: usize) {
        let i = std::cmp::max(0, self.stack.len() as isize - n as isize) as usize;
        println!("ip: {} fp: {} stack: [{}..] {:?}", self.ip, self.fp, i, &self.stack[i..])
    }

    fn add_a(&mut self, a: usize, b: usize) {
        let a = self.arg(a).as_int();
        let b = self.arg(b).as_int();
        let ret = Value::Int(a + b);
        self.stack.push(ret);
    }

    fn lt_a(&mut self, a: usize, b: usize) {
        let a = self.arg(a).as_int();
        let b = self.arg(b).as_int();
        let ret = Value::Int((a < b) as isize);
        self.stack.push(ret);
    }

    fn car_a(&mut self, list: Addr) {
        let head = self.arg(list).as_list()[0].clone();
        self.stack.push(head);
    }

    fn car(&mut self) {
        let head = self.pop_undefer().as_list()[0].clone();
        self.stack.push(head);
    }

    fn cdr(&mut self) {
        let mut tail = self.pop_undefer().as_list_mut();
        tail.pop_front();
        self.stack.push(Value::List(tail));
    }

    fn cdr_a(&mut self, list: Addr) {
        let mut tail = self.arg(list).as_list().clone();
        tail.pop_front();
        self.stack.push(Value::List(tail));
    }

    fn defer(&mut self, count: usize) {
        let tail = self.stack.split_off(self.stack.len() - count);
        self.stack.push(Value::Deferred(tail));
    }

    fn invoke(&mut self) {
        match self.pop_undefer() {
            Value::Function(addr) => self.call(addr),
            Value::Builtin(addr) => self.instr(BC::Builtin(BuiltinFunction::from_usize(addr).unwrap())),

            other => panic!("not invokable: {:?}", other),
        }
    }

    fn jumpz(&mut self, addr: Addr) {
        let top = self.pop_undefer();
        if self.is_false(top) {
            self.ip = addr;
        }
    }

    fn pop_undefer(&mut self) -> Value {
        let top = self.stack.pop().unwrap();
        if let Value::Deferred(call) = top {
            self.stack.extend(call);
            self.invoke();
            return self.stack.pop().unwrap();
        }
        return top
    }

    fn instr(&mut self, instr: BC) {
        use BC::*;
        use BuiltinFunction::*;

        match instr {
            Builtin(Nop)    => (),
            Builtin(Invoke) => self.invoke(),
            Builtin(Add)    => self.add(),
            Builtin(Div)    => self.div(),
            Builtin(Mul)    => self.mul(),
            Builtin(Sub)    => self.sub(),
            Builtin(Eq)     => self.eq(),
            Builtin(Leq)    => self.leq(),
            Builtin(Lt)     => self.lt(),
            Builtin(Geq)    => self.geq(),
            Builtin(Gt)     => self.gt(),
            Builtin(Neq)    => self.neq(),
            Builtin(Car)    => self.car(),
            Builtin(Cdr)    => self.cdr(),
            Return(argc)    => self.stdreturn(argc),
            AddA(a, b)      => self.add_a(a, b),
            Arg(a)          => self.stack.push(self.arg(a).clone()),
            Call(addr)      => self.call(addr),
            CarA(a)         => self.car_a(a),
            CdrA(a)         => self.cdr_a(a),
            Debug(n)        => self.debug(n),
            Defer(n)        => self.defer(n),
            Jump(addr)      => self.ip = addr,
            LtA(a, b)       => self.lt_a(a, b),
            Pop(n)          => self.pop(n),
            Push(v)         => self.stack.push(self.stack[v].clone()),
            PushFn(addr)    => self.stack.push(Value::Function(addr)),
            PushIns(addr)   => self.stack.push(Value::Builtin(addr)),
            JumpZ(addr)     => self.jumpz(addr),
            Label(_)        => (),
        }
    }

    fn exec(&mut self, program: &[BC]) {
        while self.ip < program.len() {
            let instr = &program[self.ip];
            // println!("ip: {} {:?} {:?}", self.ip, instr, self);
            self.ip += 1;
            self.instr(instr.clone());
        }
    }
}

pub fn link(program: &mut [BC]) {
    let mut map_label_addr: HashMap<Addr, Addr> = HashMap::new();
    for (addr, instr) in program.iter().enumerate() {
        if let BC::Label(index) = instr {
            map_label_addr.insert(*index, addr);
        }
    }
    for (_, instr) in program.iter_mut().enumerate() {
        match instr {
            BC::Jump(index)     => *instr = BC::Jump(map_label_addr[index]),
            BC::JumpZ(index)    => *instr = BC::JumpZ(map_label_addr[index]),
            BC::Call(index)     => *instr = BC::Call(map_label_addr[index]),
            BC::PushFn(index)   => *instr = BC::PushFn(map_label_addr[index]),
            _ => ()
        }
    }
}

pub fn execute(prog: &[BC], data: Vec<Value>) {
    let sp = data.len();
    let mut arch = Arch::new(data);
    println!("{:?}", arch);
    arch.exec(&prog);
    println!("{:?}", arch);
    println!("{:?}", &arch.stack[sp..]);
}
