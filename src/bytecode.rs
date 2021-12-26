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


#[derive(Clone, Debug)]
pub enum Value {
    Int(isize),
    Float(f64),
    Str(String),
    List(List),
    Addr(Addr),
    Function(Addr),
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub enum BC {
    Add,
    Car,
    Cdr,
    Invoke,
    Lt,
    AddA(Addr, Addr),
    Arg(Addr),
    CarA(Addr),
    CdrA(Addr),
    Call(Addr),
    Debug(usize),
    Label(Addr),
    LtA(Addr, Addr),
    Jump(Addr),
    JumpZ(Addr),
    Pop(usize),
    Push(Addr),
    PushFn(Addr),
    Return(usize),
}


impl Value {
    pub fn as_int(&self) -> &isize {
        if let Value::Int(n) = self { n } else { panic!("not an int: {:?}", self) }
    }
    pub fn as_addr(&self) -> &Addr {
        if let Value::Addr(a) = self { a } else { panic!("not an addr: {:?}", self) }
    }
    pub fn as_fn_addr(&self) -> &Addr {
        if let Value::Function(a) = self { a } else { panic!("not function: {:?}", self) }
    }
    pub fn as_list(&self) -> &List {
        if let Value::List(a) = self { a } else { panic!("not a list: {:?}", self) }
    }
    pub fn as_list_mut(self) -> List {
        if let Value::List(a) = self { a } else { panic!("not a list: {:?}", self) }
    }
}


impl Arch {
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
        let raddr   = *self.stack[self.fp + 0].as_addr();
        let framep  = *self.stack[self.fp + 1].as_addr();
        let ret     = self.stack.pop().unwrap();
        self.stack.truncate(self.fp - argc);
        self.stack.push(ret);
        // println!("frame: {} -> {}", self.fp, frame);
        self.fp = framep;
        self.ip = raddr;
    }

    fn call(&mut self, addr: Addr) {
        let fp = self.fp;
        self.fp = self.stack.len();
        self.stack.push(Value::Addr(self.ip)); // return addr (fp+0)
        self.stack.push(Value::Addr(fp)); // previous frame (fp+1)

        self.ip = addr;
    }

    fn arg(&self, i: Addr) -> &Value {
        return &self.stack[self.fp-1 - i]
    }

    fn push(&mut self, v: Value) {
        self.stack.push(v);
    }

    fn pop(&mut self, n: usize) {
        self.stack.truncate(self.stack.len() - n);
    }

    fn debug(&mut self, n: usize) {
        let i = std::cmp::max(0, self.stack.len() as isize - n as isize) as usize;
        println!("ip: {} fp: {} stack: [{}..] {:?}", self.ip, self.fp, i, &self.stack[i..])
    }

    fn add(&mut self) {
        let a = *self.stack.pop().unwrap().as_int();
        let b = *self.stack.pop().unwrap().as_int();
        let ret = Value::Int(a + b);
        self.push(ret);
    }

    fn lt(&mut self) {
        let b = *self.stack.pop().unwrap().as_int();
        let a = *self.stack.pop().unwrap().as_int();
        let ret = Value::Int((a < b) as isize);
        self.push(ret);
    }

    fn add_a(&mut self, a: usize, b: usize) {
        let a = self.arg(a).as_int();
        let b = self.arg(b).as_int();
        let ret = Value::Int(a + b);
        self.push(ret);
    }

    fn lt_a(&mut self, a: usize, b: usize) {
        let a = self.arg(a).as_int();
        let b = self.arg(b).as_int();
        let ret = Value::Int((a < b) as isize);
        self.push(ret);
    }

    fn car_a(&mut self, list: Addr) {
        let head = self.arg(list).as_list()[0].clone();
        self.push(head);
    }

    fn car(&mut self) {
        let head = self.stack.pop().unwrap().as_list()[0].clone();
        self.push(head);
    }

    fn cdr(&mut self) {
        let mut tail = self.stack.pop().unwrap().as_list_mut();
        tail.pop_front();
        self.push(Value::List(tail));
    }

    fn cdr_a(&mut self, list: Addr) {
        let mut tail = self.arg(list).as_list().clone();
        tail.pop_front();
        self.push(Value::List(tail));
    }

    fn exec(&mut self, program: &[BC]) {
        use BC::*;

        while self.ip < program.len() {
            let instr = &program[self.ip];
            self.ip += 1;
            // println!("ip: {} {:?} {:?}", self.ip, instr, self);

            match instr {
                Add             => self.add(),
                Car             => self.car(),
                Cdr             => self.cdr(),
                Lt              => self.lt(),
                Return(argc)    => self.stdreturn(*argc),
                AddA(a, b)      => self.add_a(*a, *b),
                Arg(a)          => self.push(self.arg(*a).clone()),
                Call(addr)      => self.call(*addr),
                CarA(a)         => self.car_a(*a),
                CdrA(a)         => self.cdr_a(*a),
                Debug(n)        => self.debug(*n),
                Jump(addr)      => self.ip = *addr,
                LtA(a, b)       => self.lt_a(*a, *b),
                Pop(n)          => self.pop(*n),
                Push(v)         => self.push(self.stack[*v].clone()),
                PushFn(addr)    => self.push(Value::Function(*addr)),
                JumpZ(addr)     => {
                    let top = self.stack.pop().unwrap();
                    if self.is_false(top) {
                        self.ip = *addr;
                        continue
                    }
                },
                Invoke          => {
                    let top = self.stack.pop().unwrap();
                    self.call(*top.as_fn_addr())
                },
                Label(_)        => (),
            }
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
            BC::Jump(index)     => *instr = BC::Jump(map_label_addr[&index]),
            BC::JumpZ(index)    => *instr = BC::JumpZ(map_label_addr[&index]),
            BC::Call(index)     => *instr = BC::Call(map_label_addr[&index]),
            BC::PushFn(index)   => *instr = BC::PushFn(map_label_addr[&index]),
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
