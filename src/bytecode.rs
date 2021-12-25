use std::collections::HashMap;
use std::collections::VecDeque;


// type SString = smartstring::SmartString::<smartstring::Compact>;
type Addr = usize;
pub type List = VecDeque<Value>;


#[derive(Debug)]
struct Arch {
    ip: Addr,
    fp: Addr,
    ap: Addr,
    stack: Vec<Value>,
}


#[derive(Clone, Debug)]
pub enum Value {
    Int(isize),
    // Float(f64),
    // Str(SString),
    List(List),
    Addr(Addr),
    None,
}

impl Value {
    pub fn as_int(&self) -> &isize {
        if let Value::Int(n) = self { n } else { panic!("not an int: {:?}", self) }
    }
    pub fn as_addr(&self) -> &Addr {
        if let Value::Addr(a) = self { a } else { panic!("not an addr: {:?}", self) }
    }
    pub fn as_list(&self) -> &List {
        if let Value::List(a) = self { a } else { panic!("not a list: {:?}", self) }
    }
}


#[derive(Clone, Debug)]
pub enum BC {
    // Data(Value),
    Arg(Addr),
    Call(Addr),
    Return,
    PushFrame,
    Push(Addr),
    Pop(usize),
    Jump(Addr),
    JumpZ(Addr),
    Label(Addr),
    Debug(usize),
    Add,
    Lt,
    AddA(Addr, Addr),
    Car(Addr),
    Cdr(Addr),
}

// impl BC {
//     fn as_data(&self) -> &Value {
//         if let BC::Data(v) = self { v } else { panic!("not BC::Data: {:?}", self) }
//     }
// }


impl Arch {
    fn new(data: Vec<Value>) -> Arch {
        let new = Arch{
            ip: 0,
            fp: data.len(),
            ap: data.len(),
            stack: data,
        };

        return new
    }

    fn is_false(&self, v: Value) -> bool {
        use Value::*;
        match v {
            Int(n)      => n == 0,
            List(list)  => list.is_empty(),
            None        => true,
            _           => panic!("invalid test: {:?}", v)
        }
    }

    // fn return_value(&mut self, v: Value) {
    //     // let raddr = *self.stack[self.stack.len() - 1].as_addr();
    //     let frame = *self.stack[self.fp - 1].as_addr();
    //     self.stack[self.fp - 2] = v;
    //     self.stack.truncate(self.fp - 1);
    //     self.fp = frame;
    //     // self.ip = raddr;
    // }

    fn stdreturn(&mut self) {
        let caller  = *self.stack[self.fp - 3].as_addr();
        let argp    = *self.stack[self.fp - 2].as_addr();
        let frame   = *self.stack[self.fp - 1].as_addr();
        self.stack[self.fp - 4] = self.stack.pop().unwrap();
        self.stack.truncate(self.fp - 3); // leave return value
        // println!("frame: {} -> {}", self.fp, frame);
        self.fp = frame;
        self.ap = argp;
        self.ip = caller;
    }

    fn push_frame(&mut self) {
        self.stack.push(Value::None); // return value (fp-4)
        self.stack.push(Value::None); // return addr (fp-3)
        self.stack.push(Value::Addr(self.ap)); // arg pointer (fp-2)
        self.stack.push(Value::Addr(self.fp)); // previous frame (fp-1)

        // println!("frame: {} -> {}", self.fp, self.stack.len());
        self.fp = self.stack.len();
    }

    fn call(&mut self, addr: Addr) {
        self.stack[self.fp - 3] = Value::Addr(self.ip);
        self.ap = self.fp;
        self.ip = addr;
    }
    
    fn arg(&self, i: Addr) -> &Value {
        return &self.stack[self.ap + i]
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
        let a = *self.stack.pop().unwrap().as_int();
        let b = *self.stack.pop().unwrap().as_int();
        let ret = Value::Int((a < b) as isize);
        self.push(ret);
    }

    fn add_a(&mut self, a: usize, b: usize) {
        let a = self.arg(a).as_int();
        let b = self.arg(b).as_int();
        let ret = Value::Int(a + b);
        self.push(ret);
    }

    fn car(&mut self, list: Addr) {
        let head = self.arg(list).as_list()[0].clone();
        self.push(head);
    }

    fn cdr(&mut self, list: Addr) {
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
                Add         => self.add(),
                Lt          => self.lt(),
                AddA(a, b)  => self.add_a(*a, *b),
                Arg(a)      => self.push(self.arg(*a).clone()),
                Car(a)      => self.car(*a),
                Cdr(a)      => self.cdr(*a),
                PushFrame   => self.push_frame(),
                Push(v)     => self.push(self.stack[*v].clone()),
                Pop(n)      => self.pop(*n),
                Debug(n)    => self.debug(*n),
                Jump(addr)  => {
                    self.ip = *addr;
                    continue
                },
                JumpZ(addr) => {
                    let top = self.stack.pop().unwrap();
                    if self.is_false(top) {
                        self.ip = *addr;
                        continue
                    }
                },
                Call(addr)  => self.call(*addr),
                Return      => self.stdreturn(),
                Label(_)    => (),
                // Data(_)     => panic!("cannot execute data"),
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
