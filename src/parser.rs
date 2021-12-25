use regex::Regex;
use unescape::unescape;


#[derive(Clone, Debug, PartialEq)]
pub enum S {
    Int(isize),
    Float(f64),
    Str(String),
    Token(String),
    List(Vec<S>),
    S(Vec<S>),
    Empty,
}

impl S {
    pub fn as_list(&self) -> &Vec<S> {
        if let S::List(list) = self { list } else { panic!("not a list: {}", self) }
    }
    pub fn as_expr(&self) -> &Vec<S> {
        if let S::S(list) = self { list } else { panic!("not an expression") }
    }
    pub fn as_token(&self) -> &String {
        if let S::Token(str) = self { str } else { panic!("not a token") }
    }
    pub fn as_int(&self) -> &isize {
        if let S::Int(n) = self { n } else { panic!("not an int: {:?}", self) }
    }

    // fn is_atom(&self) -> bool {
    //     match self {
    //         S::Int(_) | S::Str(_) | S::Empty => true,
    //         _ => false
    //     }
    // }
}

impl std::fmt::Display for S {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            S::Int(i)   => write!(f, "{}", i),
            S::Float(i) => write!(f, "{}", i),
            S::Str(s)   => write!(f, "{:?}", s),
            S::Token(s) => write!(f, "{}", s),
            S::Empty    => write!(f, "{{}}"),
            S::List(list) | S::S(list) => {
                write!(f, "{}", if let S::S(_) = self {"("} else {"["})?;
                for (i, el) in list.iter().enumerate() {
                    if i != 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}",  el)?;
                }
                write!(f, "{}", if let S::S(_) = self {")"} else {"]"})
            },
        }
    }
}


pub fn parse(code: String) -> Result<Vec<S>, String> {
    let mut stack: Vec<Vec<S>> = vec![vec![]];

    let re_token = Regex::new(
        "^;[^\\n]*\
        |^\\s+\
        |^(?P<p>[()])\
        |^(?P<b>[\\[\\]])\
        |^'(?P<q>[^'\\\\]*(\\\\.[^'\\\\]*)*)'\
        |^\"(?P<qq>[^\"\\\\]*(\\\\.[^\"\\\\]*)*)\"\
        |^(?P<f>\\d+\\.\\d*)\
        |^(?P<n>\\d+)\
        |^(?P<t>[^\\s()]+)\
        ").unwrap();

    let mut i = 0;
    while i < code.len() {
        let cap = re_token.captures(&code[i..]).expect("token");
        // println!("{:?}", cap);
        let mut cur = S::Empty;

        if cap.name("p").is_some() || cap.name("b").is_some() {
            match &cap[0] {
                "(" | "[" => stack.push(vec![]),
                ")" => {
                    let last = stack.pop().unwrap();
                    if last.is_empty() {
                        stack.last_mut().unwrap().push(S::Empty);
                    } else {
                        stack.last_mut().unwrap().push(S::S(last));
                    }
                },
                "]" => {
                    let last = stack.pop().unwrap();
                    stack.last_mut().unwrap().push(S::List(last));
                },
                _ => return Err("unmatched parenthesis".to_string()),
            }
        } else if cap.name("f").is_some() {
            cur = S::Float(cap[0].parse::<f64>().unwrap());
        } else if cap.name("n").is_some() {
            cur = S::Int(cap[0].parse::<isize>().unwrap());
        } else if cap.name("q").is_some() {
            cur = S::Str(unescape(&cap["q"]).unwrap());
        } else if cap.name("qq").is_some() {
            cur = S::Str(unescape(&cap["qq"]).unwrap());
        } else if cap.name("t").is_some() {
            cur = S::Token(String::from(&cap[0]));
        } else {
            // println!("skip: {:?}", &cap[0])
        }

        i += cap[0].len();

        match cur {
            S::Empty => (),
            _ => {
                // println!("{:?}", cur);
                stack.last_mut().unwrap().push(cur);
            },
        }
    }

    return Ok(stack.pop().unwrap())
}
