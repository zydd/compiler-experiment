use regex::Regex;
use unescape::unescape;


#[derive(Clone, Debug, PartialEq)]
pub enum S {
    Int(isize),
    Float(f64),
    Str(String),
    Token(String),
    List(Vec<S>),
    Deferred(Box<S>),
    S(Vec<S>),
    Empty,
}

#[allow(dead_code)]
impl S {
    pub fn as_list(&self) -> &Vec<S> {
        if let S::List(list) = self { list } else { panic!("not a list: {}", self) }
    }
    pub fn as_expr(&self) -> &Vec<S> {
        if let S::S(list) = self { list } else { panic!("not an expression") }
    }
    pub fn as_def(&self) -> &Vec<S> {
        if let S::Deferred(list) = self { list.as_expr() } else { panic!("not a defferred expression") }
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
            S::Int(i)       => write!(f, "{}", i),
            S::Float(i)     => write!(f, "{}", i),
            S::Str(s)       => write!(f, "{:?}", s),
            S::Token(s)     => write!(f, "{}", s),
            S::Deferred(s)  => write!(f, "'{}", s),
            S::Empty        => write!(f, "{{}}"),
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


#[derive(Debug)]
struct ParserState {
    par: Option<char>,
    defer_next: bool,
    list: Vec<S>,
}

impl ParserState {
    fn new(par: Option<char>) -> ParserState {
        return ParserState{
            par: par,
            defer_next: false,
            list: vec![],
        }
    }
}

pub fn parse(code: String) -> Result<Vec<S>, String> {
    let mut stack: Vec<ParserState> = Vec::new();
    let mut state = ParserState::new(None);

    let re_token = Regex::new(
        "^;[^\\n]*\
        |^\\s+\
        |^(?P<d>')\
        |^(?P<p>[()])\
        |^(?P<b>[\\[\\]])\
        |^\"(?P<q>[^\"\\\\]*(\\\\.[^\"\\\\]*)*)\"\
        |^(?P<f>\\d+\\.\\d*)\
        |^(?P<n>\\d+)\
        |^(?P<t>[^\\s()]+)\
        ").unwrap();

    let mut i = 0;
    while i < code.len() {
        let mut cur = S::Empty;
        let cap = re_token.captures(&code[i..]).expect("token");
        i += cap[0].len();
        // println!("{:?}", cap);

        if cap.name("p").is_some() || cap.name("b").is_some() {
            match &cap[0] {
                "(" | "[" => {
                    stack.push(state);
                    state = ParserState::new(Some(cap[0].chars().nth(0).unwrap()));
                },
                ")" => {
                    let new = state;
                    if new.par != Some('(') {
                        return Err("mismatched parenthesis".to_string())
                    }
                    state = stack.pop().unwrap();
                    cur = S::S(new.list);
                },
                "]" => {
                    let new = state;
                    if new.par != Some('[') {
                        return Err("mismatched parenthesis".to_string())
                    }
                    state = stack.pop().unwrap();
                    cur = S::List(new.list);
                },
                _ => return Err("unmatched parenthesis".to_string()),
            }
        } else if cap.name("f").is_some() {
            cur = S::Float(cap[0].parse::<f64>().unwrap());
        } else if cap.name("n").is_some() {
            cur = S::Int(cap[0].parse::<isize>().unwrap());
        } else if cap.name("q").is_some() {
            cur = S::Str(unescape(&cap["q"]).unwrap());
        } else if cap.name("t").is_some() {
            cur = S::Token(String::from(&cap[0]));
        } else if cap.name("d").is_some() {
            state.defer_next = true;
        } else {
            // println!("skip: {:?}", &cap[0])
        }

        match cur {
            S::Empty => (),
            _ => {
                if state.defer_next {
                    state.defer_next = false;
                    state.list.push(S::Deferred(Box::new(cur)));
                } else {
                    state.list.push(cur);
                }
            },
        }
    }

    return Ok(state.list)
}
