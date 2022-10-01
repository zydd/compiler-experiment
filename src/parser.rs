use regex::Regex;
use unescape::unescape;

use crate::compiler::*;
use crate::bytecode::Value;


struct ParserState {
    par: Option<char>,
    defer_next: bool,
    list: Vec<Function>,
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

pub fn parse(code: String) -> Result<Vec<Function>, String> {
    let mut stack: Vec<ParserState> = Vec::new();
    let mut state = ParserState::new(None);

    let re_token = Regex::new(
        "^;[^\\n]*\
        |^\\s+\
        |^(?P<d>')\
        |^(?P<p>[()])\
        |^(?P<l>[\\[\\]])\
        |^\"(?P<q>[^\"\\\\]*(\\\\.[^\"\\\\]*)*)\"\
        |^(?P<f>\\d+\\.\\d*)\
        |^(?P<n>\\d+)\
        |^(?P<b>(True|False)\\b)\
        |^(?P<t>[^\\s()\\[\\]]+)\
        ").unwrap();

    let mut i = 0;
    while i < code.len() {
        let mut cur = None;
        let cap = re_token.captures(&code[i..]).expect("token");
        i += cap[0].len();
        // println!("{:?}", cap);

        if cap.name("p").is_some() || cap.name("l").is_some() {
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

                    if matches!(&new.list[0], Function::Unknown(name) if name == "def") {
                        cur = Some(FunctionDefinition::new(new.list))
                    } else {
                        cur = Some(FunctionCall::new(new.list))
                    }
                },
                "]" => {
                    let new = state;
                    if new.par != Some('[') {
                        return Err("mismatched parenthesis".to_string())
                    }
                    state = stack.pop().unwrap();

                    let list = new.list.iter().map(|x| x.literal().expect("literal").borrow().value.clone()).collect();
                    cur = Some(FunctionLiteral::new(Value::List(list)));
                },
                _ => return Err("unmatched parenthesis".to_string()),
            }
        } else if cap.name("f").is_some() {
            cur = Some(FunctionLiteral::new(Value::Float(cap[0].parse::<f64>().unwrap())));
        } else if cap.name("n").is_some() {
            cur = Some(FunctionLiteral::new(Value::Int(cap[0].parse::<isize>().unwrap())));
        } else if cap.name("b").is_some() {
            cur = Some(FunctionLiteral::new(Value::Bool(&cap[0] == "True")));
        } else if cap.name("q").is_some() {
            cur = Some(FunctionLiteral::new(Value::Str(unescape(&cap["q"]).unwrap())));
        } else if cap.name("t").is_some() {
            cur = Some(Function::new(String::from(&cap[0])));
        } else if cap.name("d").is_some() {
            state.defer_next = true;
        } else {
            // println!("skip: {:?}", &cap[0])
        }

        match cur {
            None => (),
            Some(cur) => {
                if state.defer_next {
                    state.defer_next = false;
                    cur.call().unwrap().borrow_mut().deferred = true;
                }
                state.list.push(cur);
            },
        }
    }

    return Ok(state.list)
}
