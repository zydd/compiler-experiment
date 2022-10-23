use super::*;

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        fn fmt_fn_name(f: &mut std::fmt::Formatter, func: &Function) -> std::fmt::Result {
            match func {
                Function::Definition(func_data) =>
                    write!(f, "{}[fn:{}]", func_data.borrow().name, func_data.borrow().label)?,
                Function::Builtin(func_data) =>
                    write!(f, "#{}[op:{}]", func_data.borrow().name, func_data.borrow().addr.clone() as usize)?,
                other =>
                    write!(f, "{}", other)?,
            }
            return std::fmt::Result::Ok(())
        }

        match &self {
            Function::Arg(data) => {
                let arg = data.borrow();

                write!(f, "$")?;

                if arg.deferred {
                    write!(f, "'")?;
                } else if arg.strict {
                    write!(f, "!")?;
                }

                write!(f, "{}[use:{}]", arg.name, arg.refs)
            }

            Function::ArgRef(data) => write!(f, "&{}", data.borrow().name),
            Function::Builtin(_) => fmt_fn_name(f, self),

            Function::Call(data) => {
                let data = data.borrow();
                write!(f, "(")?;

                if data.function.is_some() {
                    fmt_fn_name(f, data.function.as_ref().unwrap())?;
                } else {
                    write!(f, "{}[?]", data.name)?;
                }

                if data.deferred {
                    write!(f, "[d]")?;
                }

                if data.recursive {
                    write!(f, "[r]")?;
                }

                if data.tail_call.is_some() {
                    write!(f, "[t]")?;
                }

                for arg in &data.args {
                    write!(f, " {}", arg)?;
                }
                write!(f, ")")
            }

            Function::Conditional(cond) => {
                let cond = cond.borrow();
                write!(f, "(if {}", cond.condition)?;
                write!(f, "\n    {}", cond.case_true)?;
                write!(f, "\n    {}", cond.case_false)?;
                write!(f, ")")
            }

            Function::Definition(data) => {
                let data = data.borrow();
                write!(f, "(def")?;

                if !data.locals.is_empty() {
                    write!(f, " [local:")?;
                    for a in &data.locals {
                        write!(f, " {}", a)?;
                    }
                    write!(f, "]")?;
                }

                write!(f, "\n  (")?;
                fmt_fn_name(f, self)?;
                for a in &data.args {
                    write!(f, " {}", Function::Arg(a.clone()))?;
                }
                write!(f, ") {}", data.body)?;

                write!(f, ")")
            }

            Function::FunctionRef(data) => {
                write!(f, "#")?;
                fmt_fn_name(f, &Function::Definition(data.clone()))
            }

            Function::Literal(data) => write!(f, "{}", data.borrow().value),
            // Function::Local(data) => write!(f, "$[loc:{}]", data.borrow().index),

            Function::Match(data) => {
                let data = data.borrow();
                write!(f, "\n match")?;

                if data.tail_call.is_some() {
                    write!(f, "[t]")?;
                }

                for a in &data.args {
                    write!(f, " {}", Function::Arg(a.clone()))?;
                }
                for (pat, body) in &data.cases {
                    write!(f, "\n")?;
                    for p in pat {
                        write!(f, "  {}", p)?;
                    }
                    write!(f, " => {}", body)?;
                }
                write!(f, ")")
            }

            Function::Seq(data) => {
                write!(f, "(seq")?;

                let data = data.borrow();
                for arg in &data.args {
                    write!(f, "\n    {}", arg)?;
                }

                write!(f, ")")
            }

            Function::Unknown(ukn) => write!(f, "{{{}}}", ukn.name),

            // _ => write!(f, "{:?}", self)
        }
    }
}


impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Bool(v)      => write!(f, "{}", if *v { "True" } else { "False" }),
            Value::Int(i)       => write!(f, "{}", i),
            Value::Float(i)     => write!(f, "{}", i),
            Value::Str(s)       => write!(f, "{}", s),
            Value::Deferred(s)  => write!(f, "'{:?}", s),
            Value::List(list)   => {
                write!(f, "[")?;
                for (i, el) in list.iter().enumerate() {
                    if i != 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", el)?;
                }
                write!(f, "]")
            },
            _ => write!(f, "{:?}", self)
        }
    }
}
