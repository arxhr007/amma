use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead, Write};
use crate::parser::{Expr, Stmt};

pub type Environment = HashMap<String, Value>;
pub type Functions = HashMap<String, (Vec<String>, Vec<Stmt>, Option<Expr>)>;

#[derive(Debug, Clone)]
pub enum Value {
    Int(i32),
    Str(String),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{}", i),
            Value::Str(s) => write!(f, "{}", s),
        }
    }
}

fn to_int(val: &Value) -> i32 {
    match val {
        Value::Int(i) => *i,
        Value::Str(s) => s.parse::<i32>().unwrap_or(0),
    }
}

pub fn evaluate(expr: &Expr, env: &Environment, funcs: &Functions) -> Result<Value, String> {
    match expr {
        Expr::Int(i) => Ok(Value::Int(*i)),

        Expr::Str(s) => Ok(Value::Str(s.clone())),

        Expr::Call(name, args) => {
            if let Some((params, body, ret)) = funcs.get(name) {
                let mut local_env = env.clone();
                let mut evaluated_args = Vec::new();

                for a in args {
                    evaluated_args.push(evaluate(a, env, funcs)?);
                }

                for (p, a) in params.iter().zip(evaluated_args.iter()) {
                    local_env.insert(p.clone(), a.clone());
                }

                let mut funcs_mut = funcs.clone();

                for stmt in body {
                    if let Some(result) = run_stmt(stmt, &mut local_env, &mut funcs_mut)? {
                        return Ok(result);
                    }
                }

                if let Some(ret_expr) = ret {
                    return evaluate(ret_expr, &local_env, funcs);
                }

                Ok(Value::Int(0))
            } else {
                Err(format!("Undefined function: {}", name))
            }
        },

        Expr::Index(container, idx) => {
            let container_val = evaluate(container, env, funcs)?;
            let idx_val = evaluate(idx, env, funcs)?;

            let idx_int = match idx_val {
                Value::Int(i) => i,
                Value::Str(s) => s.parse::<i32>().unwrap_or_else(|_| {
                    panic!("Index must be an integer")
                }),
            };

            match container_val {
                Value::Str(s) => {
                    if idx_int < 0 || idx_int >= s.chars().count() as i32 {
                        Err(format!("String index out of range: {}", idx_int))
                    } else {
                        let char = s.chars().nth(idx_int as usize).unwrap();
                        Ok(Value::Str(char.to_string()))
                    }
                },
                _ => Err(format!("Cannot index non-string value")),
            }
        },

        Expr::Substring(var_expr, start, end) => {
            let string = evaluate(var_expr, env, funcs)?;
            let start_val = evaluate(start, env, funcs)?;
            let end_val = evaluate(end, env, funcs)?;

            match string {
                Value::Str(s) => {
                    let start_idx = match start_val {
                        Value::Int(i) => i as usize,
                        Value::Str(s) => s.parse::<usize>().unwrap_or_else(|_| {
                            panic!("Substring indices must be integers")
                        }),
                    };

                    let end_idx = match end_val {
                        Value::Int(i) => i as usize,
                        Value::Str(s) => s.parse::<usize>().unwrap_or_else(|_| {
                            panic!("Substring indices must be integers")
                        }),
                    };

                    let chars: Vec<char> = s.chars().collect();
                    if start_idx <= end_idx && end_idx <= chars.len() {
                        let substring: String = chars[start_idx..end_idx].iter().collect();
                        Ok(Value::Str(substring))
                    } else {
                        Err(format!("Invalid substring indices: {}:{}", start_idx, end_idx))
                    }
                },
                _ => Err(format!("Cannot get substring of non-string value")),
            }
        },

        Expr::Binary(op, left, right) => {
            let left_val = evaluate(left, env, funcs)?;
            let right_val = evaluate(right, env, funcs)?;

            match op.as_str() {
                "+" => {
                    match (&left_val, &right_val) {
                        (Value::Str(_), _) | (_, Value::Str(_)) => {
                            Ok(Value::Str(format!("{}{}", left_val, right_val)))
                        },
                        _ => {
                            Ok(Value::Int(to_int(&left_val) + to_int(&right_val)))
                        }
                    }
                },

                "-" => Ok(Value::Int(to_int(&left_val) - to_int(&right_val))),

                "*" => {
                    match (&left_val, &right_val) {
                        (Value::Str(s), Value::Int(i)) => {
                            Ok(Value::Str(s.repeat(*i as usize)))
                        },
                        (Value::Int(i), Value::Str(s)) => {
                            Ok(Value::Str(s.repeat(*i as usize)))
                        },
                        _ => {
                            Ok(Value::Int(to_int(&left_val) * to_int(&right_val)))
                        }
                    }
                },

                "/" => {
                    let right_int = to_int(&right_val);
                    if right_int == 0 {
                        Err("Division by zero".to_string())
                    } else {
                        Ok(Value::Int(to_int(&left_val) / right_int))
                    }
                },

                "%" => {
                    let right_int = to_int(&right_val);
                    if right_int == 0 {
                        Err("Modulo by zero".to_string())
                    } else {
                        Ok(Value::Int(to_int(&left_val) % right_int))
                    }
                },

                "&&" => {
                    Ok(Value::Int(if to_int(&left_val) != 0 && to_int(&right_val) != 0 { 1 } else { 0 }))
                },

                "||" => {
                    Ok(Value::Int(if to_int(&left_val) != 0 || to_int(&right_val) != 0 { 1 } else { 0 }))
                },

                "==" => {
                    match (&left_val, &right_val) {
                        (Value::Int(l), Value::Int(r)) => Ok(Value::Int(if l == r { 1 } else { 0 })),
                        (Value::Str(l), Value::Str(r)) => Ok(Value::Int(if l == r { 1 } else { 0 })),
                        _ => Ok(Value::Int(0)),
                    }
                },

                "!=" => {
                    match (&left_val, &right_val) {
                        (Value::Int(l), Value::Int(r)) => Ok(Value::Int(if l != r { 1 } else { 0 })),
                        (Value::Str(l), Value::Str(r)) => Ok(Value::Int(if l != r { 1 } else { 0 })),
                        _ => Ok(Value::Int(1)),
                    }
                },

                "<" | ">" | "<=" | ">=" => {
                    match (&left_val, &right_val) {
                        (Value::Str(l), Value::Str(r)) => {
                            let result = match op.as_str() {
                                "<" => l < r,
                                ">" => l > r,
                                "<=" => l <= r,
                                ">=" => l >= r,
                                _ => unreachable!(),
                            };
                            Ok(Value::Int(if result { 1 } else { 0 }))
                        },
                        _ => {
                            let l_int = to_int(&left_val);
                            let r_int = to_int(&right_val);
                            let result = match op.as_str() {
                                "<" => l_int < r_int,
                                ">" => l_int > r_int,
                                "<=" => l_int <= r_int,
                                ">=" => l_int >= r_int,
                                _ => unreachable!(),
                            };
                            Ok(Value::Int(if result { 1 } else { 0 }))
                        }
                    }
                },

                _ => Err(format!("Unknown operator: {}", op)),
            }
        },

        Expr::Var(name) => {
            match env.get(name) {
                Some(val) => Ok(val.clone()),
                None => Ok(Value::Int(0)),
            }
        },
    }
}

pub fn run_stmt(stmt: &Stmt, env: &mut Environment, funcs: &mut Functions) -> Result<Option<Value>, String> {
    match stmt {
        Stmt::Print(expr) => {
            let val = evaluate(expr, env, funcs)?;
            println!("{}", val);
            Ok(None)
        },

        Stmt::Input(varname) => {
            print!("{} = ", varname);
            io::stdout().flush().unwrap();

            let mut input = String::new();
            io::stdin().lock().read_line(&mut input).unwrap();
            input = input.trim().to_string();

            match input.parse::<i32>() {
                Ok(val_int) => {
                    env.insert(varname.clone(), Value::Int(val_int));
                },
                Err(_) => {
                    env.insert(varname.clone(), Value::Str(input));
                },
            }

            Ok(None)
        },

        Stmt::Assign(name, expr) => {
            let val = evaluate(expr, env, funcs)?;
            env.insert(name.clone(), val);
            Ok(None)
        },

        Stmt::FileRead(name, filename_expr) => {
            let filename = match evaluate(filename_expr, env, funcs)? {
                Value::Str(s) => s,
                Value::Int(i) => i.to_string(),
            };

            match std::fs::read_to_string(&filename) {
                Ok(content) => {
                    env.insert(name.clone(), Value::Str(content));
                    Ok(None)
                },
                Err(_) => {
                    println!("❌ File not found: {}", filename);
                    env.insert(name.clone(), Value::Str(String::new()));
                    Ok(None)
                },
            }
        },

        Stmt::FileWrite(content_expr, filename_expr) => {
            let content = match evaluate(content_expr, env, funcs)? {
                Value::Str(s) => s,
                Value::Int(i) => i.to_string(),
            };

            let filename = match evaluate(filename_expr, env, funcs)? {
                Value::Str(s) => s,
                Value::Int(i) => i.to_string(),
            };

            match File::create(&filename) {
                Ok(mut file) => {
                    file.write_all(content.as_bytes()).unwrap();
                    Ok(None)
                },
                Err(e) => Err(format!("Failed to write to file: {}", e)),
            }
        },

        Stmt::FileAppend(content_expr, filename_expr) => {
            let content = match evaluate(content_expr, env, funcs)? {
                Value::Str(s) => s,
                Value::Int(i) => i.to_string(),
            };

            let filename = match evaluate(filename_expr, env, funcs)? {
                Value::Str(s) => s,
                Value::Int(i) => i.to_string(),
            };

            match std::fs::OpenOptions::new().append(true).create(true).open(&filename) {
                Ok(mut file) => {
                    file.write_all(content.as_bytes()).unwrap();
                    Ok(None)
                },
                Err(e) => Err(format!("Failed to append to file: {}", e)),
            }
        },

        Stmt::For(start_expr, end_expr, body) => {
            let start = to_int(&evaluate(start_expr, env, funcs)?);
            let end = to_int(&evaluate(end_expr, env, funcs)?);

            for i in start..=end {
                env.insert("_".to_string(), Value::Int(i));

                for stmt in body {
                    if let Some(result) = run_stmt(stmt, env, funcs)? {
                        return Ok(Some(result));
                    }
                }
            }

            Ok(None)
        },

        Stmt::While(cond, body) => {
            while to_int(&evaluate(cond, env, funcs)?) != 0 {
                for stmt in body {
                    if let Some(result) = run_stmt(stmt, env, funcs)? {
                        return Ok(Some(result));
                    }
                }
            }

            Ok(None)
        },

        Stmt::If(cond, then_body, else_body) => {
            if to_int(&evaluate(cond, env, funcs)?) != 0 {
                for stmt in then_body {
                    if let Some(result) = run_stmt(stmt, env, funcs)? {
                        return Ok(Some(result));
                    }
                }
            } else {
                for stmt in else_body {
                    if let Some(result) = run_stmt(stmt, env, funcs)? {
                        return Ok(Some(result));
                    }
                }
            }

            Ok(None)
        },

        Stmt::FuncDef(name, params, body, ret_expr) => {
            // Here's the fix: Store the return expression properly
            funcs.insert(name.clone(), (params.clone(), body.clone(), ret_expr.clone()));
            Ok(None)
        },

        Stmt::Expr(expr) => {
            evaluate(expr, env, funcs)?;
            Ok(None)
        },

        Stmt::Return(expr) => {
            let val = evaluate(expr, env, funcs)?;
            Ok(Some(val))
        },
    }
}

pub fn run(statements: &[Stmt], env: &mut Environment, funcs: &mut Functions) -> Option<Value> {
    for stmt in statements {
        match run_stmt(stmt, env, funcs) {
            Ok(Some(val)) => return Some(val),
            Ok(None) => {},
            Err(e) => {
                eprintln!("Runtime error: {}", e);
                return None;
            }
        }
    }
    None
}