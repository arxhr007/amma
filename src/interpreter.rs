use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead, Write};
use crate::parser::{Expr, Stmt};

pub type Environment = HashMap<String, Value>;
pub type Functions = HashMap<String, (Vec<String>, Vec<Stmt>)>;

#[derive(Debug, Clone)]
pub enum Value {
    Int(i32),
    Float(f64),
    Bool(bool),
    Str(String),
    List(Vec<Value>),
    Null,
}

/// Control-flow signal produced by executing a statement. `Normal` means "fall
/// through"; the others unwind to the nearest construct that handles them:
/// `Return` to the enclosing function/program, `Break`/`Continue` to the
/// nearest loop.
#[derive(Debug, Clone)]
pub enum Flow {
    Normal,
    Return(Value),
    Break,
    Continue,
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(x) => write!(f, "{}", x),
            Value::Bool(b) => write!(f, "{}", if *b { "ശരി" } else { "തെറ്റ്" }),
            Value::Str(s) => write!(f, "{}", s),
            Value::Null => write!(f, "ശൂന്യം"),
            Value::List(items) => {
                let parts: Vec<String> = items.iter().map(|v| v.to_string()).collect();
                write!(f, "[{}]", parts.join(", "))
            }
        }
    }
}

/// Restore a variable to a previous value, or remove it if it had none.
fn restore_var(env: &mut Environment, name: &str, prev: Option<Value>) {
    match prev {
        Some(v) => { env.insert(name.to_string(), v); }
        None => { env.remove(name); }
    }
}

fn is_float(v: &Value) -> bool {
    matches!(v, Value::Float(_))
}

fn to_int(val: &Value) -> i32 {
    match val {
        Value::Int(i) => *i,
        Value::Float(f) => *f as i32,
        Value::Bool(b) => if *b { 1 } else { 0 },
        Value::Str(s) => s.trim().parse::<i32>().unwrap_or(0),
        _ => 0,
    }
}

fn to_float(val: &Value) -> f64 {
    match val {
        Value::Int(i) => *i as f64,
        Value::Float(f) => *f,
        Value::Bool(b) => if *b { 1.0 } else { 0.0 },
        Value::Str(s) => s.trim().parse::<f64>().unwrap_or(0.0),
        _ => 0.0,
    }
}

/// Truthiness used by `if`/`while`/`&&`/`||`/`!`.
fn is_truthy(val: &Value) -> bool {
    match val {
        Value::Bool(b) => *b,
        Value::Int(i) => *i != 0,
        Value::Float(f) => *f != 0.0,
        Value::Str(s) => !s.is_empty(),
        Value::List(l) => !l.is_empty(),
        Value::Null => false,
    }
}

fn values_equal(l: &Value, r: &Value) -> bool {
    match (l, r) {
        (Value::Null, Value::Null) => true,
        (Value::Null, _) | (_, Value::Null) => false,
        (Value::Str(a), Value::Str(b)) => a == b,
        (Value::List(a), Value::List(b)) => {
            a.len() == b.len() && a.iter().zip(b).all(|(x, y)| values_equal(x, y))
        }
        (Value::Str(_), _) | (_, Value::Str(_)) => false,
        (Value::List(_), _) | (_, Value::List(_)) => false,
        // numeric / bool — compare on a common float footing
        _ => to_float(l) == to_float(r),
    }
}

fn compare(op: &str, l: &Value, r: &Value) -> bool {
    if let (Value::Str(a), Value::Str(b)) = (l, r) {
        match op {
            "<" => a < b,
            ">" => a > b,
            "<=" => a <= b,
            ">=" => a >= b,
            _ => false,
        }
    } else {
        let (a, b) = (to_float(l), to_float(r));
        match op {
            "<" => a < b,
            ">" => a > b,
            "<=" => a <= b,
            ">=" => a >= b,
            _ => false,
        }
    }
}

// ---------------------------------------------------------------------------
// Builtin / standard-library functions
// ---------------------------------------------------------------------------

/// Cheap dependency-free RNG (xorshift) seeded once per thread from the clock.
fn rng_next() -> u64 {
    use std::cell::Cell;
    use std::time::{SystemTime, UNIX_EPOCH};
    thread_local!(static SEED: Cell<u64> = Cell::new(
        SystemTime::now().duration_since(UNIX_EPOCH).map(|d| d.as_nanos() as u64).unwrap_or(0x9E3779B9) | 1
    ));
    SEED.with(|s| {
        let mut x = s.get();
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        s.set(x);
        x
    })
}

fn need(args: &[Value], n: usize, name: &str) -> Result<(), String> {
    if args.len() == n {
        Ok(())
    } else {
        Err(format!("{} expects {} argument(s), got {}", name, n, args.len()))
    }
}

/// Dispatched before user functions in `Expr::Call`. Returns `None` when `name`
/// is not a builtin so the caller falls through to user-defined functions.
fn call_builtin(name: &str, args: &[Value]) -> Option<Result<Value, String>> {
    Some(match name {
        // --- math ---
        "കേവലം" => need(args, 1, "കേവലം").and_then(|_| Ok(match &args[0] {
            Value::Float(f) => Value::Float(f.abs()),
            other => Value::Int(to_int(other).abs()),
        })),
        "ഘാതം" => need(args, 2, "ഘാതം").and_then(|_| {
            if let (Value::Int(b), Value::Int(e)) = (&args[0], &args[1]) {
                if *e >= 0 {
                    return Ok(Value::Int(b.pow(*e as u32)));
                }
            }
            Ok(Value::Float(to_float(&args[0]).powf(to_float(&args[1]))))
        }),
        "വർഗമൂലം" => need(args, 1, "വർഗമൂലം").map(|_| Value::Float(to_float(&args[0]).sqrt())),
        "കുറഞ്ഞത്" => Some(fold_extreme(args, true)).unwrap(),
        "കൂടിയത്" => Some(fold_extreme(args, false)).unwrap(),
        "ക്രമരഹിതം" => need(args, 1, "ക്രമരഹിതം").map(|_| {
            let n = to_int(&args[0]);
            if n <= 0 { Value::Int(0) } else { Value::Int((rng_next() % n as u64) as i32) }
        }),

        // --- type conversion ---
        "സംഖ്യ" => need(args, 1, "സംഖ്യ").map(|_| Value::Int(to_int(&args[0]))),
        "ദശാംശം" => need(args, 1, "ദശാംശം").map(|_| Value::Float(to_float(&args[0]))),
        "വാചകം" => need(args, 1, "വാചകം").map(|_| Value::Str(args[0].to_string())),

        // --- string ops ---
        "വലുത്" => need(args, 1, "വലുത്").map(|_| Value::Str(args[0].to_string().to_uppercase())),
        "ചെറുത്" => need(args, 1, "ചെറുത്").map(|_| Value::Str(args[0].to_string().to_lowercase())),
        "ഒതുക്കുക" => need(args, 1, "ഒതുക്കുക").map(|_| Value::Str(args[0].to_string().trim().to_string())),
        "വിഭജിക്കുക" => need(args, 2, "വിഭജിക്കുക").map(|_| {
            let s = args[0].to_string();
            let sep = args[1].to_string();
            let parts: Vec<Value> = if sep.is_empty() {
                s.chars().map(|c| Value::Str(c.to_string())).collect()
            } else {
                s.split(&sep).map(|p| Value::Str(p.to_string())).collect()
            };
            Value::List(parts)
        }),
        "മാറ്റുക" => need(args, 3, "മാറ്റുക").map(|_| {
            Value::Str(args[0].to_string().replace(&args[1].to_string(), &args[2].to_string()))
        }),
        "ഉണ്ടോ" => need(args, 2, "ഉണ്ടോ").map(|_| match &args[0] {
            Value::List(l) => Value::Bool(l.iter().any(|x| values_equal(x, &args[1]))),
            other => Value::Bool(other.to_string().contains(&args[1].to_string())),
        }),

        // --- list ops (return new lists; values are immutable) ---
        "ചേർക്കുക" => need(args, 2, "ചേർക്കുക").and_then(|_| match &args[0] {
            Value::List(l) => {
                let mut out = l.clone();
                out.push(args[1].clone());
                Ok(Value::List(out))
            }
            _ => Err("ചേർക്കുക expects a list as its first argument".to_string()),
        }),
        "നീക്കുക" => need(args, 2, "നീക്കുക").and_then(|_| match &args[0] {
            Value::List(l) => {
                let i = to_int(&args[1]);
                if i < 0 || i >= l.len() as i32 {
                    Err(format!("List index out of range: {}", i))
                } else {
                    let mut out = l.clone();
                    out.remove(i as usize);
                    Ok(Value::List(out))
                }
            }
            _ => Err("നീക്കുക expects a list as its first argument".to_string()),
        }),

        _ => return None,
    })
}

/// min/max over either an argument list or a single list argument.
fn fold_extreme(args: &[Value], want_min: bool) -> Result<Value, String> {
    let items: Vec<Value> = match args {
        [Value::List(l)] => l.clone(),
        [] => return Err("കുറഞ്ഞത്/കൂടിയത് expects at least one argument".to_string()),
        rest => rest.to_vec(),
    };
    if items.is_empty() {
        return Err("കുറഞ്ഞത്/കൂടിയത് expects a non-empty list".to_string());
    }
    let mut best = items[0].clone();
    for v in items.iter().skip(1) {
        let take = if want_min { to_float(v) < to_float(&best) } else { to_float(v) > to_float(&best) };
        if take {
            best = v.clone();
        }
    }
    Ok(best)
}

pub fn evaluate(expr: &Expr, env: &Environment, funcs: &Functions) -> Result<Value, String> {
    match expr {
        Expr::Int(i) => Ok(Value::Int(*i)),

        Expr::Float(x) => Ok(Value::Float(*x)),

        Expr::Bool(b) => Ok(Value::Bool(*b)),

        Expr::Null => Ok(Value::Null),

        Expr::Str(s) => Ok(Value::Str(s.clone())),

        Expr::ListLiteral(items) => {
            let mut out = Vec::with_capacity(items.len());
            for it in items {
                out.push(evaluate(it, env, funcs)?);
            }
            Ok(Value::List(out))
        }

        Expr::Not(inner) => {
            let v = evaluate(inner, env, funcs)?;
            Ok(Value::Bool(!is_truthy(&v)))
        }

        Expr::Call(name, args) => {
            // Evaluate arguments in the caller's scope first.
            let mut evaluated_args = Vec::with_capacity(args.len());
            for a in args {
                evaluated_args.push(evaluate(a, env, funcs)?);
            }

            // Builtins win over user functions (cannot be shadowed).
            if let Some(result) = call_builtin(name, &evaluated_args) {
                return result;
            }

            match funcs.get(name) {
                Some((params, body)) => {
                    // Fresh scope holding only parameters — no caller-local leak
                    // (D1) and no full-env clone (P2). `funcs` is shared by ref.
                    let mut local_env: Environment = HashMap::new();
                    for (p, a) in params.iter().zip(evaluated_args.iter()) {
                        local_env.insert(p.clone(), a.clone());
                    }

                    for stmt in body {
                        if let Flow::Return(result) = run_stmt(stmt, &mut local_env, funcs)? {
                            return Ok(result);
                        }
                    }

                    Ok(Value::Int(0))
                }
                None => Err(format!("Undefined function: {}", name)),
            }
        }

        Expr::Index(container, idx) => {
            let container_val = evaluate(container, env, funcs)?;
            let idx_val = evaluate(idx, env, funcs)?;

            let idx_int = match &idx_val {
                Value::Int(i) => *i,
                Value::Float(f) => *f as i32,
                Value::Str(s) => s.parse::<i32>()
                    .map_err(|_| format!("Index must be an integer, got \"{}\"", s))?,
                _ => return Err("Index must be an integer".to_string()),
            };

            match container_val {
                Value::Str(s) => {
                    if idx_int < 0 || idx_int >= s.chars().count() as i32 {
                        Err(format!("String index out of range: {}", idx_int))
                    } else {
                        let ch = s.chars().nth(idx_int as usize).unwrap();
                        Ok(Value::Str(ch.to_string()))
                    }
                }
                Value::List(l) => {
                    if idx_int < 0 || idx_int >= l.len() as i32 {
                        Err(format!("List index out of range: {}", idx_int))
                    } else {
                        Ok(l[idx_int as usize].clone())
                    }
                }
                _ => Err("Cannot index this value".to_string()),
            }
        }

        Expr::Substring(var_expr, start, end) => {
            let s = match evaluate(var_expr, env, funcs)? {
                Value::Str(s) => s,
                _ => return Err("Cannot get substring of non-string value".to_string()),
            };
            let start_i = to_int(&evaluate(start, env, funcs)?);
            let end_i = to_int(&evaluate(end, env, funcs)?);

            if start_i < 0 || end_i < 0 {
                return Err(format!("Invalid substring indices: {}:{}", start_i, end_i));
            }
            let (start_idx, end_idx) = (start_i as usize, end_i as usize);
            let chars: Vec<char> = s.chars().collect();
            if start_idx <= end_idx && end_idx <= chars.len() {
                Ok(Value::Str(chars[start_idx..end_idx].iter().collect()))
            } else {
                Err(format!("Invalid substring indices: {}:{}", start_idx, end_idx))
            }
        }

        Expr::Length(inner) => {
            let val = evaluate(inner, env, funcs)?;
            match val {
                Value::Str(s) => Ok(Value::Int(s.chars().count() as i32)),
                Value::List(l) => Ok(Value::Int(l.len() as i32)),
                Value::Int(i) => Ok(Value::Int(i.to_string().len() as i32)),
                other => Ok(Value::Int(other.to_string().chars().count() as i32)),
            }
        }

        Expr::Binary(op, left, right) => {
            let l = evaluate(left, env, funcs)?;
            let r = evaluate(right, env, funcs)?;

            match op.as_str() {
                "+" => match (&l, &r) {
                    (Value::Str(_), _) | (_, Value::Str(_)) => Ok(Value::Str(format!("{}{}", l, r))),
                    _ if is_float(&l) || is_float(&r) => Ok(Value::Float(to_float(&l) + to_float(&r))),
                    _ => Ok(Value::Int(to_int(&l) + to_int(&r))),
                },

                "-" => {
                    if is_float(&l) || is_float(&r) {
                        Ok(Value::Float(to_float(&l) - to_float(&r)))
                    } else {
                        Ok(Value::Int(to_int(&l) - to_int(&r)))
                    }
                }

                "*" => match (&l, &r) {
                    (Value::Str(s), Value::Int(i)) | (Value::Int(i), Value::Str(s)) => {
                        Ok(Value::Str(s.repeat(*i as usize)))
                    }
                    _ if is_float(&l) || is_float(&r) => Ok(Value::Float(to_float(&l) * to_float(&r))),
                    _ => Ok(Value::Int(to_int(&l) * to_int(&r))),
                },

                "/" => {
                    if is_float(&l) || is_float(&r) {
                        let d = to_float(&r);
                        if d == 0.0 { Err("Division by zero".to_string()) }
                        else { Ok(Value::Float(to_float(&l) / d)) }
                    } else {
                        let d = to_int(&r);
                        if d == 0 { Err("Division by zero".to_string()) }
                        else { Ok(Value::Int(to_int(&l) / d)) }
                    }
                }

                "%" => {
                    let d = to_int(&r);
                    if d == 0 { Err("Modulo by zero".to_string()) }
                    else { Ok(Value::Int(to_int(&l) % d)) }
                }

                "&&" => Ok(Value::Bool(is_truthy(&l) && is_truthy(&r))),
                "||" => Ok(Value::Bool(is_truthy(&l) || is_truthy(&r))),

                "==" => Ok(Value::Bool(values_equal(&l, &r))),
                "!=" => Ok(Value::Bool(!values_equal(&l, &r))),

                "<" | ">" | "<=" | ">=" => Ok(Value::Bool(compare(op, &l, &r))),

                _ => Err(format!("Unknown operator: {}", op)),
            }
        }

        Expr::Var(name) => {
            match env.get(name) {
                Some(val) => Ok(val.clone()),
                None => Ok(Value::Int(0)),
            }
        }
    }
}

/// Run a block, stopping at the first non-`Normal` flow signal and returning it.
fn run_block(body: &[Stmt], env: &mut Environment, funcs: &Functions) -> Result<Flow, String> {
    for stmt in body {
        match run_stmt(stmt, env, funcs)? {
            Flow::Normal => {}
            other => return Ok(other),
        }
    }
    Ok(Flow::Normal)
}

pub fn run_stmt(stmt: &Stmt, env: &mut Environment, funcs: &Functions) -> Result<Flow, String> {
    match stmt {
        Stmt::Print(expr) => {
            let val = evaluate(expr, env, funcs)?;
            println!("{}", val);
            Ok(Flow::Normal)
        }

        Stmt::Input(varname) => {
            print!("{} = ", varname);
            io::stdout().flush().unwrap();

            let mut input = String::new();
            io::stdin().lock().read_line(&mut input).unwrap();
            input = input.trim().to_string();

            match input.parse::<i32>() {
                Ok(val_int) => { env.insert(varname.clone(), Value::Int(val_int)); }
                Err(_) => { env.insert(varname.clone(), Value::Str(input)); }
            }

            Ok(Flow::Normal)
        }

        Stmt::Assign(name, expr) => {
            let val = evaluate(expr, env, funcs)?;
            env.insert(name.clone(), val);
            Ok(Flow::Normal)
        }

        Stmt::FileRead(name, filename_expr) => {
            let filename = evaluate(filename_expr, env, funcs)?.to_string();
            match std::fs::read_to_string(&filename) {
                Ok(content) => { env.insert(name.clone(), Value::Str(content)); }
                Err(_) => {
                    println!("File not found: {}", filename);
                    env.insert(name.clone(), Value::Str(String::new()));
                }
            }
            Ok(Flow::Normal)
        }

        Stmt::FileWrite(content_expr, filename_expr) => {
            let content = evaluate(content_expr, env, funcs)?.to_string();
            let filename = evaluate(filename_expr, env, funcs)?.to_string();
            match File::create(&filename) {
                Ok(mut file) => {
                    file.write_all(content.as_bytes()).unwrap();
                    Ok(Flow::Normal)
                }
                Err(e) => Err(format!("Failed to write to file: {}", e)),
            }
        }

        Stmt::FileAppend(content_expr, filename_expr) => {
            let content = evaluate(content_expr, env, funcs)?.to_string();
            let filename = evaluate(filename_expr, env, funcs)?.to_string();
            match std::fs::OpenOptions::new().append(true).create(true).open(&filename) {
                Ok(mut file) => {
                    file.write_all(content.as_bytes()).unwrap();
                    Ok(Flow::Normal)
                }
                Err(e) => Err(format!("Failed to append to file: {}", e)),
            }
        }

        Stmt::For(start_expr, end_expr, body) => {
            let start = to_int(&evaluate(start_expr, env, funcs)?);
            let end = to_int(&evaluate(end_expr, env, funcs)?);

            // Save any outer `_` so nested loops don't clobber each other (B1).
            let prev = env.get("_").cloned();

            for i in start..=end {
                env.insert("_".to_string(), Value::Int(i));
                match run_block(body, env, funcs)? {
                    Flow::Normal | Flow::Continue => {}
                    Flow::Break => break,
                    Flow::Return(v) => {
                        restore_var(env, "_", prev);
                        return Ok(Flow::Return(v));
                    }
                }
            }

            restore_var(env, "_", prev);
            Ok(Flow::Normal)
        }

        Stmt::ForEach(var, iterable, body) => {
            let coll = evaluate(iterable, env, funcs)?;
            let items: Vec<Value> = match coll {
                Value::List(l) => l,
                Value::Str(s) => s.chars().map(|c| Value::Str(c.to_string())).collect(),
                _ => return Err("Cannot iterate over a non-list / non-string value".to_string()),
            };

            let prev = env.get(var).cloned();

            for item in items {
                env.insert(var.clone(), item);
                match run_block(body, env, funcs)? {
                    Flow::Normal | Flow::Continue => {}
                    Flow::Break => break,
                    Flow::Return(v) => {
                        restore_var(env, var, prev);
                        return Ok(Flow::Return(v));
                    }
                }
            }

            restore_var(env, var, prev);
            Ok(Flow::Normal)
        }

        Stmt::While(cond, body) => {
            while is_truthy(&evaluate(cond, env, funcs)?) {
                match run_block(body, env, funcs)? {
                    Flow::Normal | Flow::Continue => {}
                    Flow::Break => break,
                    Flow::Return(v) => return Ok(Flow::Return(v)),
                }
            }
            Ok(Flow::Normal)
        }

        Stmt::If(cond, then_body, else_body) => {
            if is_truthy(&evaluate(cond, env, funcs)?) {
                run_block(then_body, env, funcs)
            } else {
                run_block(else_body, env, funcs)
            }
        }

        Stmt::FuncDef(..) => {
            // Definitions are hoisted in `run` before execution (B5).
            Ok(Flow::Normal)
        }

        Stmt::Expr(expr) => {
            evaluate(expr, env, funcs)?;
            Ok(Flow::Normal)
        }

        Stmt::Return(expr) => {
            let val = evaluate(expr, env, funcs)?;
            Ok(Flow::Return(val))
        }

        Stmt::Break => Ok(Flow::Break),
        Stmt::Continue => Ok(Flow::Continue),
    }
}

/// Hoist every top-level function definition so calls can reference functions
/// defined later in the file, and mutual recursion works (B5).
fn hoist(statements: &[Stmt], funcs: &mut Functions) {
    for stmt in statements {
        if let Stmt::FuncDef(name, params, body) = stmt {
            funcs.insert(name.clone(), (params.clone(), body.clone()));
        }
    }
}

pub fn run(statements: &[Stmt], env: &mut Environment, funcs: &mut Functions) -> Option<Value> {
    hoist(statements, funcs);

    for stmt in statements {
        match run_stmt(stmt, env, &*funcs) {
            Ok(Flow::Return(val)) => return Some(val),
            Ok(_) => {}
            Err(e) => {
                eprintln!("Runtime error: {}", e);
                return None;
            }
        }
    }
    None
}

/// REPL variant: persists `env`/`funcs` across lines and echoes the value of
/// bare expression statements so the prompt is interactive.
pub fn run_repl(statements: &[Stmt], env: &mut Environment, funcs: &mut Functions) {
    hoist(statements, funcs);

    for stmt in statements {
        match stmt {
            Stmt::FuncDef(..) => {}
            Stmt::Expr(expr) => match evaluate(expr, env, &*funcs) {
                Ok(val) => println!("{}", val),
                Err(e) => { eprintln!("Runtime error: {}", e); return; }
            },
            other => match run_stmt(other, env, &*funcs) {
                Ok(_) => {}
                Err(e) => { eprintln!("Runtime error: {}", e); return; }
            },
        }
    }
}
