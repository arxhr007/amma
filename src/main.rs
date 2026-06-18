mod lexer;
mod parser;
mod interpreter;
mod transliterator;

use std::collections::HashSet;
use std::env;
use std::fs;
use std::io::{self, BufRead, Write};
use regex::Regex;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.iter().any(|a| a == "--version" || a == "-V") {
        println!("amma {}", env!("CARGO_PKG_VERSION"));
        return;
    }
    if args.iter().any(|a| a == "--help" || a == "-h") {
        print_help();
        return;
    }

    let dump_tokens = args.iter().any(|a| a == "--tokens");
    let dump_ast = args.iter().any(|a| a == "--ast");

    match args.iter().skip(1).find(|a| a.ends_with(".amma")) {
        Some(path) => run_file(path, dump_tokens, dump_ast),
        None => repl(),
    }
}

fn print_help() {
    println!("Amma — Malayalam programming language / അമ്മ — മലയാളം പ്രോഗ്രാമിംഗ് ഭാഷ");
    println!();
    println!("Usage:");
    println!("  amma <script.amma>      Run a program");
    println!("  amma                    Start the interactive REPL");
    println!();
    println!("Options:");
    println!("  -h, --help              Show this help");
    println!("  -V, --version           Show version");
    println!("      --tokens            Print the lexer token stream and run");
    println!("      --ast               Print the parsed AST and run");
}

/// Prepare source: strip comments, transliterate manglish → Malayalam, then
/// splice in any `ഉൾപ്പെടുത്തുക("file")` imports.
fn prepare(code: &str, visited: &mut HashSet<String>) -> String {
    let code = lexer::strip_comments(code);
    let code = transliterator::transliterate_source(&code);
    expand_imports(&code, visited)
}

/// Replace each `ഉൾപ്പെടുത്തുക("file.amma")` with the prepared contents of that
/// file. A visited set prevents infinite include cycles.
fn expand_imports(src: &str, visited: &mut HashSet<String>) -> String {
    let re = Regex::new(r#"ഉൾപ്പെടുത്തുക\s*\(\s*"([^"]*)"\s*\)"#).unwrap();

    let mut out = String::new();
    let mut last = 0;
    for caps in re.captures_iter(src) {
        let whole = caps.get(0).unwrap();
        let fname = caps.get(1).unwrap().as_str().to_string();

        out.push_str(&src[last..whole.start()]);

        if visited.insert(fname.clone()) {
            match fs::read_to_string(&fname) {
                Ok(content) => out.push_str(&prepare(&content, visited)),
                Err(_) => eprintln!("Import not found: {}", fname),
            }
        }

        last = whole.end();
    }
    out.push_str(&src[last..]);
    out
}

fn run_file(path: &str, dump_tokens: bool, dump_ast: bool) {
    let code = fs::read_to_string(path).expect("Could not read file");

    let mut visited = HashSet::new();
    visited.insert(path.to_string());
    let code = prepare(&code, &mut visited);

    let tokens = lexer::tokenize(&code);
    if dump_tokens {
        for (tok, line) in &tokens {
            println!("{}\t{}", line, tok);
        }
    }

    let mut parser = parser::Parser::new(tokens);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("Parse error: {}", e);
            std::process::exit(1);
        }
    };

    if dump_ast {
        println!("{:#?}", ast);
    }

    let mut env = std::collections::HashMap::new();
    let mut funcs = std::collections::HashMap::new();
    interpreter::run(&ast, &mut env, &mut funcs);
}

fn repl() {
    println!("Amma REPL — type code; Ctrl+D (Ctrl+Z on Windows) to exit");
    let mut env = std::collections::HashMap::new();
    let mut funcs = std::collections::HashMap::new();
    let stdin = io::stdin();

    loop {
        print!("amma> ");
        io::stdout().flush().ok();

        let mut line = String::new();
        if stdin.lock().read_line(&mut line).unwrap_or(0) == 0 {
            println!();
            break;
        }

        let mut visited = HashSet::new();
        let code = prepare(&line, &mut visited);
        let tokens = lexer::tokenize(&code);
        if tokens.is_empty() {
            continue;
        }

        let mut parser = parser::Parser::new(tokens);
        match parser.parse() {
            Ok(ast) => interpreter::run_repl(&ast, &mut env, &mut funcs),
            Err(e) => eprintln!("Parse error: {}", e),
        }
    }
}
