mod lexer;
mod parser;
mod interpreter;
mod transliterator;

use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() > 1 && args[1].ends_with(".amma") {
        let path = &args[1];
        let code = fs::read_to_string(path).expect("Could not read file");
        // Strip comments before transliteration so a stray quote in a comment
        // can't corrupt the transliterator's string-protection regex (L3).
        let code = lexer::strip_comments(&code);
        let code = transliterator::transliterate_source(&code);
        let tokens = lexer::tokenize(&code);
        let mut parser = parser::Parser::new(tokens);
        let ast = match parser.parse() {
            Ok(ast) => ast,
            Err(e) => {
                eprintln!("Parse error: {}", e);
                std::process::exit(1);
            }
        };
        let mut env = std::collections::HashMap::new();
        let mut funcs = std::collections::HashMap::new();
        interpreter::run(&ast, &mut env, &mut funcs);
    } else {
        println!("Usage: ./amma <script.amma> (linux)");
        println!("Usage: .\\amma <script.amma> (windows)");
        
    }
}
