/// True if `tok` is a user identifier (ASCII letter/underscore start, then
/// alphanumeric/underscore). Replaces the old per-call `Regex` compilation and
/// also keeps Malayalam keyword tokens (matched separately by `==`) from ever
/// being treated as variable names.
fn is_ident(tok: &str) -> bool {
    let mut chars = tok.chars();
    match chars.next() {
        Some(c) if c.is_ascii_alphabetic() || c == '_' => {}
        _ => return false,
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

/// True if `tok` is a word token — an identifier starting with any (incl.
/// Malayalam) letter or `_`. Used for call-name detection so pure-Malayalam
/// builtin calls like `കേവലം(5)` are recognized, not just ASCII names.
fn is_word(tok: &str) -> bool {
    let mut chars = tok.chars();
    match chars.next() {
        Some(c) if c.is_alphabetic() || c == '_' => {}
        _ => return false,
    }
    chars.all(|c| c.is_alphanumeric() || c == '_')
}

/// True if `tok` is `=` or a compound-assignment operator.
fn is_assign_op(tok: &str) -> bool {
    matches!(tok, "=" | "+=" | "-=" | "*=" | "/=" | "%=")
}

/// Translate backslash escape sequences inside a string literal's body.
/// Supports `\n \t \r \\ \" \'`; an unknown escape keeps the char verbatim.
fn unescape(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => out.push('\n'),
                Some('t') => out.push('\t'),
                Some('r') => out.push('\r'),
                Some('\\') => out.push('\\'),
                Some('"') => out.push('"'),
                Some('\'') => out.push('\''),
                Some(other) => { out.push('\\'); out.push(other); }
                None => out.push('\\'),
            }
        } else {
            out.push(c);
        }
    }
    out
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int(i32),
    Float(f64),
    Bool(bool),
    Null,
    Str(String),
    Var(String),
    ListLiteral(Vec<Expr>),
    Not(Box<Expr>),
    Binary(String, Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Substring(Box<Expr>, Box<Expr>, Box<Expr>),
    Length(Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Print(Expr),
    Input(String),
    Assign(String, Expr),
    FileRead(String, Expr),
    FileWrite(Expr, Expr),
    FileAppend(Expr, Expr),
    For(Expr, Expr, Vec<Stmt>),
    ForEach(String, Expr, Vec<Stmt>),
    While(Expr, Vec<Stmt>),
    If(Expr, Vec<Stmt>, Vec<Stmt>),
    FuncDef(String, Vec<String>, Vec<Stmt>),
    Expr(Expr),
    Return(Expr),
    Break,
    Continue,
}

pub struct Parser {
    tokens: Vec<String>,
    lines: Vec<usize>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<(String, usize)>) -> Self {
        let (tokens, lines): (Vec<String>, Vec<usize>) = tokens.into_iter().unzip();
        Parser { tokens, lines, pos: 0 }
    }

    /// Line number of the current token (for error messages).
    fn cur_line(&self) -> usize {
        self.lines.get(self.pos)
            .copied()
            .unwrap_or_else(|| self.lines.last().copied().unwrap_or(0))
    }

    fn peek(&self) -> Option<&String> {
        if self.pos < self.tokens.len() {
            Some(&self.tokens[self.pos])
        } else {
            None
        }
    }

    fn consume(&mut self) -> String {
        // Return an empty token at EOF instead of panicking; callers and
        // `match_token` then surface a clean "Expected ... got ''" error.
        match self.peek() {
            Some(tok) => {
                let token = tok.clone();
                self.pos += 1;
                token
            }
            None => String::new(),
        }
    }

    fn match_token(&mut self, expected: &str) -> Result<(), String> {
        let line = self.cur_line();
        let token = self.consume();
        if token != expected {
            return Err(format!("Expected '{}', got '{}' (near line {})", expected, token, line));
        }
        Ok(())
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, String> {
        let mut stmts = Vec::new();
        while self.peek().is_some() {
            stmts.push(self.parse_statement()?);
        }
        Ok(stmts)
    }

    fn parse_logical(&mut self) -> Result<Expr, String> {
        let mut node = self.parse_comparison()?;

        while let Some(op) = self.peek() {
            if op == "&&" || op == "||" {
                let operator = self.consume();
                let right = self.parse_comparison()?;
                node = Expr::Binary(operator, Box::new(node), Box::new(right));
            } else {
                break;
            }
        }

        Ok(node)
    }

    fn parse_comparison(&mut self) -> Result<Expr, String> {
        let left = self.parse_expression()?;
        let mut ops = Vec::new();

        while let Some(op) = self.peek() {
            if ["==", "!=", "<", ">", "<=", ">="].contains(&op.as_str()) {
                let operator = self.consume();
                let right = self.parse_expression()?;
                ops.push((operator, right));
            } else {
                break;
            }
        }

        if ops.is_empty() {
            return Ok(left);
        }

        let mut exprs = Vec::new();
        let mut prev = left.clone();

        for (op, operand) in ops {
            exprs.push((op, prev.clone(), operand.clone()));
            prev = operand;
        }

        let mut node = Expr::Binary(
            exprs[0].0.clone(),
            Box::new(exprs[0].1.clone()),
            Box::new(exprs[0].2.clone()),
        );

        for e in exprs.iter().skip(1) {
            let right = Expr::Binary(
                e.0.clone(),
                Box::new(e.1.clone()),
                Box::new(e.2.clone()),
            );
            node = Expr::Binary(
                "&&".to_string(),
                Box::new(node),
                Box::new(right),
            );
        }

        Ok(node)
    }

    fn parse_expression(&mut self) -> Result<Expr, String> {
        let mut node = self.parse_term()?;

        while let Some(op) = self.peek() {
            if op == "+" || op == "-" {
                let operator = self.consume();
                let right = self.parse_term()?;
                node = Expr::Binary(operator, Box::new(node), Box::new(right));
            } else {
                break;
            }
        }

        Ok(node)
    }

    fn parse_term(&mut self) -> Result<Expr, String> {
        let mut node = self.parse_factor()?;

        while let Some(op) = self.peek() {
            if op == "*" || op == "/" || op == "%" {
                let operator = self.consume();
                let right = self.parse_factor()?;
                node = Expr::Binary(operator, Box::new(node), Box::new(right));
            } else {
                break;
            }
        }

        Ok(node)
    }

    fn parse_factor(&mut self) -> Result<Expr, String> {
        if let Some(tok) = self.peek() {
            // Unary minus: `-x` desugars to `0 - x` so negative literals and
            // negated expressions parse (B4). Binds tighter than * and /.
            if tok == "-" {
                self.consume();
                let operand = self.parse_factor()?;
                return Ok(Expr::Binary(
                    "-".to_string(),
                    Box::new(Expr::Int(0)),
                    Box::new(operand),
                ));
            }

            // Logical NOT: `!x` / `അല്ല x`. Binds like other unary operators.
            if tok == "!" || tok == "അല്ല" {
                self.consume();
                let operand = self.parse_factor()?;
                return Ok(Expr::Not(Box::new(operand)));
            }

            // Check for the new length function
            if tok == "നീളം" {
                self.consume(); // Consume "നീളം"
                self.match_token("(")?;
                let expr = self.parse_logical()?;
                self.match_token(")")?;
                return Ok(Expr::Length(Box::new(expr)));
            }

            if (is_ident(tok)
                || tok.starts_with('"')
                || tok.starts_with('\''))
                && self.pos + 1 < self.tokens.len()
                && self.tokens[self.pos + 1] == "[" {

                let expr = self.parse_primary()?;

                if self.peek() == Some(&"[".to_string()) {
                    self.consume();
                    let index = self.parse_logical()?;
                    self.match_token("]")?;
                    return Ok(Expr::Index(Box::new(expr), Box::new(index)));
                }

                return Ok(expr);
            }

            if is_ident(tok)
                && self.pos + 2 < self.tokens.len()
                && self.tokens[self.pos + 1] == "."
                && self.tokens[self.pos + 2] == "മുറിക്കുക" {

                let var_name = self.consume();
                self.match_token(".")?;
                self.match_token("മുറിക്കുക")?;
                self.match_token("(")?;
                let start = self.parse_logical()?;
                self.match_token(",")?;
                let end = self.parse_logical()?;
                self.match_token(")")?;

                return Ok(Expr::Substring(
                    Box::new(Expr::Var(var_name)),
                    Box::new(start),
                    Box::new(end)
                ));
            }
        }

        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        if let Some(tok) = self.peek() {

            // Boolean / null literals.
            if tok == "ശരി" { self.consume(); return Ok(Expr::Bool(true)); }
            if tok == "തെറ്റ്" { self.consume(); return Ok(Expr::Bool(false)); }
            if tok == "ശൂന്യം" { self.consume(); return Ok(Expr::Null); }

            // List literal: [a, b, c]
            if tok == "[" {
                self.consume();
                let mut items = Vec::new();
                if self.peek() != Some(&"]".to_string()) {
                    items.push(self.parse_logical()?);
                    while self.peek() == Some(&",".to_string()) {
                        self.consume();
                        items.push(self.parse_logical()?);
                    }
                }
                self.match_token("]")?;
                return Ok(Expr::ListLiteral(items));
            }

            // Function call (user functions or Malayalam-named builtins).
            if is_word(tok)
                && self.pos + 1 < self.tokens.len()
                && self.tokens[self.pos + 1] == "(" {

                let name = self.consume();
                self.consume();

                let mut args = Vec::new();
                if self.peek() != Some(&")".to_string()) {
                    args.push(self.parse_logical()?);
                    while self.peek() == Some(&",".to_string()) {
                        self.consume();
                        args.push(self.parse_logical()?);
                    }
                }

                self.match_token(")")?;
                return Ok(Expr::Call(name, args));
            }

            if (tok.starts_with('"') && tok.ends_with('"'))
                || (tok.starts_with('\'') && tok.ends_with('\'')) {

                let val = self.consume();
                return Ok(Expr::Str(unescape(&val[1..val.len()-1])));
            }

            // Float literal (must be tried before int; lexer only emits a float
            // token when a `.` is present).
            if tok.contains('.') && tok.parse::<f64>().is_ok() {
                let val = self.consume().parse::<f64>().unwrap();
                return Ok(Expr::Float(val));
            }

            if tok.parse::<i32>().is_ok() {
                let val = self.consume().parse::<i32>().unwrap();
                return Ok(Expr::Int(val));
            }

            if is_ident(tok) {
                return Ok(Expr::Var(self.consume()));
            }

            if tok == "(" {
                self.consume();
                let expr = self.parse_logical()?;
                self.match_token(")")?;
                return Ok(expr);
            }
        }

        Err(format!("Unexpected token in factor: {:?} (near line {})", self.peek(), self.cur_line()))
    }

    fn parse_statement(&mut self) -> Result<Stmt, String> {
        if let Some(tok) = self.peek() {

            if tok == "നിശ്ചയിക്കുക" {
                self.consume();
                let name = self.consume();
                self.match_token("(")?;

                let mut params = Vec::new();
                if self.peek() != Some(&")".to_string()) {
                    params.push(self.consume());
                    while self.peek() == Some(&",".to_string()) {
                        self.consume();
                        params.push(self.consume());
                    }
                }

                self.match_token(")")?;
                self.match_token("{")?;

                // Returns stay in `body`; the interpreter short-circuits on
                // them via run_stmt's Option<Value> propagation (B2). No
                // hoisting, so position and early-return semantics are correct.
                let mut body = Vec::new();
                while self.peek().is_some() && self.peek() != Some(&"}".to_string()) {
                    body.push(self.parse_statement()?);
                }

                self.match_token("}")?;
                return Ok(Stmt::FuncDef(name, params, body));
            }

            if tok == "മറുപടി" {
                self.consume();
                let expr = self.parse_logical()?;
                return Ok(Stmt::Return(expr));
            }

            if tok == "നിർത്തുക" {
                self.consume();
                return Ok(Stmt::Break);
            }

            if tok == "തുടരുക" {
                self.consume();
                return Ok(Stmt::Continue);
            }

            // for-each: `ഓരോ x ഇൽ <iterable> { ... }`
            if tok == "ഓരോ" {
                self.consume();
                let var = self.consume();
                self.match_token("ഇൽ")?;
                let iterable = self.parse_logical()?;
                self.match_token("{")?;

                let mut body = Vec::new();
                while self.peek().is_some() && self.peek() != Some(&"}".to_string()) {
                    body.push(self.parse_statement()?);
                }

                self.match_token("}")?;
                return Ok(Stmt::ForEach(var, iterable, body));
            }

            if tok == "പറയുക" {
                self.consume();
                return Ok(Stmt::Print(self.parse_logical()?));
            }

            if tok == "സ്വീകരിക്കുക" {
                self.consume();
                let varname = self.consume();
                return Ok(Stmt::Input(varname));
            }

            if is_ident(tok)
                && self.pos + 1 < self.tokens.len()
                && is_assign_op(&self.tokens[self.pos + 1]) {

                let varname = self.consume();
                let op = self.consume(); // "=", "+=", "-=", "*=", "/=", "%="

                if op == "=" && self.peek() == Some(&"വായിക്കുക".to_string()) {
                    self.consume();
                    self.match_token("(")?;
                    let filename_expr = self.parse_logical()?;
                    self.match_token(")")?;
                    return Ok(Stmt::FileRead(varname, filename_expr));
                }

                let rhs = self.parse_logical()?;
                let value = if op == "=" {
                    rhs
                } else {
                    // Desugar `x += e` -> `x = x + e`.
                    let binop = op.trim_end_matches('=').to_string();
                    Expr::Binary(binop, Box::new(Expr::Var(varname.clone())), Box::new(rhs))
                };
                return Ok(Stmt::Assign(varname, value));
            }

            if tok == "എഴുതുക" {
                self.consume();
                self.match_token("(")?;
                let content_expr = self.parse_logical()?;
                self.match_token(",")?;
                let filename_expr = self.parse_logical()?;
                self.match_token(")")?;
                return Ok(Stmt::FileWrite(content_expr, filename_expr));
            }

            if tok == "കൂട്ടിച്ചേർക്കുക" {
                self.consume();
                self.match_token("(")?;
                let content_expr = self.parse_logical()?;
                self.match_token(",")?;
                let filename_expr = self.parse_logical()?;
                self.match_token(")")?;
                return Ok(Stmt::FileAppend(content_expr, filename_expr));
            }

            if tok == "ഈ" {
                self.consume();
                let cond = self.parse_logical()?;
                self.match_token("സത്യമാവണവരെ")?;
                self.match_token("{")?;

                let mut body = Vec::new();
                while self.peek() != Some(&"}".to_string()) {
                    body.push(self.parse_statement()?);
                }

                self.match_token("}")?;
                return Ok(Stmt::While(cond, body));
            }

            if tok == "ഇത്" {
                self.consume();
                let cond = self.parse_logical()?;
                self.match_token("സത്യമോ")?;
                self.match_token("{")?;

                let mut then_body = Vec::new();
                while self.peek() != Some(&"}".to_string()) {
                    then_body.push(self.parse_statement()?);
                }

                self.match_token("}")?;

                let mut else_body = Vec::new();
                if self.peek() == Some(&"അല്ലെങ്കില്".to_string()) {
                    self.consume();

                    // else-if: `അല്ലെങ്കില് ഇത് ...` chains by recursing into
                    // another `If`, which becomes the sole statement of this
                    // else block. Otherwise it's a plain `{ ... }` else block.
                    if self.peek() == Some(&"ഇത്".to_string()) {
                        else_body.push(self.parse_statement()?);
                    } else {
                        self.match_token("{")?;
                        while self.peek() != Some(&"}".to_string()) {
                            else_body.push(self.parse_statement()?);
                        }
                        self.match_token("}")?;
                    }
                }

                return Ok(Stmt::If(cond, then_body, else_body));
            }

            // For-loop or bare expression statement (B3, L4). Both begin with a
            // general expression, so parse one and branch on whether `മുതൽ`
            // follows. A failed parse backs out cleanly via the saved position.
            let saved = self.pos;
            match self.parse_logical() {
                Ok(start_expr) => {
                    if self.peek() == Some(&"മുതൽ".to_string()) {
                        self.consume();
                        let end_expr = self.parse_logical()?;
                        self.match_token("വരെ")?;
                        self.match_token("{")?;

                        let mut body = Vec::new();
                        while self.peek().is_some() && self.peek() != Some(&"}".to_string()) {
                            body.push(self.parse_statement()?);
                        }

                        self.match_token("}")?;
                        return Ok(Stmt::For(start_expr, end_expr, body));
                    }

                    return Ok(Stmt::Expr(start_expr));
                }
                Err(_) => {
                    self.pos = saved;
                }
            }
        }

        Err(format!("Unknown statement start: {:?} (near line {})", self.peek(), self.cur_line()))
    }
}
