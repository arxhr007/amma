use regex::Regex;

#[derive(Debug, Clone)]
pub enum Expr {
    Int(i32),
    Str(String),
    Var(String),
    Binary(String, Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Substring(Box<Expr>, Box<Expr>, Box<Expr>),
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
    While(Expr, Vec<Stmt>),
    If(Expr, Vec<Stmt>, Vec<Stmt>),
    FuncDef(String, Vec<String>, Vec<Stmt>, Option<Expr>),
    Expr(Expr),
    Return(Expr),
}

pub struct Parser {
    tokens: Vec<String>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<String>) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn peek(&self) -> Option<&String> {
        if self.pos < self.tokens.len() {
            Some(&self.tokens[self.pos])
        } else {
            None
        }
    }

    fn consume(&mut self) -> String {
        let token = self.peek().unwrap().clone();
        self.pos += 1;
        token
    }

    fn match_token(&mut self, expected: &str) -> Result<(), String> {
        let token = self.consume();
        if token != expected {
            return Err(format!("Expected '{}', got '{}'", expected, token));
        }
        Ok(())
    }

    fn next_is_loop(&self) -> bool {
        if let Some(tok) = self.peek() {
            if self.pos + 1 < self.tokens.len() {
                let next_tok = &self.tokens[self.pos + 1];
                return (tok.parse::<i32>().is_ok() 
                    || Regex::new(r"[a-zA-Z_]\w*").unwrap().is_match(tok)
                    || tok.starts_with('"')
                    || tok.starts_with('\'')
                    || tok == "(") && next_tok == "മുതൽ";
            }
        }
        false
    }

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while self.peek().is_some() {
            stmts.push(self.parse_statement().unwrap());
        }
        stmts
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

            if (Regex::new(r"[a-zA-Z_]\w*").unwrap().is_match(tok) 
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

            if Regex::new(r"[a-zA-Z_]\w*").unwrap().is_match(tok) 
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

            if Regex::new(r"[a-zA-Z_]\w*").unwrap().is_match(tok) 
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
                return Ok(Expr::Str(val[1..val.len()-1].to_string()));
            }

            if tok.parse::<i32>().is_ok() {
                let val = self.consume().parse::<i32>().unwrap();
                return Ok(Expr::Int(val));
            }

            if Regex::new(r"[a-zA-Z_]\w*").unwrap().is_match(tok) {
                return Ok(Expr::Var(self.consume()));
            }

            if tok == "(" {
                self.consume();
                let expr = self.parse_logical()?;
                self.match_token(")")?;
                return Ok(expr);
            }
        }

        Err(format!("Unexpected token in factor: {:?}", self.peek()))
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

                let mut body = Vec::new();
                let mut ret_expr = None;

                while self.peek().is_some() && self.peek() != Some(&"}".to_string()) {
                    let stmt = self.parse_statement()?;
                    if let Stmt::Return(expr) = stmt {
                        ret_expr = Some(expr);
                    } else {
                        body.push(stmt);
                    }
                }

                self.match_token("}")?;
                return Ok(Stmt::FuncDef(name, params, body, ret_expr));
            }

            if tok == "മറുപടി" {
                self.consume();
                let expr = self.parse_logical()?;
                return Ok(Stmt::Return(expr));
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

            if Regex::new(r"[a-zA-Z_]\w*").unwrap().is_match(tok) 
                && self.pos + 1 < self.tokens.len() 
                && self.tokens[self.pos + 1] == "=" {

                let varname = self.consume();
                self.match_token("=")?;

                if self.peek() == Some(&"വായിക്കുക".to_string()) {
                    self.consume();
                    self.match_token("(")?;
                    let filename_expr = self.parse_logical()?;
                    self.match_token(")")?;
                    return Ok(Stmt::FileRead(varname, filename_expr));
                } else {
                    return Ok(Stmt::Assign(varname, self.parse_logical()?));
                }
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

            if self.next_is_loop() {
                let start_expr = self.parse_logical()?;
                self.match_token("മുതൽ")?;
                let end_expr = self.parse_logical()?;
                self.match_token("വരെ")?;
                self.match_token("{")?;

                let mut body = Vec::new();
                while self.peek() != Some(&"}".to_string()) {
                    body.push(self.parse_statement()?);
                }

                self.match_token("}")?;
                return Ok(Stmt::For(start_expr, end_expr, body));
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
                    self.match_token("{")?;

                    while self.peek() != Some(&"}".to_string()) {
                        else_body.push(self.parse_statement()?);
                    }

                    self.match_token("}")?;
                }

                return Ok(Stmt::If(cond, then_body, else_body));
            }

            if Regex::new(r"[a-zA-Z_]\w*").unwrap().is_match(tok) 
                && self.pos + 1 < self.tokens.len() 
                && self.tokens[self.pos + 1] == "(" {

                let call_expr = self.parse_factor()?;
                return Ok(Stmt::Expr(call_expr));
            }
        }

        Err(format!("Unknown statement start: {:?}", self.peek()))
    }
}