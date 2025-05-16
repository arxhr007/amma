use regex::Regex;

pub fn tokenize(code: &str) -> Vec<String> {
    let code_without_single_comments = Regex::new(r"//.*").unwrap().replace_all(code, "");
    let code_without_comments = Regex::new(r"/\*[\s\S]*?\*/").unwrap().replace_all(&code_without_single_comments, "");
    
 
    let token_spec = r#"("[^"]*"|'[^']*'|==|!=|<=|>=|<|>|&&|\|\||\d+|വായിക്കുക|എഴുതുക|കൂട്ടിച്ചേർക്കുക|പറയുക|സ്വീകരിക്കുക|മുതൽ|വരെ|ഈ|സത്യമാവണവരെ|ഇത്|സത്യമോ|അല്ലെങ്കില്|നിശ്ചയിക്കുക|മറുപടി|മുറിക്കുക|നീളം|[a-zA-Z_]\w*|[\[\].]|[%+\-*/=(),{}])"#;
    
    let re = Regex::new(token_spec).unwrap();
    
    re.find_iter(&code_without_comments)
      .map(|m| m.as_str().to_string())
      .collect()
}
