use regex::Regex;

/// Remove `//` line comments and `/* ... */` block comments.
///
/// Exposed so callers can strip comments *before* transliteration (L3):
/// otherwise an unbalanced quote inside a comment can mis-pair with a later
/// quote in the transliterator's string-protection regex and corrupt code.
pub fn strip_comments(code: &str) -> String {
    let without_line = Regex::new(r"//.*").unwrap().replace_all(code, "");
    Regex::new(r"/\*[\s\S]*?\*/").unwrap().replace_all(&without_line, "").to_string()
}

pub fn tokenize(code: &str) -> Vec<String> {
    let code_without_comments = strip_comments(code);

    let token_spec = r#"("[^"]*"|'[^']*'|==|!=|<=|>=|<|>|&&|\|\||\d+|വായിക്കുക|എഴുതുക|കൂട്ടിച്ചേർക്കുക|പറയുക|സ്വീകരിക്കുക|മുതൽ|വരെ|ഈ|സത്യമാവണവരെ|ഇത്|സത്യമോ|അല്ലെങ്കില്|നിശ്ചയിക്കുക|മറുപടി|മുറിക്കുക|നീളം|[a-zA-Z_]\w*|[\[\].]|[%+\-*/=(),{}])"#;

    let re = Regex::new(token_spec).unwrap();

    re.find_iter(&code_without_comments)
      .map(|m| m.as_str().to_string())
      .collect()
}
