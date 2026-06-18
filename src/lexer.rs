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

/// Tokenize source into `(token, line_number)` pairs. Line numbers are 1-based
/// and let the parser report `Parse error (near line N)`.
///
/// Ordering in `token_spec` matters: multi-char operators and float literals
/// come before their single-char prefixes, and every Malayalam keyword comes
/// before the generic ASCII identifier rule so keywords win. `അല്ലെങ്കില്`
/// precedes `അല്ല` because the latter is a prefix of the former.
pub fn tokenize(code: &str) -> Vec<(String, usize)> {
    let code = strip_comments(code);

    let token_spec = r#"("(?:\\.|[^"\\])*"|'(?:\\.|[^'\\])*'|==|!=|<=|>=|\+=|-=|\*=|/=|%=|<|>|&&|\|\||\d+\.\d+|\d+|വായിക്കുക|എഴുതുക|കൂട്ടിച്ചേർക്കുക|പറയുക|സ്വീകരിക്കുക|മുതൽ|വരെ|സത്യമാവണവരെ|ഇത്|സത്യമോ|അല്ലെങ്കില്|അല്ല|നിശ്ചയിക്കുക|മറുപടി|മുറിക്കുക|നീളം|ശരി|തെറ്റ്|ശൂന്യം|നിർത്തുക|തുടരുക|ഓരോ|ഇൽ|ഈ|കേവലം|ഘാതം|വർഗമൂലം|കുറഞ്ഞത്|കൂടിയത്|ക്രമരഹിതം|സംഖ്യ|ദശാംശം|വാചകം|വലുത്|ചെറുത്|ഒതുക്കുക|വിഭജിക്കുക|മാറ്റുക|ഉണ്ടോ|ചേർക്കുക|നീക്കുക|[a-zA-Z_]\w*|[\[\].]|[%+\-*/=(),{}!])"#;

    let re = Regex::new(token_spec).unwrap();

    re.find_iter(&code)
      .map(|m| {
          let line = code[..m.start()].bytes().filter(|&b| b == b'\n').count() + 1;
          (m.as_str().to_string(), line)
      })
      .collect()
}
