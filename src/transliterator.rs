use regex::Regex;
use std::collections::HashMap;

pub fn transliterate_source(src: &str) -> String {
    let keyword_map: HashMap<&str, &str> = [
        ("parayuka", "പറയുക"),
        ("sweekarikkuka", "സ്വീകരിക്കുക"),
        ("muthal", "മുതൽ"),
        ("vare", "വരെ"),
        ("ee", "ഈ"),
        ("sathyamaavanavare", "സത്യമാവണവരെ"),
        ("ithu", "ഇത്"),
        ("sathyamo", "സത്യമോ"),
        ("allenkil", "അല്ലെങ്കില്"),
        ("nischayikkuka", "നിശ്ചയിക്കുക"),
        ("marupadi", "മറുപടി"),
        ("vayikkuka", "വായിക്കുക"),
        ("ezhuthuka", "എഴുതുക"),
        ("kootticherkuka", "കൂട്ടിച്ചേർക്കുക"),
        ("murikkuka", "മുറിക്കുക"),
        ("neelam", "നീളം"),

        // --- data-type literals ---
        ("sari", "ശരി"),
        ("thettu", "തെറ്റ്"),
        ("shoonyam", "ശൂന്യം"),

        // --- control flow ---
        ("nirthuka", "നിർത്തുക"),
        ("thudaruka", "തുടരുക"),
        ("alla", "അല്ല"),
        ("oro", "ഓരോ"),
        ("il", "ഇൽ"),

        // --- stdlib: math ---
        ("kevalam", "കേവലം"),
        ("ghaatham", "ഘാതം"),
        ("vargamoolam", "വർഗമൂലം"),
        ("kuranjathu", "കുറഞ്ഞത്"),
        ("koodiyathu", "കൂടിയത്"),
        ("kramarahitham", "ക്രമരഹിതം"),

        // --- stdlib: type conversion ---
        ("samkhya", "സംഖ്യ"),
        ("dashaamsham", "ദശാംശം"),
        ("vaachakam", "വാചകം"),

        // --- stdlib: string / list ops ---
        ("valuthu", "വലുത്"),
        ("cheruthu", "ചെറുത്"),
        ("othukkuka", "ഒതുക്കുക"),
        ("vibhajikkuka", "വിഭജിക്കുക"),
        ("maattuka", "മാറ്റുക"),
        ("undo", "ഉണ്ടോ"),
        ("cherkkuka", "ചേർക്കുക"),
        ("neekkuka", "നീക്കുക"),

        // --- tooling ---
        ("ulppeduthuka", "ഉൾപ്പെടുത്തുക"),
    ].iter().cloned().collect();

    let re = Regex::new(r#"(".*?"|'.*?')"#).unwrap();

    let mut result = String::new();
    let mut last_end = 0;

    for mat in re.find_iter(src) {

        let before_match = &src[last_end..mat.start()];
        result.push_str(&transliterate_segment(before_match, &keyword_map));

        result.push_str(mat.as_str());

        last_end = mat.end();
    }

    if last_end < src.len() {
        result.push_str(&transliterate_segment(&src[last_end..], &keyword_map));
    }

    result
}

fn transliterate_segment(text: &str, keyword_map: &HashMap<&str, &str>) -> String {
    let word_regex = Regex::new(r"\b(\w+)\b").unwrap();

    word_regex.replace_all(text, |caps: &regex::Captures| {
        let word = caps.get(1).unwrap().as_str();
        match keyword_map.get(word) {
            Some(&replacement) => replacement.to_string(),
            None => word.to_string(),
        }
    }).to_string()
}
