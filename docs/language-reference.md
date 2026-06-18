# Amma Language Reference

This is the complete reference for the Amma language. Every keyword has a native
Malayalam form and a manglish (Latin transliteration) form; both are shown as
`മലയാളം` / `manglish`. They are interchangeable and may even be mixed in one
program — manglish is converted to Malayalam before tokenizing.

- [Writing in two scripts](#writing-in-two-scripts)
- [Lexical structure](#lexical-structure)
- [Data types](#data-types)
- [Variables](#variables)
- [Operators](#operators)
- [Control flow](#control-flow)
- [Functions](#functions)
- [Strings](#strings)
- [Lists](#lists)
- [Built-in functions](#built-in-functions)
- [Input and output](#input-and-output)
- [File I/O](#file-io)
- [Modules / imports](#modules--imports)
- [Tooling](#tooling)
- [Errors](#errors)
- [Keyword index](#keyword-index)

---

## Writing in two scripts

These two programs are identical:

```
nischayikkuka sq(n) { marupadi n * n }
parayuka sq(5)
```
```
നിശ്ചയിക്കുക sq(n) { മറുപടി n * n }
പറയുക sq(5)
```

Manglish keywords are replaced by their Malayalam equivalents before the program
is tokenized. Text inside string literals is never transliterated.

> **Reserved words.** Because manglish keywords are replaced everywhere they
> appear as whole words, do not use a keyword (e.g. `muthal`, `vare`, `il`,
> `oro`, `sari`, `undo`, `neelam`) as a variable or function name.

---

## Lexical structure

### Comments
```
// single-line comment

/* multi-line
   comment */
```
Comments are stripped before anything else runs.

### Identifiers
Start with an ASCII letter or `_`, followed by letters, digits or `_`. Examples:
`name`, `total2`, `_temp`. Builtins are referred to by their Malayalam names and
may be called directly even in pure-Malayalam code (e.g. `കേവലം(5)`).

### Literals

| Kind | Examples |
|------|----------|
| Integer | `0`, `42`, `-7` |
| Float | `3.14`, `0.5` (must contain a `.` with digits on both sides) |
| String | `"hello"`, `'world'` |
| Boolean | `ശരി` / `sari`, `തെറ്റ്` / `thettu` |
| Null | `ശൂന്യം` / `shoonyam` |
| List | `[1, 2, 3]`, `["a", "b"]`, `[]` |

String literals support escape sequences: `\n`, `\t`, `\r`, `\\`, `\"`, `\'`.
An unknown escape is left verbatim.

---

## Data types

| Type | Notes |
|------|-------|
| Integer | 32-bit signed |
| Float | 64-bit; any arithmetic mixing int and float yields a float |
| Boolean | produced by comparisons / logical operators; displays as `ശരി` / `തെറ്റ്` |
| String | UTF-8; indexing and length are by character, not byte |
| List | ordered, heterogeneous; 0-based indexing |
| Null | the "nothing" value (`ശൂന്യം`) |

### Truthiness
Used by `if`, `while`, `&&`, `||`, `!`:

| Value | Truthy when |
|-------|-------------|
| Boolean | it is `ശരി` |
| Integer | non-zero |
| Float | non-zero |
| String | non-empty |
| List | non-empty |
| Null | always false |

### Coercion
- `+` with any string operand concatenates (the other operand is stringified).
- Numeric operators coerce strings via parsing; an unparseable string becomes `0`
  (int context) or `0.0` (float context). This is lenient by design.
- Booleans act as `1` / `0` in numeric context.

### Equality
`==` / `!=` compare by value. Numbers and booleans compare on a numeric footing
(`1 == ശരി` is true). `ശൂന്യം` equals only `ശൂന്യം`. Lists compare element-wise.

---

## Variables

Assignment creates or updates a variable:

```
name = "Aaron"
age = 19
pi = 3.14
```

Reading an **undefined** variable yields `0` (it does not error). Compound
assignment updates in place:

```
total = 0
total += 5     // 5   (== total = total + 5)
total -= 2     // 3
total *= 4     // 12
total /= 3     // 4
total %= 3     // 1
```

---

## Operators

| Category | Operators |
|----------|-----------|
| Arithmetic | `+`  `-`  `*`  `/` (integer or float division)  `%` (integer modulo) |
| Comparison | `==`  `!=`  `<`  `>`  `<=`  `>=` |
| Logical | `&&` (and)  `\|\|` (or)  `!` or `അല്ല` / `alla` (not) |
| Assignment | `=`  `+=`  `-=`  `*=`  `/=`  `%=` |
| String | `+` (concatenate)  `*` (repeat, `"ab" * 3`) |

### Precedence (lowest → highest)
1. `&&`, `||`
2. comparisons (`==`, `!=`, `<`, `>`, `<=`, `>=`)
3. `+`, `-`
4. `*`, `/`, `%`
5. unary `-`, `!` / `അല്ല`, and the postfix-like forms (`నീളം`, indexing `[]`, `.മുറിക്കുക`)

Parenthesize with `( )` to override. Note: a chain of comparisons such as
`a < b < c` is evaluated as `(a < b) && (b < c)`.

### Division and modulo
- `/` is integer division when both operands are integers, float division if
  either is a float.
- `%` always operates on integers.
- Division or modulo by zero is a runtime error.

---

## Control flow

### If / else / else-if

```
ithu condition sathyamo {
    // when truthy
} allenkil {
    // otherwise
}
```
```
ഇത് condition സത്യമോ {
    // when truthy
} അല്ലെങ്കില് {
    // otherwise
}
```

Chain with else-if by following `allenkil` / `അല്ലെങ്കില്` with another
`ithu` / `ഇത്`:

```
ithu n == 1 sathyamo {
    parayuka "one"
} allenkil ithu n == 2 sathyamo {
    parayuka "two"
} allenkil {
    parayuka "other"
}
```

### While loop

```
ee condition sathyamaavanavare {
    // repeats while condition is truthy
}
```
```
ഈ condition സത്യമാവണവരെ {
    // ...
}
```

### For loop (counting)
Counts from a start value up to and **including** an end value. The current value
is available as `_`. Nested loops each get their own `_` (the outer value is
restored when the inner loop ends).

```
0 muthal 5 vare {
    parayuka _      // 0 1 2 3 4 5
}
```
```
0 മുതൽ 5 വരെ {
    പറയുക _
}
```
The start and end may be any expression: `x + 1 muthal len(items) - 1 vare { }`.

### For-each loop
Iterates over the items of a list, or the characters of a string.

```
oro item il [10, 20, 30] {
    parayuka item
}
```
```
ഓരോ item ഇൽ [10, 20, 30] {
    പറയുക item
}
```

### Break and continue
`nirthuka` / `നിർത്തുക` exits the nearest loop; `thudaruka` / `തുടരുക` skips to
its next iteration. Both work in `muthal..vare`, `oro..il`, and `while` loops.

```
0 muthal 10 vare {
    ithu _ == 5 sathyamo { nirthuka }
    ithu _ % 2 == 0 sathyamo { thudaruka }
    parayuka _
}
```

---

## Functions

```
nischayikkuka add(a, b) {
    marupadi a + b
}
parayuka add(2, 3)        // 5
```
```
നിശ്ചയിക്കുക add(a, b) {
    മറുപടി a + b
}
```

- `marupadi` / `മറുപടി` returns a value and stops the function immediately. A
  function that returns nothing yields `0`.
- **Scope is parameter-only.** A function sees its parameters and any variables
  it assigns locally — not the caller's variables. This avoids accidental leaks.
- **Hoisting.** All top-level functions are registered before execution, so a
  function may call another defined later in the file, and mutual recursion
  works.
- **Builtins win.** A user function cannot shadow a built-in name.

```
// recursion
nischayikkuka fact(n) {
    ithu n <= 1 sathyamo { marupadi 1 }
    marupadi n * fact(n - 1)
}
parayuka fact(5)          // 120
```

---

## Strings

| Operation | Syntax | Example → result |
|-----------|--------|------------------|
| Concatenate | `a + b` | `"foo" + "bar"` → `foobar` |
| Repeat | `s * n` | `"ab" * 3` → `ababab` |
| Length | `neelam(s)` / `നീളം(s)` | `neelam("hello")` → `5` |
| Index | `s[i]` | `"hello"[1]` → `e` |
| Substring | `s.murikkuka(a, b)` / `s.മുറിക്കുക(a, b)` | `"hello".murikkuka(0, 3)` → `hel` |

Indexing and length count characters (Unicode-aware). Substring takes a start
index and an end index (exclusive). Out-of-range access is a runtime error.

```
word = "Aaron"
parayuka word[0]                  // A
parayuka word.murikkuka(1, 3)     // ar
parayuka neelam(word)             // 5
```

---

## Lists

```
nums = [1, 2, 3]
parayuka nums          // [1, 2, 3]
parayuka nums[0]       // 1
parayuka neelam(nums)  // 3
```

List builtins return a **new** list (values are immutable), so reassign:

```
nums = cherkkuka(nums, 4)   // [1, 2, 3, 4]   append
nums = neekkuka(nums, 0)    // [2, 3, 4]      remove index 0
parayuka undo(nums, 3)      // ശരി           contains?
```

Iterate with a for-each loop, or by index with a counting loop:

```
oro n il nums { parayuka n }

0 muthal neelam(nums) - 1 vare {
    parayuka nums[_]
}
```

---

## Built-in functions

Available everywhere; callable by Malayalam or manglish name. Cannot be shadowed.

### Math
| Malayalam | Manglish | Description |
|-----------|----------|-------------|
| കേവലം | `kevalam(x)` | Absolute value |
| ഘാതം | `ghaatham(base, exp)` | Power |
| വർഗമൂലം | `vargamoolam(x)` | Square root (float) |
| കുറഞ്ഞത് | `kuranjathu(a, b, …)` | Minimum of arguments, or of one list |
| കൂടിയത് | `koodiyathu(a, b, …)` | Maximum of arguments, or of one list |
| ക്രമരഹിതം | `kramarahitham(n)` | Random integer in `0 .. n` |

### Type conversion
| Malayalam | Manglish | Description |
|-----------|----------|-------------|
| സംഖ്യ | `samkhya(x)` | To integer |
| ദശാംശം | `dashaamsham(x)` | To float |
| വാചകം | `vaachakam(x)` | To string |

### String / list
| Malayalam | Manglish | Description |
|-----------|----------|-------------|
| വലുത് | `valuthu(s)` | Uppercase |
| ചെറുത് | `cheruthu(s)` | Lowercase |
| ഒതുക്കുക | `othukkuka(s)` | Trim surrounding whitespace |
| വിഭജിക്കുക | `vibhajikkuka(s, sep)` | Split string into a list by `sep` (empty `sep` → characters) |
| മാറ്റുക | `maattuka(s, old, new)` | Replace all `old` with `new` |
| ഉണ്ടോ | `undo(haystack, needle)` | Contains check → boolean (string substring, or list membership) |
| ചേർക്കുക | `cherkkuka(list, x)` | New list with `x` appended |
| നീക്കുക | `neekkuka(list, i)` | New list with index `i` removed |

```
parayuka kevalam(-7)                 // 7
parayuka ghaatham(2, 10)            // 1024
parayuka vargamoolam(144)           // 12
parayuka kuranjathu(5, 2, 9)        // 2
parayuka samkhya("42") + 8          // 50
parayuka valuthu("hello")           // HELLO
parayuka vibhajikkuka("a,b,c", ",") // [a, b, c]
parayuka maattuka("hello", "l", "L")// heLLo
```

---

## Input and output

```
parayuka "Hello"          // print a line  (പറയുക)
parayuka variable

sweekarikkuka name        // read a line into `name`  (സ്വീകരിക്കുക)
```
Input that parses as an integer is stored as an integer; otherwise as a string.

---

## File I/O

```
content = vayikkuka("in.txt")          // read   (വായിക്കുക)
ezhuthuka(content, "out.txt")          // write  (എഴുതുക)
kootticherkuka(" more", "out.txt")     // append (കൂട്ടിച്ചേർക്കുക)
```
Reading a missing file prints a notice and yields an empty string.

---

## Modules / imports

`ulppeduthuka` / `ഉൾപ്പെടുത്തുക` splices another `.amma` file in at that point
before the program runs. Paths are relative to the directory you run `amma` from.
Include cycles are detected and ignored.

`lib_math.amma`:
```
nischayikkuka irattikuka(n) { marupadi n * 2 }
```
`main.amma`:
```
ulppeduthuka("lib_math.amma")
parayuka irattikuka(21)      // 42
```

---

## Tooling

### Command-line
```
amma <script.amma>      Run a program
amma                    Start the REPL
amma --tokens <file>    Print the token stream, then run
amma --ast <file>       Print the parsed AST, then run
amma --version          Show version
amma --help             Show usage
```

### REPL
`amma` with no file opens an interactive prompt. Variables and functions persist
across lines; bare expressions print their value. Exit with Ctrl+D (Ctrl+Z on
Windows).

---

## Errors

- **Parse errors** report a source line, e.g.
  `Parse error: Expected '}', got '' (near line 12)`.
- **Runtime errors** print `Runtime error: <message>` and stop the program —
  e.g. division by zero, index out of range, calling an undefined function.

---

## Keyword index

| Malayalam | Manglish | Meaning |
|-----------|----------|---------|
| പറയുക | parayuka | Print |
| സ്വീകരിക്കുക | sweekarikkuka | Read input |
| മുതൽ | muthal | For-loop start |
| വരെ | vare | For-loop end |
| ഈ | ee | While-loop start |
| സത്യമാവണവരെ | sathyamaavanavare | While-loop condition end |
| ഇത് | ithu | If condition start |
| സത്യമോ | sathyamo | If condition end |
| അല്ലെങ്കില് | allenkil | Else |
| നിശ്ചയിക്കുക | nischayikkuka | Define function |
| മറുപടി | marupadi | Return |
| വായിക്കുക | vayikkuka | Read file |
| എഴുതുക | ezhuthuka | Write file |
| കൂട്ടിച്ചേർക്കുക | kootticherkuka | Append to file |
| മുറിക്കുക | murikkuka | Substring |
| നീളം | neelam | Length (string or list) |
| ശരി | sari | Boolean true |
| തെറ്റ് | thettu | Boolean false |
| ശൂന്യം | shoonyam | Null |
| അല്ല | alla | Logical NOT |
| നിർത്തുക | nirthuka | Break |
| തുടരുക | thudaruka | Continue |
| ഓരോ | oro | For-each |
| ഇൽ | il | For-each "in" |
| ഉൾപ്പെടുത്തുക | ulppeduthuka | Import a file |
| കേവലം | kevalam | Absolute value |
| ഘാതം | ghaatham | Power |
| വർഗമൂലം | vargamoolam | Square root |
| കുറഞ്ഞത് | kuranjathu | Minimum |
| കൂടിയത് | koodiyathu | Maximum |
| ക്രമരഹിതം | kramarahitham | Random number |
| സംഖ്യ | samkhya | To integer |
| ദശാംശം | dashaamsham | To float |
| വാചകം | vaachakam | To string |
| വലുത് | valuthu | Uppercase |
| ചെറുത് | cheruthu | Lowercase |
| ഒതുക്കുക | othukkuka | Trim |
| വിഭജിക്കുക | vibhajikkuka | Split |
| മാറ്റുക | maattuka | Replace |
| ഉണ്ടോ | undo | Contains |
| ചേർക്കുക | cherkkuka | List append |
| നീക്കുക | neekkuka | List remove |
