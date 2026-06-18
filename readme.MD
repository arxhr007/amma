<h1 align="center">AMMA programming language & Interpreter</h1>
<p align="center">
    <img src="https://i.ibb.co/gLDYVsJ8/amma.png" alt="amma logo"/>
</p>

**Amma** is an educational programming language written in **Malayalam**, designed primarily as a learning tool for understanding compiler and interpreter design. Despite being created for educational purposes, Amma offers a complete programming experience even like **recursion, file-handling, string operations etc** with Malayalam keywords, making programming more accessible to Malayalam speakers.

> [!note]
> I created this project for me to learn about compilers, lexers, and related concepts. This is a personal educational project to help me understand how programming languages work under the hood.

## Table of Contents

- [Documentation](#documentation)
- [Overview](#overview)
- [Installation](#installation)
- [Language Features](#language-features)
  - [Variables and Data Types](#variables-and-data-types)
  - [Basic Operations](#basic-operations)
  - [Control Structures](#control-structures)
  - [Functions](#functions)
  - [File I/O Operations](#file-io-operations)
  - [String Operations](#string-operations)
  - [Built-in Functions](#built-in-functions)
  - [Tooling](#tooling)
- [Syntax Guide](#syntax-guide)
- [Examples](#examples)
- [Keywords Reference](#keywords-reference)

## Documentation

- [Getting Started](docs/getting-started.md) — install, first program, the REPL
- [Language Reference](docs/language-reference.md) — complete reference: every
  keyword, operator and builtin, in both Malayalam and manglish
- [`samples/`](samples) — runnable examples in both scripts

## Overview

Amma is implemented in Rust, showcasing principles of lexical analysis, parsing, and interpretation. A key feature of Amma is its support for both Malayalam script and Latin transliteration - you can write code using English characters that will be automatically converted to Malayalam keywords. This makes the language accessible even to those who cannot type in Malayalam directly.

## Installation

### Using Pre-built Binaries

Pre-built binaries for Windows and Linux are available in the [GitHub Releases](https://github.com/arxhr007/amma/releases/tag/v1.0.1) section. Simply:

1. Download the appropriate binary for your platform
2. Make it executable (on Linux: `chmod +x amma`) // ignore this for windows
3. Create your Amma script with `.amma` file extension or try one from [samples](https://github.com/arxhr007/amma/tree/main/samples) i have provided
4. Run your script ( for linux in terminal ):
   ```bash
   ./amma your_script.amma
   ```
5. Run your script ( for windows cmd ):
   ```bash
   .\amma your_script.amma
   ```

### Building from Source

If you prefer to build from source, ensure you have the Rust compiler installed on your system:

1. Clone the Amma repository
2. Build the project using Cargo:
   ```bash
   cargo build --release
   ```
3. The compiled binary will be available in the `target/release` directory

## Language Features

### Language file extension 

Amma language used **.amma** file extension , which means the program file should have **.amma** extension ,ex : hello.amma

### Variables and Data Types

Amma supports the following data types:
- **Integers**: Whole numbers, e.g. `19`, `-7`
- **Floats**: Decimal numbers, e.g. `3.14`. Any arithmetic mixing an integer and a float produces a float.
- **Strings**: Text enclosed in single or double quotes
- **Booleans**: `ശരി` / `sari` (true) and `തെറ്റ്` / `thettu` (false). Comparisons and logical operators produce booleans. Booleans still interoperate with numbers (true = 1, false = 0).
- **Null**: `ശൂന്യം` / `shoonyam` — the "nothing" value
- **Lists**: Ordered collections written with square brackets, e.g. `[1, 2, 3]`

Variable assignment is straightforward:

```
name = "Aaron"
age = 19
pi = 3.14
ok = sari            // boolean true
nothing = shoonyam   // null
nums = [10, 20, 30]  // list
```

**Lists** are indexed with `[]` (0-based), measured with `നീളം` / `neelam`, and
support the list builtins described in [Built-in Functions](#built-in-functions):

```
nums = [10, 20, 30]
parayuka nums[1]        // 20
parayuka neelam(nums)   // 3
```

### Basic Operations

Amma provides standard arithmetic and logical operations:

- Addition: `+`
- Subtraction: `-`
- Multiplication: `*`
- Division: `/` (integer division)
- Modulo: `%`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Logical: `&&` (AND), `||` (OR), `!` or `അല്ല` / `alla` (NOT)
- Compound assignment: `+=`, `-=`, `*=`, `/=`, `%=` (e.g. `total += 5`)

**Logical NOT** flips truthiness and returns a boolean:

```
flag = thettu
ithu alla flag sathyamo {
    parayuka "not false"
}
```

### Control Structures

#### Conditional Statements

```
ഇത് condition സത്യമോ {
    // code to execute if true
} അല്ലെങ്കില് {
    // code to execute if false
}
```

Transliterated version (automatically converted to Malayalam):
```
ithu condition sathyamo {
    // code to execute if true
} allenkil {
    // code to execute if false
}
```

**Else-if chains** are formed by following `അല്ലെങ്കില്` / `allenkil` with another
`ഇത്` / `ithu` instead of a block:

```
ithu n == 1 sathyamo {
    parayuka "one"
} allenkil ithu n == 2 sathyamo {
    parayuka "two"
} allenkil {
    parayuka "other"
}
```

#### Loops

**For Loop:**
```
start മുതൽ end വരെ {
    // code to execute
    // Use _ variable for current iteration
}
```

Transliterated:
```
start muthal end vare {
    // code to execute
    // Use _ variable for current iteration
}
```

**While Loop:**
```
ഈ condition സത്യമാവണവരെ {
    // code to execute
}
```

Transliterated:
```
ee condition sathyamaavanavare {
    // code to execute
}
```

**For-each Loop** iterates over the items of a list (or the characters of a
string) with `ഓരോ` / `oro` … `ഇൽ` / `il`:

```
ഓരോ item ഇൽ collection {
    // code to execute, `item` holds each element
}
```

Transliterated:
```
oro item il collection {
    // code to execute, `item` holds each element
}
```

**Break and Continue** control loop flow. `നിർത്തുക` / `nirthuka` exits the loop;
`തുടരുക` / `thudaruka` skips to the next iteration:

```
0 muthal 10 vare {
    ithu _ == 5 sathyamo {
        nirthuka          // stop the loop
    }
    ithu _ % 2 == 0 sathyamo {
        thudaruka         // skip even numbers
    }
    parayuka _
}
```

### Functions

Function definition and calls:

```
നിശ്ചയിക്കുക function_name(parameter1, parameter2) {
    // function body
    മറുപടി result
}
```

Transliterated:
```
nischayikkuka function_name(parameter1, parameter2) {
    // function body
    marupadi result
}
```

### File I/O Operations

Amma provides simple file operations:

**Reading:**
```
content = വായിക്കുക("filename.txt")
```

**Writing:**
```
എഴുതുക(content, "filename.txt")
```

**Appending:**
```
കൂട്ടിച്ചേർക്കുക(content, "filename.txt")
```

### String Operations

**String Concatenation:**
```
full_name = first_name + " " + last_name
```

**String Length:**
```
x= നീളം("hello")
```

**String Indexing:**
```
first_char = name[0]
```

**Substring:**
```
part = name.മുറിക്കുക(0, 5)
```

**Multiplication:**
```
x=  "*" * 10
```

**Escape sequences** are supported inside string literals: `\n` (newline),
`\t` (tab), `\r`, `\\`, `\"`, `\'`.
```
parayuka "line1\nline2\ttabbed"
```

## Built-in Functions

These functions are available everywhere and work with both Malayalam and
manglish names. Builtins cannot be shadowed by user-defined functions.

### Math

| Malayalam | Transliteration | Description |
|-----------|-----------------|-------------|
| കേവലം | `kevalam(x)` | Absolute value |
| ഘാതം | `ghaatham(base, exp)` | Power (`base` raised to `exp`) |
| വർഗമൂലം | `vargamoolam(x)` | Square root (returns a float) |
| കുറഞ്ഞത് | `kuranjathu(a, b, …)` | Minimum of the arguments (or of a single list) |
| കൂടിയത് | `koodiyathu(a, b, …)` | Maximum of the arguments (or of a single list) |
| ക്രമരഹിതം | `kramarahitham(n)` | Random integer in `0 .. n` |

### Type conversion

| Malayalam | Transliteration | Description |
|-----------|-----------------|-------------|
| സംഖ്യ | `samkhya(x)` | Convert to integer |
| ദശാംശം | `dashaamsham(x)` | Convert to float |
| വാചകം | `vaachakam(x)` | Convert to string |

### String / list operations

| Malayalam | Transliteration | Description |
|-----------|-----------------|-------------|
| വലുത് | `valuthu(s)` | Uppercase a string |
| ചെറുത് | `cheruthu(s)` | Lowercase a string |
| ഒതുക്കുക | `othukkuka(s)` | Trim surrounding whitespace |
| വിഭജിക്കുക | `vibhajikkuka(s, sep)` | Split a string into a list by `sep` |
| മാറ്റുക | `maattuka(s, old, new)` | Replace occurrences of `old` with `new` |
| ഉണ്ടോ | `undo(haystack, needle)` | Whether a string/list contains `needle` (returns boolean) |
| ചേർക്കുക | `cherkkuka(list, x)` | Return a new list with `x` appended |
| നീക്കുക | `neekkuka(list, i)` | Return a new list with index `i` removed |

> List operations return a **new** list rather than mutating in place, so reassign
> the result: `nums = cherkkuka(nums, 4)`.

```
parayuka kevalam(-7)              // 7
parayuka ghaatham(2, 10)         // 1024
parayuka vargamoolam(144)        // 12
parayuka valuthu("hello")        // HELLO
parayuka vibhajikkuka("a,b,c", ",")  // [a, b, c]

nums = [1, 2, 3]
nums = cherkkuka(nums, 4)        // [1, 2, 3, 4]
```

## Tooling

### Command-line options

```
amma <script.amma>      Run a program
amma                    Start the interactive REPL
amma --tokens <file>    Print the token stream, then run
amma --ast <file>       Print the parsed AST, then run
amma --version          Show the version
amma --help             Show usage
```

### REPL

Running `amma` with no script starts an interactive prompt. Variables and
function definitions persist across lines, and bare expressions echo their value:

```
amma> x = 21
amma> x * 2
42
```

### Importing other files

`ഉൾപ്പെടുത്തുക` / `ulppeduthuka` splices another `.amma` file in before running.
Paths are resolved relative to where you run `amma`. Include cycles are detected
and ignored.

```
ulppeduthuka("samples/manglish/lib_math.amma")
parayuka irattikuka(21)   // uses a function defined in the imported file
```

### Error messages

Parse errors include the source line number, e.g.
`Parse error: Expected '}', got '' (near line 12)`.

## Syntax Guide

### Comments
```
// This is a single-line comment

/* This is a
   multi-line comment */
```

### Input and Output

**Printing to console:**
```
പറയുക "Hello, World!"
പറയുക variable_name
```

**Taking user input:**
```
സ്വീകരിക്കുക variable_name
```

## Transliteration System

A standout feature of Amma is its transliteration system. You can write code in three ways:

1. **Direct Malayalam**: Using Malayalam script if you have a Malayalam keyboard
2. **English Transliteration**: Using romanized equivalents that will be automatically converted to Malayalam at run type

For example, the print statement can be written in any of these forms:

```
പറയുക "Hello, World!"  // Direct Malayalam
parayuka "Hello, World!"  // English transliteration
```

All keywords will be automatically converted to Malayalam during processing, making the language accessible to everyone regardless of keyboard capabilities or typing preferences.

| Write This (English) | Gets Converted To (Malayalam) |
|---------------------|------------------------------|
| parayuka           | പറയുക                       |
| sweekarikkuka      | സ്വീകരിക്കുക                  |
| muthal             | മുതൽ                         |
| vare               | വരെ                          |
| ithu               | ഇത്                          |
| sathyamo           | സത്യമോ                       |
etc......

## Examples

### Hello World
```
പറയുക "Hello, World!"
```
Or with transliteration:
```
parayuka "Hello, World!"
```

### Simple Calculator
```
parayuka "Simple Calculator"
sweekarikkuka num1
sweekarikkuka num2
parayuka "Sum: " + (num1 + num2)
parayuka "Difference: " + (num1 - num2)
parayuka "Product: " + (num1 * num2)
ithu num2 != 0 sathyamo {
    parayuka "Division: " + (num1 / num2)
} allenkil {
    parayuka "Cannot divide by zero"
}
```

### Fibonacci Sequence
```
നിശ്ചയിക്കുക fib(n) {
    ഇത്  (n == 0) സത്യമോ {
        മറുപടി(0)
    } അല്ലെങ്കില് {
        ഇത്  (n == 1) സത്യമോ{
            മറുപടി(1)
        } അല്ലെങ്കില് {
            മറുപടി(fib(n - 1) + fib(n - 2))
        }
    }
}
സ്വീകരിക്കുക x

0 മുതൽ  x വരെ {
    പറയുക(fib(_))
 }
```

### File Handling
```
// write
ezhuthuka("Aaron Thomas", "out.txt")

// append
കൂട്ടിച്ചേർക്കുക(" build this", "out.txt")

// read
x=vayikkuka("out.txt")
പറയുക x
```

## More Examples are in samples 



## Keywords Reference

| Malayalam Keyword | Transliteration | Function |
|------------------|----------------|----------|
| പറയുക | parayuka | Print to console |
| സ്വീകരിക്കുക | sweekarikkuka | Accept user input |
| മുതൽ | muthal | For loop start |
| വരെ | vare | For loop end |
| ഈ | ee | While loop condition start |
| സത്യമാവണവരെ | sathyamaavanavare | While loop condition end |
| ഇത് | ithu | If statement condition |
| സത്യമോ | sathyamo | If statement condition end |
| അല്ലെങ്കില് | allenkil | Else statement |
| നിശ്ചയിക്കുക | nischayikkuka | Function definition |
| മറുപടി | marupadi | Return statement |
| വായിക്കുക | vayikkuka | Read file |
| എഴുതുക | ezhuthuka | Write file |
| കൂട്ടിച്ചേർക്കുക | kootticherkuka | Append to file |
| മുറിക്കുക | murikkuka | Substring operation |
| നീളം | neelam | Length of a string or list |
| ശരി | sari | Boolean true |
| തെറ്റ് | thettu | Boolean false |
| ശൂന്യം | shoonyam | Null / nothing |
| അല്ല | alla | Logical NOT |
| നിർത്തുക | nirthuka | Break out of a loop |
| തുടരുക | thudaruka | Continue to next loop iteration |
| ഓരോ | oro | For-each loop ("for each") |
| ഇൽ | il | For-each "in" |
| ഉൾപ്പെടുത്തുക | ulppeduthuka | Import another file |
| കേവലം | kevalam | Absolute value |
| ഘാതം | ghaatham | Power |
| വർഗമൂലം | vargamoolam | Square root |
| കുറഞ്ഞത് | kuranjathu | Minimum |
| കൂടിയത് | koodiyathu | Maximum |
| ക്രമരഹിതം | kramarahitham | Random number |
| സംഖ്യ | samkhya | Convert to integer |
| ദശാംശം | dashaamsham | Convert to float |
| വാചകം | vaachakam | Convert to string |
| വലുത് | valuthu | Uppercase |
| ചെറുത് | cheruthu | Lowercase |
| ഒതുക്കുക | othukkuka | Trim whitespace |
| വിഭജിക്കുക | vibhajikkuka | Split string into list |
| മാറ്റുക | maattuka | Replace in string |
| ഉണ്ടോ | undo | Contains check |
| ചേർക്കുക | cherkkuka | Append to list |
| നീക്കുക | neekkuka | Remove from list |



## Educational Purpose

Amma was created primarily as an educational tool for learning about compilers. The language demonstrates:

1. **Lexical Analysis**: Converting source code to tokens
2. **Parsing**: Constructing an Abstract Syntax Tree (AST)
3. **Interpretation**: Executing the parsed AST
4. **Transliteration**: Converting between writing systems

By studying and extending Amma, you can gain insights into language design and implementation.

---

## Contributing

Contributions to Amma are welcome! Feel free to submit issues or pull requests to help improve the language.

<p align="center">Made with ❤️ by <a href="https://github.com/arxhr007">arxhr007</a></p>
<p align="center">If you find this project helpful, please consider giving it a star ⭐</p>

## License

This project is open source and available under the [MIT License](LICENSE).
