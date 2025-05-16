<h1 align="center">AMMA programming language & Interpreter</h1>
<p align="center">
    <img src="https://i.ibb.co/gLDYVsJ8/amma.png" alt="amma logo"/>
</p>

**Amma** is an educational programming language written in **Malayalam**, designed primarily as a learning tool for understanding compiler and interpreter design. Despite being created for educational purposes, Amma offers a complete programming experience even like **recursion, file-handling, string operations etc** with Malayalam keywords, making programming more accessible to Malayalam speakers.

> [!note]
> I created this project for me to learn about compilers, lexers, and related concepts. This is a personal educational project to help me understand how programming languages work under the hood.

## Table of Contents

- [Overview](#overview)
- [Installation](#installation)
- [Language Features](#language-features)
  - [Variables and Data Types](#variables-and-data-types)
  - [Basic Operations](#basic-operations)
  - [Control Structures](#control-structures)
  - [Functions](#functions)
  - [File I/O Operations](#file-io-operations)
  - [String Operations](#string-operations)
- [Syntax Guide](#syntax-guide)
- [Examples](#examples)
- [Keywords Reference](#keywords-reference)

## Overview

Amma is implemented in Rust, showcasing principles of lexical analysis, parsing, and interpretation. A key feature of Amma is its support for both Malayalam script and Latin transliteration - you can write code using English characters that will be automatically converted to Malayalam keywords. This makes the language accessible even to those who cannot type in Malayalam directly.

## Installation

### Using Pre-built Binaries

Pre-built binaries for Windows and Linux are available in the [GitHub Releases](https://github.com/arxhr007/amma/releases/tag/v1.0.0) section. Simply:

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
- **Numbers**: Integers are the primary number type
- **Strings**: Text enclosed in single or double quotes
- **Boolean**: Represented as 1 (true) and 0 (false)

Variable assignment is straightforward:

```
name = "Aaron"
age = 19
```

### Basic Operations

Amma provides standard arithmetic and logical operations:

- Addition: `+`
- Subtraction: `-`
- Multiplication: `*`
- Division: `/` (integer division)
- Modulo: `%`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Logical: `&&` (AND), `||` (OR)

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
