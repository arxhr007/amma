# Getting Started with Amma

Amma (അമ്മ) is an educational programming language with Malayalam keywords. You
write code in either **native Malayalam script** or **manglish** (Latin
transliteration) — both compile to the same program.

## Install

### Pre-built binary
Download from [Releases](https://github.com/arxhr007/amma/releases), then:

```bash
# Linux
chmod +x amma
./amma hello.amma
```
```powershell
# Windows
.\amma.exe hello.amma
```

### Build from source
Requires the Rust toolchain (and a working linker — on Windows, the
"Desktop development with C++" workload from Visual Studio Build Tools).

```bash
cargo build --release
# binary at target/release/amma
```

## Your first program

Create `hello.amma`:

```
parayuka "Hello, World!"
```

Run it:

```bash
./amma hello.amma
```

The same program in native Malayalam:

```
പറയുക "Hello, World!"
```

## The REPL

Run `amma` with no file to start an interactive session. State persists across
lines, and bare expressions print their value:

```
$ ./amma
Amma REPL — type code; Ctrl+D (Ctrl+Z on Windows) to exit
amma> x = 21
amma> x * 2
42
amma> nischayikkuka sq(n) { marupadi n * n }
amma> sq(9)
81
```

## A bigger example

```
// sum the even numbers from 1..n, skipping odds with `thudaruka`
sweekarikkuka n

total = 0
1 muthal n vare {
    ithu _ % 2 != 0 sathyamo {
        thudaruka
    }
    total += _
}
parayuka "Sum of evens: " + total
```

## Where next

- [Language Reference](language-reference.md) — every keyword, operator and
  builtin, with both Malayalam and manglish forms.
- [`samples/`](../samples) — runnable example programs in both scripts.
