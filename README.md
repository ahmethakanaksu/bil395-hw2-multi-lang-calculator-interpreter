# Programming Languages - Calculator Interpreters

**Author:** Ahmet Hakan Aksu  
**Student ID:** 191101068

This repository contains simple calculator interpreter implementations in **five different programming languages**, developed as part of the Programming Languages course assignment.

Each interpreter supports:
- Basic arithmetic operations (`+`, `-`, `*`, `/`)
- Parentheses and operator precedence
- Variable assignment
- Floating-point number support
- Error handling (invalid input, division by zero, etc.)

---

## Project Structure

| Language | Folder | Description |
|----------|--------|-------------|
| 🦀 Rust | [`rust_calculator`](./rust_calculator) | Full-featured Rust CLI calculator with AST and variable memory |
| 🐚 Scheme | [`scheme`](./scheme) | R5RS-compliant interpreter using `chezscheme` with variable and float support |
| 💎 Perl | [`perl`](./perl) | Token-based Perl calculator with error handling and variables |
| 🧠 Prolog | [`prolog`](./prolog) | SWI-Prolog REPL-based calculator using custom parser |
| ⚙️ Ada | [`ada`](./ada) | Recursive descent parser + evaluator using Ada’s strong typing |

---

## How to Use

Each folder contains:
- The source code (`.rs`, `.pl`, `.scm`, `.adb`, etc.)
- A dedicated `README.md` with:
  - Compilation/run instructions
  - Usage examples
  - Error cases

You can go into any folder and follow the instructions in its README to try it out!
