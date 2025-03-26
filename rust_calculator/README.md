# Rust Calculator Interpreter

**Name:** Ahmet Hakan Aksu  
**Student ID:** 191101068

This project is a **Rust-based command-line calculator** that supports arithmetic expressions with variable assignments, floating-point numbers, operator precedence, and error handling. It is designed to be lightweight, readable, and fully interactive.

---

## ✅ Features

- Arithmetic operations: `+`, `-`, `*`, `/`, `^`
- Parentheses and nested expressions
- Operator precedence and right-associative exponentiation
- Variable assignment using `=`
- Reusing defined variables in later expressions
- Floating-point and integer support
- Smart output:
  - Whole numbers are displayed as integers (e.g., `5`)
  - Non-integers are shown as floats (e.g., `4.5`)
- Clear error messages for:
  - Division by zero
  - Unknown variables
  - Invalid tokens or characters
  - Unbalanced parentheses
- Continuous prompt (`>>`) with `exit` to quit

---

## Requirements

- Rust toolchain (with `cargo` and `rustc`)

### Install Rust (if not already installed):

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

Then restart your terminal or run:

```bash
source $HOME/.cargo/env
```

---

## How to Compile and Run

1. Navigate to the project directory:

```bash
cd rust_calculator
```

2. Build and run the program:

```bash
cargo run
```

> Alternatively, compile manually:

```bash
rustc src/main.rs -o calculator
./calculator
```

---

## Usage Examples

```text
>> 3 + 5
Result: 8

>> (2 + 3) * 4
Result: 20

>> 2 ^ 3 ^ 2
Result: 512

>> x = 10
x = 10

>> y = x + 2.5
y = 12.5

>> z = (x + y) / 2
z = 11.25

>> z * (x - 5)
Result: 56.25

>> a = ((5 + 3) * (2 + 6)) / (4 - 2)
a = 32

>> b = ((3 + 5 * 2) ^ 2 - (4 / 2)) * (1 + 1)
b = 334

>> ((x + y) ^ 2 - (z * 4)) / (3 + 1)
Result: 115.3125

>> ((2 + 3.5) * (4 - 1)) ^ 2
Result: 272.25
```

---

## ❗ Error Handling Examples

```text
>> 10 / 0
Error: Division by zero!

>> 3 + / 2
Error: Insufficient values in expression.

>> abc + 2
Unknown variable: abc

>> 8 % 3
Invalid token: %

>> (3 + 5
Mismatched parentheses.
```

> If an error occurs, no result is printed, and the program stays active.

---

## Files

```text
Cargo.toml         -- Project metadata and dependencies
Cargo.lock         -- Locked versions of dependencies (important for reproducibility)
src/main.rs        -- Full source code of the calculator
README.md          -- This documentation
```

---

## Notes

- The parser builds an Abstract Syntax Tree (AST) respecting operator precedence.
- Variables are stored using `HashMap<String, f64>`.
