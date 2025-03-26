# Prolog Calculator Interpreter

**Name:** Ahmet Hakan Aksu  
**Student ID:** 191101068

This directory contains the **Prolog implementation** of a simple calculator interpreter. It supports arithmetic operations, variable assignments, operator precedence, and includes essential error handling.

---

## ✅ Features

- Arithmetic operations: `+`, `-`, `*`, `/`, `^`
- Parentheses support: handles nested and grouped expressions
- Exponentiation operator: `^` (right-associative)
- Variable assignment using `=`
- Variable reuse in later expressions
- Integer and floating-point number support
- Robust error handling for:
  - Division by zero
  - Undefined variables
  - Syntax errors (e.g., unmatched parentheses or invalid operators)
- Clean output formatting:
  - Integers are displayed as-is
  - Floats are displayed with decimal point precision
  - Errors do not show results
- Continuous prompt with `>>`
- Sometimes, especially when typing long or nested expressions involving parentheses, some characters may **not appear visibly** on the terminal while typing. This is a display quirk, but your input is **still correctly received** and evaluated.
- Type `exit.` to quit

---

## Requirements

- **SWI-Prolog**

### Install on Ubuntu:

```bash
sudo apt update
sudo apt install swi-prolog
```

---

## How to Run

1. Open terminal and navigate to the `prolog/` directory.

2. Launch the Prolog interpreter:

```bash
swipl
```

3) Load the calculator file inside the Prolog console:

```bash
?- [calculator].
```

4. Start the interpreter:

```prolog
?- start.
```

---

## Usage Examples

- ❗❗❗Always end your expressions with a **period (`.`)**.

-❗❗❗Sometimes, especially when typing long or nested expressions involving parentheses, some characters may **not appear visibly** on the terminal while typing. This is a display quirk, but your input is **still correctly received** and evaluated.


```text
>> 3 + 5.
Result: 8

>> 2 * (4 + 3).
Result: 14

>> 2 ^ 3 ^ 2.
Result: 512

>> x = 5.
x = 5

>> y = x + 3.5.
y = 8.5

>> y * 2.
Result: 17

>> z = (x + y) / 2.
z = 6.75

>> z + x * 3 - (2 ^ 2).
Result: 19.75

>> big = ((5 + 3) * (2 + 6)) / (4 - 2).
big = 32

>> result = ((3 + 5 * 2) ^ 2 - (4 / 2)) * (1 + 1).
result = 334
```

---

## Error Handling Examples

```text
>> 10 / 0.
Error: Division by zero.

>> (3 + 5.
Error: expected ')'

>> abc + 2.
Error: unknown variable: abc
```

## Files

```text
calculator.pl     -- Prolog source code
README.md         -- This documentation
```

---

## Notes

- Input expressions must be terminated with a period (`.`), as per standard Prolog syntax.
- The program uses DCG rules to parse the expressions and recursively evaluate them.
- The symbol table (for storing variable values) is managed dynamically.
- To exit the calculator, type `exit.` and press Enter.
