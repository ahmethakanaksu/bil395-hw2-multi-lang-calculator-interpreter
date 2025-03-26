# Scheme Calculator Interpreter

**Name:** Ahmet Hakan Aksu  
**Student ID:** 191101068

This directory contains the **Scheme implementation** of a simple calculator interpreter. It allows basic arithmetic operations, variable definitions, floating-point calculations, and robust error handling — all in an interactive shell-like environment.

---

## ✅ Features

- Arithmetic operations: `+`, `-`, `*`, `/`, `expt` (for exponentiation)
- Parentheses and nested expressions
- Variable definitions using `(define x 5)`
- Variable reuse in expressions: `(+ x 3)`
- Integer and floating-point support
- Error detection:
  - Division by zero
  - Undefined variables
  - Unmatched or missing parentheses
  - Invalid operators
- Smart result display:
  - Integers are shown without decimals
  - Floats are printed normally
- Type `exit` to quit

---

## 🛠 Requirements

- **Chez Scheme** interpreter (or another R6RS/R7RS-compatible Scheme)

### Install on Ubuntu:

```bash
sudo apt update
sudo apt install chezscheme
```

---

## How to Run

1. Open terminal and navigate to the `scheme/` directory.
2. Run the interpreter:

```bash
chezscheme --script calculator.scm
```

You will see:

```text
Scheme Calculator (enter 'exit' to quit)
>>
```

Now you're ready to type expressions.

---

## ⚠️ Usage Notes

- All expressions must be valid **Scheme S-expressions**.
- Always use **prefix notation**:

  ✅ `(+ 3 5)`  
  ✅ `(* (+ 2 3) 4)`  
  ✅ `(define x 9)`  
  ❌ `3 + 5`

- The calculator reads from input line-by-line. Unbalanced parentheses will raise an error like `Error: expected ')'`.

---

## Usage Examples

```text
>> (+ 3 5)
Result: 8

>> (* (- 7 2) (+ 3 1))
Result: 20

>> (define x 5)
x = 5

>> x
Result: 5

>> (+ x 3.5)
Result: 8.5

>> (/ (* (+ x 4) (- x 2)) (+ x 1))
Result: 4.5

>> (expt 2 (expt 3 2))
Result: 512

>> (define y (+ x 10))
y = 15

>> (* y 2)
Result: 30
```

---

## ❗ Error Handling Examples

```text
>> (/ 10 0)
Error: Division by zero!

>> (+ abc 5)
Error: Unknown variable: abc

>> (% 10 3)
Error: Invalid operator: %
```

If an error occurs, no result is printed and the interpreter remains active.

---

## Files

```text
calculator.scm    -- Scheme source code
README.md         -- Documentation
```

---

## Notes

- Built with standard Scheme syntax using lists and basic evaluator logic.
- Does not require SRFI libraries; works with base Scheme.
- All results are auto-formatted:
  - If the result is a whole number, it prints as an integer (e.g., `5`)
  - If the result is a float, it prints with decimals (e.g., `4.5`)

---
