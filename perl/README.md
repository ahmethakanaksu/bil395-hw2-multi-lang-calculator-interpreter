# Perl Calculator Interpreter

**Name:** Ahmet Hakan Aksu  
**Student ID:** 191101068

This directory contains the **Perl implementation** of a calculator interpreter for arithmetic expressions and variable assignments. It includes support for floating-point numbers, operator precedence, and error handling, all in an interactive CLI environment.

---

## ✅ Features

- Arithmetic operations: `+`, `-`, `*`, `/`, `^`
- Parentheses support for grouped and nested expressions
- Exponentiation with `^` (right-associative)
- Variable assignment using `=`
- Variables can be reused in subsequent expressions
- Floating-point and integer number support
- Proper operator precedence is respected
- Error detection for:
  - Division by zero
  - Invalid tokens
  - Undefined variables
  - Syntax errors
- No result is shown if an error is detected
- Type `exit` to quit

---

## Requirements

- **Perl** (default installed on most Unix-based systems)

### To check if Perl is installed:

```bash
perl -v
```

If not installed:

```bash
sudo apt update
sudo apt install perl
```

---

## How to Run

1. Open terminal and navigate to the `perl/` directory.

2. Run the interpreter:

```bash
perl calculator.pl
```

You’ll see the interactive prompt:

```text
>> 
```

Start typing expressions or assignments.

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

>> 3 + * 7
Syntax Error: Insufficient values.

>> abc + 2
Unknown variable: ABC

>> 5 % 7
Syntax Error: Invalid token '%'
```

> The interpreter will not print a result if an error occurs, and it remains usable.

---

## ⚠️ Notes

- All inputs must be in **standard arithmetic notation**:
  - ✅ `x = 5 + 3 * 2`
  - ✅ `(3 + 5) * 2`
- Variable names must begin with a letter and can include digits
- Whitespace is optional and flexible
- Exponentiation binds stronger than multiplication/division

---

## Files

```text
calculator.pl     -- Perl source code
README.md         -- This documentation
```
