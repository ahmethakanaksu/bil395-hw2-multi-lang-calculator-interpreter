# ADA Calculator Interpreter

**Name:** Ahmet Hakan Aksu  
**Student ID:** 191101068

This directory contains the **ADA implementation** of a calculator interpreter for arithmetic expressions with support for variables, floating-point numbers, and error handling. It is part of the Programming Languages course assignment.

---

## ✅ Features

- Basic arithmetic operations: `+`, `-`, `*`, `/`, `^`
- Supports **parentheses** for grouping expressions
- **Exponentiation** (`^`) is right-associative (`2 ^ 3 ^ 2 = 512`)
- **Floating-point support**:
  - Accepts numbers like `3.14`, `5.0`
  - Shows integer results without decimal places
  - Shows floating-point results with 6 decimal precision
- **Variable assignment** using `=`
- **Variable reuse** in expressions
- Detailed **error handling**:
  - Division by zero
  - Missing closing parenthesis
  - Undefined variable usage
  - Syntax errors
- **Clean user experience**:
  - If an error occurs, result is not printed
  - Program continues after errors
  - Type `q` to quit the program

---

## Requirements

- GNAT Ada Compiler (includes `gnatmake`)
- Tested on GNAT version **13.3.0**

### Install on Ubuntu:

```bash
sudo apt update
sudo apt install gnat
```

## How to Compile and Run

1. Place yourself inside the ada/ directory.
2. Compile the code:

```bash
gnatmake calculator.adb
```

3. Run the executable:

```bash
./calculator
```

---

## 💻 Usage Examples

```text
>> 3 + 5
= 8

>> 2 * (4 + 3)
= 14

>> 2 ^ 3 ^ 2
= 512

>> x = 5
x = 5

>> y = x + 3.5
y = 8.5

>> y * 2
= 17

>> z = (x + y) / 2
z = 6.75

>> z + x * 3 - (2 ^ 2)
= 19.75

>> big = ((5 + 3) * (2 + 6)) / (4 - 2)
big = 32

```

---

## ❗ Error Handling Examples

```text
>> 10 / 0
Error: Division by zero.

>> (3 + 5
Error: expected ')'

>> a + 2
Error: unknown variable 'a'

>> 3 + %
Error: Unexpected character '%'
```

If an error is detected, no result is shown. The calculator remains usable for the next input.

---

## 📁 Files

```text
calculator.adb    -- Source code
README.md         -- Documentation (this file)
```

---

## 📌 Notes

- The parser uses **recursive descent** technique.
- Expression tree is built and evaluated recursively.
- Variables are stored in a hash map using Ada’s built-in containers.
