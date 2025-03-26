# ADA Calculator Interpreter

**Name:** Ahmet Hakan Aksu  
**Student ID:** 12345678

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

## 🛠 Requirements

- GNAT Ada Compiler (includes `gnatmake`)
- Tested on GNAT version **13.3.0**

### Install on Ubuntu:

```bash
sudo apt update
sudo apt install gnat
