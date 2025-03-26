use std::collections::HashMap;
use std::io::{self, Write};

fn precedence(op: &str) -> i32 {
    match op {
        "+" | "-" => 1,
        "*" | "/" => 2,
        "^" => 3,
        _ => 0,
    }
}

fn is_operator(token: &str) -> bool {
    token == "+" || token == "-" || token == "*" || token == "/" || token == "^"
}

fn apply_operator(op: &str, b: f64, a: f64) -> Result<f64, String> {
    match op {
        "+" => Ok(a + b),
        "-" => Ok(a - b),
        "*" => Ok(a * b),
        "/" => {
            if b == 0.0 {
                Err("Error: Division by zero!".to_string())
            } else {
                Ok(a / b)
            }
        }
        "^" => Ok(a.powf(b)),
        _ => Err(format!("Unknown operator: {}", op)),
    }
}

fn is_number(s: &str) -> bool {
    s.parse::<f64>().is_ok()
}

fn get_value(token: &str, vars: &HashMap<String, f64>) -> Result<f64, String> {
    if is_number(token) {
        Ok(token.parse::<f64>().unwrap())
    } else if let Some(v) = vars.get(token) {
        Ok(*v)
    } else {
        Err(format!("Unknown variable: {}", token))
    }
}

fn tokenize(expr: &str) -> Vec<String> {
    let mut tokens = vec![];
    let mut current = String::new();
    let mut last_type: Option<char> = None;

    for c in expr.chars() {
        if c.is_whitespace() {
            if !current.is_empty() {
                tokens.push(current.clone());
                current.clear();
                last_type = None;
            }
            continue;
        }

        if "+-*/^()".contains(c) {
            if !current.is_empty() {
                tokens.push(current.clone());
                current.clear();
            }
            tokens.push(c.to_string());
            last_type = None;
        } else if c.is_ascii_digit() || c == '.' {
            if last_type == Some('d') || last_type.is_none() {
                current.push(c);
                last_type = Some('d');
            } else {
                tokens.push(current.clone());
                current.clear();
                current.push(c);
                last_type = Some('d');
            }
        } else if c.is_ascii_alphabetic() {
            if last_type == Some('a') || last_type.is_none() {
                current.push(c);
                last_type = Some('a');
            } else {
                tokens.push(current.clone());
                current.clear();
                current.push(c);
                last_type = Some('a');
            }
        } else {
            // Unexpected character
            if !current.is_empty() {
                tokens.push(current.clone());
                current.clear();
            }
            tokens.push(c.to_string());
            last_type = None;
        }
    }

    if !current.is_empty() {
        tokens.push(current);
    }

    tokens
}

fn infix_to_postfix(tokens: Vec<String>) -> Result<Vec<String>, String> {
    let mut output = vec![];
    let mut stack = vec![];

    for token in tokens {
        if is_number(&token) || token.chars().all(|c| c.is_alphabetic()) {
            output.push(token);
        } else if token == "(" {
            stack.push(token);
        } else if token == ")" {
            while let Some(top) = stack.pop() {
                if top == "(" {
                    break;
                } else {
                    output.push(top);
                }
            }
        } else if is_operator(&token) {
            while let Some(top) = stack.last() {
                if is_operator(top)
                    && ((precedence(top) > precedence(&token))
                        || (precedence(top) == precedence(&token) && token != "^"))
                {
                    output.push(stack.pop().unwrap());
                } else {
                    break;
                }
            }
            stack.push(token);
        } else {
            return Err(format!("Invalid token: {}", token));
        }
    }

    while let Some(top) = stack.pop() {
        if top == "(" || top == ")" {
            return Err("Mismatched parentheses.".to_string());
        }
        output.push(top);
    }

    Ok(output)
}

fn eval_postfix(postfix: Vec<String>, vars: &HashMap<String, f64>) -> Result<f64, String> {
    let mut stack: Vec<f64> = vec![];

    for token in postfix {
        if is_number(&token) || token.chars().all(|c| c.is_alphabetic()) {
            let val = get_value(&token, vars)?;
            stack.push(val);
        } else if is_operator(&token) {
            if stack.len() < 2 {
                return Err("Insufficient values in expression.".to_string());
            }
            let b = stack.pop().unwrap();
            let a = stack.pop().unwrap();
            let result = apply_operator(&token, b, a)?;
            stack.push(result);
        } else {
            return Err(format!("Invalid token in postfix: {}", token));
        }
    }

    if stack.len() != 1 {
        return Err("Invalid expression.".to_string());
    }

    Ok(stack[0])
}

fn eval_expr(expr: &str, vars: &HashMap<String, f64>) -> Result<f64, String> {
    let tokens = tokenize(expr);
    let postfix = infix_to_postfix(tokens)?;
    eval_postfix(postfix, vars)
}

fn main() {
    let mut vars: HashMap<String, f64> = HashMap::new();
    println!("Rust Calculator Interpreter (Interactive Mode)");
    println!("Supports +, -, *, /, ^ and variable assignment like: x = (2 + 3) * 4");
    println!("Type 'exit' to quit.\n");

    loop {
        print!(">> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        if io::stdin().read_line(&mut input).is_err() {
            println!("Failed to read input.");
            continue;
        }

        let input = input.trim();
        if input.is_empty() {
            continue;
        }

        if input == "exit" {
            break;
        }

        if input.contains('=') {
            let parts: Vec<&str> = input.split('=').collect();
            if parts.len() != 2 {
                println!("Invalid assignment syntax. Use: x = expression");
                continue;
            }

            let var = parts[0].trim().to_string();
            let expr = parts[1].trim();

            if !var.chars().all(|c| c.is_alphabetic()) {
                println!("Invalid variable name: {}", var);
                continue;
            }

            match eval_expr(expr, &vars) {
                Ok(res) => {
                    vars.insert(var.clone(), res);
                    println!("{} = {}", var, res);
                }
                Err(e) => println!("{}", e),
            }
        } else {
            match eval_expr(input, &vars) {
                Ok(res) => println!("Result: {}", res),
                Err(e) => println!("{}", e),
            }
        }
    }
}
