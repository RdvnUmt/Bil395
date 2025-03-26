use std::io;

enum CalculatorError {
    InvalidExpression(String),
    DivisionByZero
}

struct Calculator {}

impl Calculator {
    fn new() -> Calculator {
        Calculator {}
    }
    
    fn evaluate(&self, expression: &str) -> Result<f64, CalculatorError> {
        self.parse_expression(expression)
    }
    
    fn parse_expression(&self, expression: &str) -> Result<f64, CalculatorError> {
        let expression = expression.trim();
        
        if expression.is_empty() {
            return Err(CalculatorError::InvalidExpression("Boş ifade".to_string()));
        }
        
        let expression: String = expression.chars().filter(|c| !c.is_whitespace()).collect();
        
        if !expression.contains('+') && !expression.contains('-') && 
           !expression.contains('*') && !expression.contains('/') && 
           !expression.contains('(') && !expression.contains(')') {
            
            if let Ok(number) = expression.parse::<f64>() {
                return Ok(number);
            }
            
            return Err(CalculatorError::InvalidExpression(format!("Geçersiz sayı: {}", expression)));
        }
        
        if expression.starts_with('(') && expression.ends_with(')') {
            return self.parse_expression(&expression[1..expression.len()-1]);
        }
        
        let mut operator_pos = None;
        let mut parenthesis_level = 0;
        
        for (i, c) in expression.chars().enumerate() {
            match c {
                '(' => parenthesis_level += 1,
                ')' => parenthesis_level -= 1,
                '+' | '-' => {
                    if parenthesis_level == 0 && i > 0 {
                        let prev = expression.chars().nth(i-1).unwrap();
                        if prev.is_digit(10) || prev == ')' {
                            operator_pos = Some(i);
                            break;
                        }
                    }
                },
                _ => {}
            }
        }
        
        if operator_pos.is_none() {
            parenthesis_level = 0;
            
            for (i, c) in expression.chars().enumerate() {
                match c {
                    '(' => parenthesis_level += 1,
                    ')' => parenthesis_level -= 1,
                    '*' | '/' => {
                        if parenthesis_level == 0 {
                            operator_pos = Some(i);
                            break;
                        }
                    },
                    _ => {}
                }
            }
        }
        
        if let Some(pos) = operator_pos {
            let left_expr = &expression[0..pos];
            let left_value = self.parse_expression(left_expr)?;
            
            let right_expr = &expression[pos+1..];
            let right_value = self.parse_expression(right_expr)?;
            
            match expression.chars().nth(pos).unwrap() {
                '+' => return Ok(left_value + right_value),
                '-' => return Ok(left_value - right_value),
                '*' => return Ok(left_value * right_value),
                '/' => {
                    if right_value == 0.0 {
                        return Err(CalculatorError::DivisionByZero);
                    }
                    return Ok(left_value / right_value);
                },
                _ => return Err(CalculatorError::InvalidExpression("Bilinmeyen operatör".to_string()))
            }
        }
        
        Err(CalculatorError::InvalidExpression(format!("İfade anlaşılamadı: {}", expression)))
    }
}

fn main() {
    let calculator = Calculator::new();
    println!("Çıkmak için 'exit' yazın");
    
    loop {
        print!("-> ");
      
        let mut input = String::new();
        io::stdin().read_line(&mut input).expect("Girdi okunamadı");
        
        let input = input.trim();
        
        if input.is_empty() {
            continue;
        }
        
        match input {
            "exit" => break,
            _ => {
                match calculator.evaluate(input) {
                    Ok(result) => println!("= {}", result),
                    Err(error) => {
                        match error {
                            CalculatorError::InvalidExpression(message) => println!("Hata: {}", message),
                            CalculatorError::DivisionByZero => println!("Hata: Sıfıra bölme işlemi yapılamaz"),
                        }
                    }
                }
            }
        }
    }
}