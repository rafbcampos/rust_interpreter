use logos::{Lexer, Logos};

use crate::lexer::Token;

#[derive(Debug)]
enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
}

#[derive(Debug)]
struct BinaryOperator {
    left: Box<Expression>,
    operator: Operator,
    right: Box<Expression>,
}

#[derive(Debug)]
enum Literal {
    Number(i64),
    String(String),
    Boolean(bool),
    Array(Vec<Expression>),
}

#[derive(Debug)]
struct Let {
    name: String,
    value: Box<Expression>,
}

#[derive(Debug)]
struct Block {
    expressions: Vec<Expression>,
}

#[derive(Debug)]
struct Return {
    value: Box<Expression>,
}

#[derive(Debug)]
enum Expression {
    BinaryOperator(BinaryOperator),
    Literal(Literal),
    Let(Let),
    Block(Block),
    Return(Return),
}

#[derive(Debug)]
pub struct Program {
    expressions: Vec<Expression>,
}

type Parser = fn(&Token, &mut Lexer<Token>, &mut Vec<Expression>) -> Option<Expression>;

fn chain_pasers(token: &Token, lexer: &mut Lexer<Token>, expressions: &mut Vec<Expression>, parsers: Vec<Parser>) -> Option<Expression> {
   let mut expression: Option<Expression> = None;
   for parser in parsers {
       expression = parser(token, lexer, expressions);
       if expression.is_some() {
           break;
       }
   } 
   expression
}

fn parse_number(token: &Token, _lexer: &mut Lexer<Token>, _expressions: &mut Vec<Expression>)  -> Option<Expression> {
    if let Token::Number(value) = token {
        Some(Expression::Literal(Literal::Number(*value)))
    } else {
        None
    }
}

fn parse_string(token: &Token, _lexer: &mut Lexer<Token>, _expressions: &mut Vec<Expression>)  -> Option<Expression> {
    if let Token::Ident(value) = token {
        Some(Expression::Literal(Literal::String(value.to_string())))
    } else {
        None
    }
}

fn parse_boolean(token: &Token, _lexer: &mut Lexer<Token>, _expressions: &mut Vec<Expression>)  -> Option<Expression> {
    match token {
        Token::True => Some(Expression::Literal(Literal::Boolean(true))),
        Token::False => Some(Expression::Literal(Literal::Boolean(false))),
        _ => None,
    }
}

fn get_tokens_until_end_of_block(lexer: &mut Lexer<Token>) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut depth = 0;
    loop {
        let token = lexer.next();
        match token {
            Some(Token::LBrace) => depth += 1,
            Some(Token::RBrace) => {
                if depth == 0 {
                    break;
                } else {
                    depth -= 1;
                }
            },
            Some(_) => (),
            None => panic!("Unexpected end of file"),
        }
        tokens.push(token.unwrap());
    }
    tokens
}

fn get_tokens_until_end_of_array(lexer: &mut Lexer<Token>) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut depth = 0;
    loop {
        let token = lexer.next();
        match token {
            Some(Token::LBracket) => depth += 1,
            Some(Token::RBracket) => {
                if depth == 0 {
                    break;
                } else {
                    depth -= 1;
                }
            },
            Some(tk) => tokens.push(tk),
            None => panic!("Unexpected end of file"),
        }
    }
    tokens
}

fn parse_array(token: &Token, lexer: &mut Lexer<Token>, expressions: &mut Vec<Expression>)  -> Option<Expression> {
    if let Token::LBracket = token {
        let tokens = get_tokens_until_end_of_array( lexer);
        let mut expressions = Vec::new();
        for token in tokens {
            let expression = chain_pasers(&token, lexer, &mut expressions, vec![parse_array, parse_number, parse_string, parse_boolean, parse_binary_operator]);
            if let Some(expression) = expression {
                expressions.push(expression);
            }
        } 
        Some(Expression::Literal(Literal::Array(expressions)))
    } else {
        None
    }
}

fn parse_binary_operator(token: &Token, lexer: &mut Lexer<Token>, expressions: &mut Vec<Expression>)  -> Option<Expression> {
    match token {
        Token::Plus => Some(Expression::BinaryOperator(BinaryOperator {
            left: Box::new(expressions.pop().unwrap()),
            operator: Operator::Plus,
            right: Box::new(chain_pasers(&lexer.next().unwrap(), lexer, expressions, vec![parse_array, parse_number, parse_string, parse_boolean, parse_binary_operator]).unwrap()),
        })),
        Token::Minus => Some(Expression::BinaryOperator(BinaryOperator {
            left: Box::new(expressions.pop().unwrap()),
            operator: Operator::Minus,
            right: Box::new(chain_pasers(&lexer.next().unwrap(), lexer, expressions, vec![parse_array, parse_number, parse_string, parse_boolean, parse_binary_operator]).unwrap()),
        })),
        Token::Asterisk => Some(Expression::BinaryOperator(BinaryOperator {
            left: Box::new(expressions.pop().unwrap()),
            operator: Operator::Multiply,
            right: Box::new(chain_pasers(&lexer.next().unwrap(), lexer, expressions, vec![parse_array, parse_number, parse_string, parse_boolean, parse_binary_operator]).unwrap()),
        })),
        Token::Slash => Some(Expression::BinaryOperator(BinaryOperator {
            left: Box::new(expressions.pop().unwrap()),
            operator: Operator::Divide,
            right: Box::new(chain_pasers(&lexer.next().unwrap(), lexer, expressions, vec![parse_array, parse_number, parse_string, parse_boolean, parse_binary_operator]).unwrap()),
        })),
        Token::Equal => Some(Expression::BinaryOperator(BinaryOperator {
            left: Box::new(expressions.pop().unwrap()),
            operator: Operator::Equal,
            right: Box::new(chain_pasers(&lexer.next().unwrap(), lexer, expressions, vec![parse_array, parse_number, parse_string, parse_boolean, parse_binary_operator]).unwrap()),
        })),
        Token::NotEqual => Some(Expression::BinaryOperator(BinaryOperator {
            left: Box::new(expressions.pop().unwrap()),
            operator: Operator::NotEqual,
            right: Box::new(chain_pasers(&lexer.next().unwrap(), lexer, expressions, vec![parse_array, parse_number, parse_string, parse_boolean, parse_binary_operator]).unwrap()),
        })),
        Token::LessThan => Some(Expression::BinaryOperator(BinaryOperator {
            left: Box::new(expressions.pop().unwrap()),
            operator: Operator::LessThan,
            right: Box::new(chain_pasers(&lexer.next().unwrap(), lexer, expressions, vec![parse_array, parse_number, parse_string, parse_boolean, parse_binary_operator]).unwrap()),
        })),
        Token::GreaterThan => Some(Expression::BinaryOperator(BinaryOperator {
            left: Box::new(expressions.pop().unwrap()),
            operator: Operator::GreaterThan,
            right: Box::new(chain_pasers(&lexer.next().unwrap(), lexer, expressions, vec![parse_array, parse_number, parse_string, parse_boolean, parse_binary_operator]).unwrap()),
        })),
        Token::LessThanEqual => Some(Expression::BinaryOperator(BinaryOperator {
            left: Box::new(expressions.pop().unwrap()),
            operator: Operator::LessThanEqual,
            right: Box::new(chain_pasers(&lexer.next().unwrap(), lexer, expressions, vec![parse_array, parse_number, parse_string, parse_boolean, parse_binary_operator]).unwrap()),
        })),
        Token::GreaterThanEqual => Some(Expression::BinaryOperator(BinaryOperator {
            left: Box::new(expressions.pop().unwrap()),
            operator: Operator::GreaterThanEqual,
            right: Box::new(chain_pasers(&lexer.next().unwrap(), lexer, expressions, vec![parse_array, parse_number, parse_string, parse_boolean, parse_binary_operator]).unwrap()),
        })),
        _ => None,
    }
}

pub fn parser(input: &str) -> Program {
    let mut lexer = Token::lexer(input);
    let mut expressions = Vec::new();
    loop {
        let token = lexer.next();
        match token {
            Some(token) => {
                let expression = chain_pasers(&token, &mut lexer, &mut expressions, vec![parse_array, parse_number, parse_string, parse_boolean, parse_binary_operator]);
                if let Some(expression) = expression {
                    expressions.push(expression);
                }
            },
            None => break,
        }
    }
    Program { expressions }
}

#[test]
fn parses_number() {
    let program = parser("1");

    assert_eq!(program.expressions.len(), 1);
    
    if let Expression::Literal(Literal::Number(value)) = &program.expressions[0] {
        assert_eq!(*value, 1);
    } else {
        println!("{:?}", program.expressions[0]);
        panic!("Unexpected expression");
    }
}

#[test]
fn parses_true() {
    let program = parser("true");

    assert_eq!(program.expressions.len(), 1);
    
    if let Expression::Literal(Literal::Boolean(value)) = &program.expressions[0] {
        assert!(*value);
    } else {
        println!("{:?}", program.expressions[0]);
        panic!("Unexpected expression");
    }
}
