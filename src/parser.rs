use logos::{Lexer, Logos};

use crate::lexer::Token;

// Data types ======================================================================================

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
struct BinaryOperator {
    left: Box<Expression>,
    operator: Operator,
    right: Box<Expression>,
}

#[derive(Debug, Clone)]
enum Literal {
    Number(i64),
    String(String),
    Boolean(bool),
    Array(Vec<Expression>),
}

#[derive(Debug, Clone)]
struct Let {
    name: String,
    value: Box<Expression>,
}

#[derive(Debug, Clone)]
struct Block {
    expressions: Vec<Expression>,
}

#[derive(Debug, Clone)]
struct Return {
    value: Box<Expression>,
}

#[derive(Debug, Clone)]
enum Expression {
    Call,
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

// Helpers =========================================================================================

const PARSERS: [Parser; 7] = [parse_array, parse_number, parse_string, parse_boolean, parse_binary_operator, parse_parens, parse_let];

fn chain_pasers(token: &Token, lexer: &mut Lexer<Token>, expressions: &mut Vec<Expression>) -> Option<Expression> {
   let mut expression: Option<Expression> = None;
   for parser in PARSERS {
       expression = parser(token, lexer, expressions);
       if expression.is_some() {
           break;
       }
   } 
   expression
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

// Parsers =========================================================================================

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


fn parse_array(token: &Token, lexer: &mut Lexer<Token>, expressions: &mut Vec<Expression>)  -> Option<Expression> {
    if let Token::LBracket = token {
        let tokens = get_tokens_until_end_of_array( lexer);
        let mut expressions = Vec::new();
        for token in tokens {
            let expression = chain_pasers(&token, lexer, &mut expressions);
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
    // TODO: rething this logic, it doesn't work for expressions like let a = 1 + 2
    match token {
        Token::Plus => Some(Expression::BinaryOperator(BinaryOperator {
            left: Box::new(expressions.pop().unwrap()),
            operator: Operator::Plus,
            right: Box::new(chain_pasers(&lexer.next().unwrap(), lexer, expressions).unwrap()),
        })),
        Token::Minus => Some(Expression::BinaryOperator(BinaryOperator {
            left: Box::new(expressions.pop().unwrap()),
            operator: Operator::Minus,
            right: Box::new(chain_pasers(&lexer.next().unwrap(), lexer, expressions).unwrap()),
        })),
        Token::Asterisk => Some(Expression::BinaryOperator(BinaryOperator {
            left: Box::new(expressions.pop().unwrap()),
            operator: Operator::Multiply,
            right: Box::new(chain_pasers(&lexer.next().unwrap(), lexer, expressions).unwrap()),
        })),
        Token::Slash => Some(Expression::BinaryOperator(BinaryOperator {
            left: Box::new(expressions.pop().unwrap()),
            operator: Operator::Divide,
            right: Box::new(chain_pasers(&lexer.next().unwrap(), lexer, expressions).unwrap()),
        })),
        Token::Equal => Some(Expression::BinaryOperator(BinaryOperator {
            left: Box::new(expressions.pop().unwrap()),
            operator: Operator::Equal,
            right: Box::new(chain_pasers(&lexer.next().unwrap(), lexer, expressions).unwrap()),
        })),
        Token::NotEqual => Some(Expression::BinaryOperator(BinaryOperator {
            left: Box::new(expressions.pop().unwrap()),
            operator: Operator::NotEqual,
            right: Box::new(chain_pasers(&lexer.next().unwrap(), lexer, expressions).unwrap()),
        })),
        Token::LessThan => Some(Expression::BinaryOperator(BinaryOperator {
            left: Box::new(expressions.pop().unwrap()),
            operator: Operator::LessThan,
            right: Box::new(chain_pasers(&lexer.next().unwrap(), lexer, expressions).unwrap()),
        })),
        Token::GreaterThan => Some(Expression::BinaryOperator(BinaryOperator {
            left: Box::new(expressions.pop().unwrap()),
            operator: Operator::GreaterThan,
            right: Box::new(chain_pasers(&lexer.next().unwrap(), lexer, expressions).unwrap()),
        })),
        Token::LessThanEqual => Some(Expression::BinaryOperator(BinaryOperator {
            left: Box::new(expressions.pop().unwrap()),
            operator: Operator::LessThanEqual,
            right: Box::new(chain_pasers(&lexer.next().unwrap(), lexer, expressions).unwrap()),
        })),
        Token::GreaterThanEqual => Some(Expression::BinaryOperator(BinaryOperator {
            left: Box::new(expressions.pop().unwrap()),
            operator: Operator::GreaterThanEqual,
            right: Box::new(chain_pasers(&lexer.next().unwrap(), lexer, expressions).unwrap()),
        })),
        _ => None,
    }
}

fn parse_parens(token: &Token, lexer: &mut Lexer<Token>, expressions: &mut Vec<Expression>)  -> Option<Expression> {
    if let Token::LParen = token {
        let mut expressions = Vec::new();
        loop {
            let token = lexer.next();
                        if let Some(Token::RParen) = token {
                lexer.next();
                break;
            }

            match token {
                Some(token) => {
                    let expression = chain_pasers(&token, lexer, &mut expressions);
                    if let Some(expression) = expression {
                        expressions.push(expression);
                    }
                },
                None => panic!("Unexpected end of file"),
            }
        }
        
        // TODO: This is a hack to get the last expression in the parens
        // Need to rething this logic to reduce the expression to a single one.
        if expressions.is_empty() {
            Some(Expression::Call)
        } else {
            Some(expressions.last().unwrap().clone())
        }
    } else {
        None
    }
}

fn parse_let(token: &Token, lexer: &mut Lexer<Token>, expressions: &mut Vec<Expression>)  -> Option<Expression> {
    if let Token::Let = token {
        let ident = lexer.next();
        let assingment = lexer.next();
        let value = lexer.next();
        if let Some(Token::Ident(ident)) = ident {
            if let Some(Token::Assign) = assingment {
                if let Some(value) = value {
                    let expression = chain_pasers(&value, lexer, expressions);
                    if let Some(expression) = expression {
                        Some(Expression::Let(Let {name: ident, value: Box::new(expression)}))
                    } else {
                        panic!("Unexpected end of file");
                    }
                } else {
                    panic!("Unexpected end of file");
                }
            } else {
                panic!("Unexpected end of file");
            }
        } else {
            panic!("Unexpected end of file");
        }
    } else {
        None
    }
}

pub fn parser(input: &str) -> Program {
    let mut lexer = Token::lexer(input);
    let mut expressions = Vec::new();
    loop {
        let token = lexer.next();
        match token {
            Some(token) => {
                let expression = chain_pasers(&token, &mut lexer, &mut expressions);
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
