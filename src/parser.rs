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
    left: Vec<Expression>,
    operator: Operator,
    right: Vec<Expression>,
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

type Parser = fn(&Token, &mut Lexer<Token>) -> Option<Expression>;

fn chain_pasers(token: &Token, lexer: &mut Lexer<Token>, parsers: Vec<Parser>) -> Option<Expression> {
   let mut expression: Option<Expression> = None;
   for parser in parsers {
       expression = parser(token, lexer);
       if expression.is_some() {
           break;
       }
   } 
   expression
}

fn parse_number(token: &Token, _lexer: &mut Lexer<Token>)  -> Option<Expression> {
    if let Token::Number(value) = token {
        Some(Expression::Literal(Literal::Number(*value)))
    } else {
        None
    }
}

fn parse_string(token: &Token, _lexer: &mut Lexer<Token>)  -> Option<Expression> {
    if let Token::Ident(value) = token {
        Some(Expression::Literal(Literal::String(value.to_string())))
    } else {
        None
    }
}

fn parse_boolean(token: &Token, _lexer: &mut Lexer<Token>)  -> Option<Expression> {
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

// fn parse_array(token: &Token, lexer: Lexer<Token> )  -> Option<Expression> {}
    
pub fn parser(input: &str) -> Program {
    let mut lexer = Token::lexer(input);
    let mut expressions = Vec::new();
    loop {
        let token = lexer.next();
        match token {
            Some(token) => {
                let expression = chain_pasers(&token, &mut lexer, vec![parse_number, parse_string, parse_boolean]);
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
