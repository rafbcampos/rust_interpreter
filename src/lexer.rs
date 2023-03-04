use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
#[regex(r"[ \n\t\f]+", logos::skip)]
pub enum Token {
    // keywords
    #[token("fn")]
    Function,
    #[token("let")]
    Let,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("return")]
    Return,
    // operators
    #[token("==")]
    Equal,
    #[token("!=")]
    NotEqual,
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token("<=")]
    LessThanEqual,
    #[token(">=")]
    GreaterThanEqual,
    #[token("=")]
    Assign,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("!")]
    Bang,
    #[token("*")]
    Asterisk,
    #[token("/")]
    Slash,
    // delimiters
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    // identifiers
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_-]*", |lex| lex.slice().to_string())]
    Ident(String),
    // numbers
    #[regex(r"[0-9]+", |lex| lex.slice().parse())]
    Number(u64),
    // illegal
    #[regex(r"[ \n\t\f]+", logos::skip)]
    #[error]
    Ilega,
    // end of file
    Eof,
}

pub fn lexer(input: &str) -> Vec<Token> {
    let lex = Token::lexer(input);
    let mut tokens = Vec::new();
    for token in lex {
        tokens.push(token);
    }
    tokens.push(Token::Eof);
    tokens
}    

    #[test]
    fn tokenize() {
        let input = r#"
        let five = 5;
        let ten = 10;

        let add = fn(x, y) {
            x + y;
        };

        let result = add(five, ten);

        let is_greater = fn(x, y) {
            if (x > y) {
                return true;
            } else {
                return false;
            }
        };

        10 == 10;
        10 != 9;
        10 >= 10;
        9 <= 9;
        2 * 2;
        2 / 2;
        "#;

        let expected = vec![
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Number(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Number(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::LParen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::RParen,
            Token::Semicolon,
            Token::Let, 
            Token::Ident("is_greater".to_string()), 
            Token::Assign,
            Token::Function, 
            Token::LParen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::If,
            Token::LParen,
            Token::Ident("x".to_string()),
            Token::GreaterThan,
            Token::Ident("y".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::RBrace,
            Token::Semicolon,
            Token::Number(10),
            Token::Equal,
            Token::Number(10),
            Token::Semicolon,
            Token::Number(10),
            Token::NotEqual,
            Token::Number(9),
            Token::Semicolon,
            Token::Number(10),
            Token::GreaterThanEqual,
            Token::Number(10),
            Token::Semicolon,
            Token::Number(9),
            Token::LessThanEqual,
            Token::Number(9),
            Token::Semicolon,
            Token::Number(2),
            Token::Asterisk,
            Token::Number(2),
            Token::Semicolon,
            Token::Number(2),
            Token::Slash,
            Token::Number(2),
            Token::Semicolon,
            Token::Eof,
        ];

        let tokens = lexer(input);
        assert_eq!(tokens, expected);
    }

