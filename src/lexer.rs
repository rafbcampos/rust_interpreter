use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
#[regex(r"[ \n\t\f]+", logos::skip)]
pub enum Token {
    // keywords
    #[token("fn")]
    Function,
    #[token("let")]
    Let,
    // operators
    #[token("=")]
    Assign,
    #[token("+")]
    Plus,
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
    fn handles_case1() {
        let input = r#"
        let five = 5;
        let ten = 10;
        let add = fn(x, y) {
            x + y;
        };
        let result = add(five, ten);
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
            Token::Eof,
        ];

        let tokens = lexer(input);
        assert_eq!(tokens, expected);
    }

