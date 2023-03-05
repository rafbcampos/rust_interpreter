mod lexer;
mod parser;

use parser::parser;
use lexer::Token;
use logos::Logos;
use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};

fn repl() -> Result<()> {
    let mut rl = DefaultEditor::new()?; 

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                let ast = parser(&line);
                let tokens = Token::lexer(&line);
                println!("Tokens:");
                for token in tokens {
                    println!("{:?}", token);
                }
                println!("----------------");
                println!("AST: {:?}", ast);
            },
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            },
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    Ok(())
}

fn main() {
    repl().unwrap();
}
