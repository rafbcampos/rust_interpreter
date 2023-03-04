pub mod lexer;

use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};
use logos::Logos;
use lexer::Token;


fn repl() -> Result<()> {
    let mut rl = DefaultEditor::new()?; 

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                let tokens = Token::lexer(&line);
                for token in tokens {
                    println!("{:?}", token);
                }
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
