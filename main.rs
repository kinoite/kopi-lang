use ariadne::{Label, Report, ReportKind, Source};
use std::fs;
use clap::Parser; // Import clap's Parser

mod lexer;
mod parser;
mod interpreter;

/// A fast, modern interpreter for the Kopi language built in Rust
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// The Kopi script file to execute
    filename: String,
}

fn main() {
    let args = Args::parse();
    let filename = &args.filename;

    let source_code = match fs::read_to_string(filename) {
        Ok(code) => code,
        Err(e) => {
            eprintln!("Error reading file {}: {}", filename, e);
            return;
        }
    };

    // --- Lexer Pass ---
    let tokens = match lexer::tokenize(&source_code) {
        Ok(tokens) => tokens,
        Err((msg, span)) => {
            Report::build(ReportKind::Error, (filename.clone(), span.clone()))
                .with_message("Lexical Error")
                .with_label(Label::new((filename.clone(), span)).with_message(msg))
                .finish()
                .print((filename.clone(), Source::from(&source_code)))
                .unwrap();
            return;
        }
    };

    // --- Parser Pass ---
    let mut parser = parser::Parser::new(&tokens);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err((msg, span)) => {
            Report::build(ReportKind::Error, (filename.clone(), span.clone()))
                .with_message("Parsing Error")
                .with_label(Label::new((filename.clone(), span)).with_message(msg))
                .finish()
                .print((filename.clone(), Source::from(&source_code)))
                .unwrap();
            return;
        }
    };
    
    // --- Interpreter Pass ---
    let mut interpreter = interpreter::Interpreter::new();
    if let Err((msg, span)) = interpreter.run(ast) {
        Report::build(ReportKind::Error, (filename.clone(), span.clone()))
            .with_message("Runtime Error")
            .with_label(Label::new((filename.clone(), span)).with_message(msg))
            .finish()
            .print((filename.clone(), Source::from(&source_code)))
            .unwrap();
    }
}
