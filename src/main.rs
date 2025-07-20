use ariadne::{Label, Report, ReportKind, Source};
use std::fs;
use clap::Parser;

mod lexer;
mod parser;
mod interpreter;
mod stdlib;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
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

    let mut parser = parser::Parser::new(&tokens);
    let (ast, errors) = parser.parse();

    if !errors.is_empty() {
        for (msg, span) in errors {
            Report::build(ReportKind::Error, (filename.clone(), span.clone()))
                .with_message("Parsing Error")
                .with_label(Label::new((filename.clone(), span)).with_message(msg))
                .finish()
                .print((filename.clone(), Source::from(&source_code)))
                .unwrap();
        }
        return;
    }
    
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
