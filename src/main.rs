#![allow(clippy::needless_return)]

mod lexer;
mod parser;
mod repl;

fn main() {
    repl::start();
}
