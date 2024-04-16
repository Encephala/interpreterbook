#![allow(clippy::needless_return)]

mod lexer;
mod parser;
mod repl;
mod evaluate;

fn main() {
    repl::start();
}
