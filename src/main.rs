#[macro_use]
extern crate num_derive;

mod vm;
mod chunk;
pub mod tokenizer;

use vm::*;
use std::io;
use std::io::prelude::*;

fn main() {
    let stdin = io::stdin();
    println!("Rustiikki REPL");
    for line in stdin.lock().lines() {
        let code = line.unwrap();
        let mut machine = VM::new(tokenizer::Tokenizer::new(&code));
        machine.set_debug(true);
        let compilation_result = machine.compile();
        match compilation_result {
            Ok(()) => {
                let result = machine.run();
                match result {
                    Ok(r) => println!("{:?}", r),
                    Err(e) => println!("{:?}", e)
                }
            }
            Err(e) => {
                println!("{:?}", e)
            }
        }
    }
}
