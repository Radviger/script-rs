extern crate script;

use std::error::Error;
use std::any::TypeId;
use script::{Engine, TokenIterator, FnSignature};

fn main() {
    let mut engine = Engine::new();
    if let Err(e) = engine.eval::<f32>("fn kek(a, b: char, c) -> f64 { print(25, 41) }") {
        eprintln!("{}", e);
    }
    let matches = engine.functions.find_partial(&FnSignature {
        name: "kek".to_string(),
        args: vec![None, None, None],
        ret: Some(("f64".to_string(), TypeId::of::<f64>()))
    });
    println!("Matches: ");
    for m in matches {
        println!("{}", m);
    }
}