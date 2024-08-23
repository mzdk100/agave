use std::{env, fs::File, io::Read};

use crate::exec::ExecContext;

mod builtin;
mod ast;
mod exec;
mod parse;

fn main() {
    let inputs: Vec<String> = env::args().collect();
    if inputs.len() < 2 {
        print!("没有输入脚本文件，请提供需要运行的脚本文件名。");
        return;
    }
    let mut f = File::open(&inputs[1]).unwrap();
    let mut buffer = String::new();
    f.read_to_string(&mut buffer).unwrap();
    match ExecContext::new(buffer) {
        Ok(c) => c.exec(),
        Err(e) => println!("{}", e)
    }
}
