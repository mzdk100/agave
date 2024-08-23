use std::collections::HashMap;
use crate::{
    ast::{Token, Vals},
    exec::ExecContext,
};

#[derive(Debug)]
pub(crate) enum Callable<'a> {
    Builtin(fn(&ExecContext, &Vec<Token>, HashMap<Token, Vals>) -> Vals),
    Customize(&'a Vec<Token>, &'a Vec<Token>),
    None,
}

pub(crate) fn get_builtin_callable(name: &str) -> Callable {
    match name {
        "打印" => Callable::Builtin(print),
        "打印一行" => Callable::Builtin(println),
        _ => Callable::None,
    }
}

pub(crate) fn print(context: &ExecContext, args: &Vec<Token>, mut local_variables: HashMap<Token, Vals>) -> Vals {
    args.iter().for_each(move |i| {
        print!("{}", i);
    });
    Vals::Invalid
}

pub(crate) fn println(context: &ExecContext, args: &Vec<Token>, mut local_variables: HashMap<Token, Vals>) -> Vals {
    args.iter().for_each(move |i| {
        println!("{}", i);
    });
    Vals::Invalid
}
