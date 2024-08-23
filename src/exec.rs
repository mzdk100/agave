use std::{
    cell::RefCell,
    collections::HashMap,
    rc::{Rc, Weak},
};

use crate::{
    ast::{Opcode, Program, Token, Vals},
    builtin::{get_builtin_callable, Callable},
    parse::{AstParser, ParseError},
};

pub(crate) struct ExecContext {
    global_variables: RefCell<HashMap<Token, Vals>>,
    root: Vec<Program>,
}

impl ExecContext {
    /// 创建一个虚拟机环境。
    ///
    /// # Arguments
    ///
    /// * `buffer`: 要运行的脚本内容。
    ///
    /// returns: Result<ExecContext, SyntaxError> 如果脚本内容没有语法错误则成功。
    ///
    /// # Examples
    ///
    /// ```
    /// ExecContext::new("函数 测试() {}")
    /// ```
    pub(crate) fn new<'a>(buffer: String) -> Result<Self, ParseError> {
        let global_variables = RefCell::new(HashMap::new());
        let mut parser = AstParser::new();
        let root = match parser.to_ast(&buffer) {
            Err(e) => return Err(e),
            Ok(b) => b,
        };

        Ok(Self {
            global_variables,
            root,
        })
    }

    pub(crate) fn exec(&self) {
        if self.root.is_empty() {
            return;
        }
        for i in &self.root {
            match i {
                Program::Entry(sentences) => {
                    self.call_function(
                        &Token::new_ident("entry"),
                        &vec![],
                        Callable::Customize(&vec![], &sentences),
                    );
                }
                //全局变量
                Program::Global(Token::Assignment(name, token)) => {
                    let var = self.run_sentence(token.as_ref(), unsafe {
                        &mut *self.global_variables.as_ptr()
                    });
                    self.global_variables
                        .borrow_mut()
                        .insert(Token::new_ident(&name), var);
                }
                _ => {}
            }
        }
    }

    fn calc_expr(
        &self,
        left: &Token,
        right: &Token,
        opcode: &Opcode,
        local_params: &mut HashMap<Token, Vals>,
    ) -> Vals {
        let left_num = self.run_sentence(left, local_params);
        let right_num = self.run_sentence(right, local_params);
        match opcode {
            Opcode::Add => left_num + right_num,
            Opcode::Subtract => left_num - right_num,
            Opcode::Multiply => left_num * right_num,
            Opcode::Divide => left_num / right_num,
        }
        .into()
    }

    pub(crate) fn run_sentence(&self, sentence: &Token, local_variables: &mut HashMap<Token, Vals>) -> Vals {
        match sentence {
            Token::Express(opcode, left, right) => {
                self.calc_expr(left.as_ref(), right.as_ref(), opcode, local_variables)
            }
            Token::Value(v) => v.clone(),
            Token::Ident(_) => {
                return if self.global_variables.borrow().contains_key(sentence) {
                    //全局
                    self.global_variables
                        .borrow()
                        .get(sentence)
                        .unwrap()
                        .clone()
                } else if local_variables.get(sentence).is_some() {
                    //局部
                    local_variables.get(sentence).unwrap().clone()
                } else {
                    Vals::Invalid
                };
            }
            Token::Assignment(name, token) => {
                let name = Token::new_ident(name);
                let var = self.run_sentence(token.as_ref(), local_variables);
                if self.global_variables.borrow().contains_key(&name) {
                    self.global_variables.borrow_mut().insert(name, var);
                } else {
                    local_variables.insert(name, var);
                }
                match token.as_ref() {
                    Token::Value(v) => v.clone(),
                    _ => Vals::Invalid,
                }
            }
            Token::CallFunction(name, actual_args) => {
                let call_func = self.find_function(Rc::downgrade(name));
                let actual_args = actual_args.iter().map(|i| Token::new_value(self.run_sentence(i, local_variables))).collect();
                self.call_function(name.as_ref(), &actual_args, call_func)
            }
            Token::Comma => Vals::Invalid,
        }
    }

    fn call_customize_function(
        &self,
        actual_args: &Vec<Token>,
        formal_args: &Vec<Token>,
        sentences: &Vec<Token>,
        mut local_variables: HashMap<Token, Vals>,
    ) -> Vals {
        for (i, v) in actual_args.iter().enumerate() {
            let name = &formal_args[i];
            let var = self.run_sentence(v, &mut local_variables);
            local_variables.insert(name.clone(), var);
        }
        let mut res = Vals::Invalid;
        for i in sentences {
            res = self.run_sentence(i, &mut local_variables);
        }
        res
    }

    fn call_function(&self, name: &Token, actual_args: &Vec<Token>, call_func: Callable) -> Vals {
        let local_variables: HashMap<Token, Vals> = HashMap::new();
        match call_func {
            Callable::None => {
                let call_func = Token::new_call_function(name.clone(), actual_args.clone());
                println!("不能找到函数`{}`，因此无法调用`{}`！", name, call_func);
                Vals::Invalid
            }
            Callable::Builtin(func) => func(self, actual_args, local_variables),
            Callable::Customize(formal_args, sentences) => {
                self.call_customize_function(actual_args, formal_args, sentences, local_variables)
            }
        }
    }

    fn find_function(&self, name: Weak<Token>) -> Callable {
        let Some(name) = name.upgrade() else {
            return Callable::None;
        };
        if let Token::Ident(name) = name.as_ref() {
            if let Callable::Builtin(func) = get_builtin_callable(name) {
                return Callable::Builtin(func);
            }
        }
        for i in &self.root {
            match i {
                Program::Function {
                    name: name2,
                    args,
                    sentences,
                } if name2 == name.as_ref() => return Callable::Customize(args, sentences),
                _ => (),
            }
        }
        Callable::None
    }
}
