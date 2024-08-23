use std::{
    error::Error,
    fmt::{Display, Formatter},
};

use nom::bytes::complete::take_until;
use nom::character::complete::char;
use nom::combinator::{cut, opt};
use nom::sequence::preceded;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{alpha1, alphanumeric1, digit1, multispace0, multispace1},
    combinator::recognize,
    error::{Error as NomError, ErrorKind},
    multi::{fold_many0, many0, separated_list0},
    sequence::{delimited, pair, terminated},
    Err, IResult,
};

use crate::ast::{Program, Token, Tree, Vals};

#[derive(Debug)]
pub(crate) struct ParseError(String);

impl<'a> ParseError {
    pub(crate) fn new(name: String) -> Self {
        Self(name)
    }
}

impl<'a> Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "无法解析代码：\n{}", self.0)
    }
}

impl<'a> Error for ParseError {}

pub(crate) struct AstParser {
    errors: Vec<ParseError>,
    lines: usize,
}

impl<'a> AstParser {
    pub(crate) fn new() -> Self {
        Self {
            errors: vec![],
            lines: 1,
        }
    }

    fn get_error_trace(&self) -> ParseError {
        let errors = self
            .errors
            .iter()
            .map(|i| i.0.clone())
            .collect::<Vec<_>>()
            .join("\n");
        ParseError::new(format!("{}\n语法错误！", errors))
    }

    //字符串转换成AST(抽象语法树)
    pub(crate) fn to_ast(&mut self, input: &'a str) -> Result<Tree, ParseError> {
        let Ok((remaining, root)) = many0(self.parse_program())(input) else {
            return Err(self.get_error_trace());
        };
        if !remaining.is_empty() {
            return Err(self.get_error_trace());
        }
        Ok(root)
    }

    fn take_blank_lines(&mut self) -> impl FnMut(&str) -> IResult<&str, ()> + '_ {
        |input| {
            let (remaining, out) = opt(preceded(opt(char('\r')), multispace1))(input)?;
            self.lines += out.unwrap_or("").chars().filter(|i| i == &'\n').count();
            Ok((remaining, ()))
        }
    }

    fn gen_error<T>(&mut self, reason: &str, remaining: &'a str) -> IResult<&'a str, T> {
        let (_, line) =
            take_until::<&str, &str, NomError<_>>("\n")(remaining).unwrap_or(("", remaining));
        self.errors.push(ParseError::new(format!(
            "【行{}】 {}： {}",
            self.lines, reason, line
        )));
        Err(Err::Failure(NomError::new(remaining, ErrorKind::Fail)))
    }

    fn parse_program(&mut self) -> impl FnMut(&'a str) -> IResult<&'a str, Program> + '_ {
        |input| {
            let (remaining, _) = self.take_blank_lines()(input)?;
            let res = alt((
                parse_function_program,
                parse_entry_program,
                parse_global_variable_program,
            ))(remaining);
            if res.is_err() {
                return self.gen_error("无法识别的程序", remaining);
            }
            res
        }
    }
}

fn parse_sentence_token(input: &str) -> IResult<&str, Token> {
    terminated(
        alt((parse_assignment_token, parse_call_function_token)),
        multispace0,
    )(input)
}

fn parse_block_token(input: &str) -> IResult<&str, Vec<Token>> {
    let (remaining, sentences) = delimited(
        tag("{"),
        delimited(multispace0, many0(parse_sentence_token), multispace0),
        tag("}"),
    )(input)?;
    Ok((remaining, sentences))
}

pub(crate) fn parse_call_function_token(input: &str) -> IResult<&str, Token> {
    let (remaining, ident) = delimited(multispace0, parse_ident_token, multispace0)(input)?;
    let (remaining, args) = delimited(
        multispace0,
        terminated(
            delimited(
                tag("("),
                separated_list0(
                    parse_comma_token,
                    alt((parse_const_expr_token, parse_ident_token)),
                ),
                tag(")"),
            ),
            tag(";"),
        ),
        multispace0,
    )(remaining)?;
    Ok((remaining, Token::new_call_function(ident, args)))
}

fn parse_comma_token(input: &str) -> IResult<&str, Token> {
    let (remaining, _) = delimited(multispace0, tag(","), multispace0)(input)?;
    Ok((remaining, Token::new_comma()))
}

fn parse_function_program(input: &str) -> IResult<&str, Program> {
    let (remaining, _) = tag("函数")(input)?;
    let (remaining, _) = multispace1(remaining)?;
    let (remaining, name) = parse_ident_token(remaining)?;
    let (remaining, args) = delimited(
        multispace0,
        delimited(
            tag("("),
            separated_list0(parse_comma_token, parse_ident_token),
            tag(")"),
        ),
        multispace0,
    )(remaining)?;
    let (remaining, ex) = parse_block_token(remaining)?;
    Ok((remaining, Program::new_function(name, args, ex)))
}

fn parse_entry_program(input: &str) -> IResult<&str, Program> {
    let (input, _) = tag("入口")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, ex) = parse_block_token(input)?;
    Ok((input, Program::new_entry(ex)))
}

fn parse_global_variable_program(input: &str) -> IResult<&str, Program> {
    let (remaining, _) = tag("全局")(input)?;
    let (remaining, _) = multispace1(remaining)?;
    let (remaining, token) = parse_assignment_token(remaining)?;
    if let Token::Assignment(name, token) = token {
        return Ok((
            remaining,
            Program::new_global(&name, token.as_ref().clone()),
        ));
    }
    Err(Err::Failure(NomError::new(
        "Can't parse a global variable.",
        ErrorKind::Fail,
    )))
}

fn parse_string_value_token(input: &str) -> IResult<&str, Token> {
    let (remaining, res) = delimited(tag("\""), take_while1(|f| f != '"'), tag("\""))(input)?;
    Ok((remaining, Token::new_value(Vals::String(res.to_string()))))
}

fn chinese(input: &str) -> IResult<&str, &str> {
    take_while1(|c| (c >= '一' && c <= '龟') || c == '_')(input)
}

fn parse_ident_token(input: &str) -> IResult<&str, Token> {
    let (remaining, ident) = recognize(pair(
        alt((alpha1, chinese)),
        many0(alt((alphanumeric1, chinese))),
    ))(input)?;
    Ok((remaining, Token::new_ident(ident)))
}

fn parse_assignment_token(input: &str) -> IResult<&str, Token> {
    let (remaining, Token::Ident(name)) = terminated(
        delimited(multispace0, parse_ident_token, multispace0),
        tag("="),
    )(input)?
    else {
        return Err(Err::Failure(NomError::new(
            "Can't parse the ident.",
            ErrorKind::Fail,
        )));
    };
    let (remaining, res) = parse_calc_token(remaining)?;
    let (remaining, _) = tag(";")(remaining)?;
    Ok((remaining, Token::new_assignment(&name, res)))
}

fn parse_const_expr_token(input: &str) -> IResult<&str, Token> {
    alt((parse_number_value_token, parse_string_value_token))(input)
}

fn parse_calc_group_expr_token(input: &str) -> IResult<&str, Token> {
    alt((
        parse_const_expr_token,
        delimited(tag("("), parse_calc_token, tag(")")),
    ))(input)
}

fn parse_calc_token(input: &str) -> IResult<&str, Token> {
    let (remaining, _) = multispace0(input)?;
    let (remaining, left_handle) = parse_calc_group_expr_token(remaining)?;
    let result = fold_many0(
        pair(
            delimited(
                multispace0,
                alt((tag("+"), tag("-"), tag("*"), tag("/"))),
                multispace0,
            ),
            parse_calc_group_expr_token,
        ),
        || left_handle.clone(),
        |acc, (operator, right_operand)| match operator {
            "+" => Token::new_add(acc, right_operand),
            "-" => Token::new_subtract(acc, right_operand),
            "*" => Token::new_multiply(acc, right_operand),
            "/" => Token::new_divide(acc, right_operand),
            _ => unreachable!(),
        },
    )(remaining);
    result
}

fn parse_number_value_token(input: &str) -> IResult<&str, Token> {
    let (remaining, integer) = digit1(input)?;
    let integer = integer.parse::<i32>().unwrap();
    let Ok((remaining, _)) = tag::<&str, _, NomError<_>>(".")(remaining) else {
        let val = Token::new_value(Vals::Integer(integer));
        return Ok((remaining, val));
    };
    let (remaining, decimal) = digit1(remaining)?;
    let decimal = decimal.parse::<i32>().unwrap();
    let val = Token::new_value(Vals::Float(integer, decimal));
    Ok((remaining, val))
}
