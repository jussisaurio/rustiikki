extern crate combine;

use combine::*;
use combine::parser::char::*;
use combine::stream::*;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Token {
    Number(f64),
    Plus,
    Minus,
    Asterisk,
    Slash,
    Bang,
    Equal,
    BangEqual,
    GEqual,
    LEqual,
    Less,
    Greater,
    ParenL,
    ParenR,
    Boolean(bool),
    Null,
    EOFt
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct TokenWithLine {
    pub token: Token,
    pub line_information: (usize, usize)
}

#[derive(Debug, PartialEq)]
pub struct ErrorToken<'a> {
    errors: Vec<easy::Error<char, &'a str>>,
    line_information: (usize, usize)
}

trait Bifunctor<A, B, C, D> {
    type Mapped;
    fn bimap(self, left: impl FnOnce(B) -> D, right: impl FnOnce(A) -> C) -> Self::Mapped;
}

impl<A,B,C,D> Bifunctor<A,B,C,D> for Result<A,B> {
    type Mapped = Result<C,D>;
    fn bimap(self, left: impl FnOnce(B) -> D, right: impl FnOnce(A) -> C) -> Result<C,D> {
        match self {
            Ok(x) => Ok(right(x)),
            Err(x) => Err(left(x))
        }
    }
}

fn parse_number<Input>() -> impl Parser<Input, Output = Token>
where
    Input: Stream<Token = char>,
    Input::Error: error::ParseError<Input::Token, Input::Range, Input::Position>
{
    let leading_minus = optional(token('-'));
    let decimal_points = optional((token('.'), many1(digit())).map(|(a,b): (char, String)| format!("{}{}", a, b)));
    let integer = many1(digit());
    let number_parts = (leading_minus, integer, decimal_points).map(|(a,b,c): (Option<char>, String, Option<String>)| {
        format!("{}{}{}", a.map(|c| c.to_string()).unwrap_or(String::from("")), b, c.unwrap_or(String::new()))
    });

    attempt(number_parts.map(|num| Token::Number(num.parse::<f64>().unwrap())))
}

fn parse_operator<Input>() -> impl Parser<Input, Output = Token>
where
    Input: Stream<Token = char>,
    Input::Error: error::ParseError<Input::Token, Input::Range, Input::Position>
{

    let to_token = |x: char| {
        println!("{}", x);
        match x {
            '<' => Token::Less,
            '>' => Token::Greater,
            '-' => Token::Minus,
            '+' => Token::Plus,
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '(' => Token::ParenL,
            ')' => Token::ParenR,
            '!' => Token::Bang,
            _ => panic!("unexpected")
        }
    };

    let to_token_multichar = |x: &'static str | {
        match x {
            "==" => Token::Equal,
            ">=" => Token::GEqual,
            "<=" => Token::LEqual,
            "!=" => Token::BangEqual,
            _ => panic!("unexpected")
        }
    };

    let sing = move |x: char| {
        attempt(token(x).map(to_token))
    };

    let mul = move |x: &'static str| {
        attempt(string(x).map(to_token_multichar))
    };

    let equal = mul("==");
    let gequal = mul(">=");
    let lequal = mul("<=");
    let bangequal = mul("!=");

    let minus = sing('-');
    let plus = sing('+');
    let asterisk = sing('*');
    let slash = sing('/');
    let parenl = sing('(');
    let parenr = sing(')');
    let bang = sing('!');
    let greater = sing('>');
    let less = sing('<');

    choice!(equal, gequal, lequal, bangequal, minus, plus, asterisk, slash, parenl, parenr, bang, greater, less)
}

fn parse_keyword<Input>() -> impl Parser<Input, Output = Token>
where
    Input: Stream<Token = char>,
    Input::Error: error::ParseError<Input::Token, Input::Range, Input::Position>
{

    let truep = attempt(string("true").map(|_| Token::Boolean(true)));
    let falsep = attempt(string("false").map(|_| Token::Boolean(false)));
    let nullp = attempt(string("null").map(|_| Token::Null));

    choice!(truep, falsep, nullp)
}

#[derive(Debug)]
pub struct Tokenizer<'a> {
    pub stream: position::Stream<&'a str, position::SourcePosition>
}

impl<'a> Tokenizer<'a> {
    pub fn new(source: &'a str) -> Tokenizer<'a> {
        Tokenizer { stream: position::Stream::new(source) }
    }

    pub fn parse(&mut self)
    -> Result<TokenWithLine, ErrorToken> {
    let spaces = many::<String, _, _>(space());
    let eof_ = eof().map(|_| Token::EOFt);
    
    let mut combined_parser = spaces.with(choice!(parse_keyword(), parse_operator(), parse_number(), eof_));
    
    let strm = self.stream.clone();
    let token = combined_parser.easy_parse(strm).bimap(
        |err| {
            let position = err.position();
            ErrorToken {
                errors: err.errors,
                line_information: (position.line as usize, position.column as usize)
            }
        },
        |(token, rest)| {
            let position = rest.position();
            self.stream = rest;
            TokenWithLine {
                token,
                line_information: (position.line as usize, position.column as usize)
            }
        }
    )?;

    Ok(token)
}
}

