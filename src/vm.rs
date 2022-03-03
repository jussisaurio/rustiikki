use crate::chunk::*;
extern crate combine;
extern crate num;
use combine::stream::position::*;
use crate::tokenizer::*;


#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, FromPrimitive, ToPrimitive)]
pub enum Precedence {
    None = 0,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary
}

impl Precedence {
    fn next(&self) -> Precedence {
        let numeric_prec: usize = num::ToPrimitive::to_usize(self).unwrap();
        num::FromPrimitive::from_usize(numeric_prec).unwrap_or(Precedence::Primary)
    }
}

pub struct VM<'a> {
    instruction_pointer: usize,
    parser: MyParser,
    chunk: Chunk,
    stack: Vec<Value>,
    debug: bool,
    tokenizer: Tokenizer<'a>,
}

#[derive(Debug, Clone)]
pub enum InterpretError {
    CompileError(String),
    RuntimeError(String)
}

pub enum ErrorSource {
    E(ErrorToken),
    T((&'static str, TokenWithLine))
}

pub type InterpretResult = Result<Value, InterpretError>;

#[derive(Debug, PartialEq)]
enum ExprParserType {
    Grouping,
    Unary,
    Binary,
    Literal,
    None
}

type ParseRule = (ExprParserType, ExprParserType, Precedence);


impl TokenWithLine {
    fn get_parse_rule(&self) -> ParseRule {
        match &self.token {
            &Token::Minus => (ExprParserType::Unary, ExprParserType::Binary, Precedence::Term),
            &Token::Bang => (ExprParserType::Unary, ExprParserType::None, Precedence::Unary),
            &Token::Equal => (ExprParserType::None, ExprParserType::Binary, Precedence::Equality),
            &Token::BangEqual => (ExprParserType::None, ExprParserType::Binary, Precedence::Equality),
            &Token::GEqual => (ExprParserType::None, ExprParserType::Binary, Precedence::Comparison),
            &Token::LEqual => (ExprParserType::None, ExprParserType::Binary, Precedence::Comparison),
            &Token::Less => (ExprParserType::None, ExprParserType::Binary, Precedence::Comparison),
            &Token::Greater => (ExprParserType::None, ExprParserType::Binary, Precedence::Comparison),
            &Token::Plus => (ExprParserType::None, ExprParserType::Binary, Precedence::Term),
            &Token::Asterisk => (ExprParserType::None, ExprParserType::Binary, Precedence::Factor),
            &Token::Slash => (ExprParserType::None, ExprParserType::Binary, Precedence::Factor),
            &Token::Number(_) => (ExprParserType::Literal, ExprParserType::None, Precedence::None),
            &Token::Null => (ExprParserType::Literal, ExprParserType::None, Precedence::None),
            &Token::Boolean(_) => (ExprParserType::Literal, ExprParserType::None, Precedence::None),
            &Token::ParenL => (ExprParserType::Grouping, ExprParserType::None, Precedence::None),
            &Token::ParenR => (ExprParserType::None, ExprParserType::None, Precedence::None),
            &Token::EOFt => (ExprParserType::None, ExprParserType::None, Precedence::None),
        }
    }

    fn get_prefix_parse_rule(&self) -> ExprParserType {
        self.get_parse_rule().0
    }
    fn get_infix_parse_rule(&self) -> ExprParserType {
        self.get_parse_rule().1
    }
    fn get_precedence(&self) -> Precedence {
        self.get_parse_rule().2
    }    
}

#[derive(Debug)]
struct MyParser {
    cur: Option<TokenWithLine>,
    prev: Option<TokenWithLine>,
}

impl MyParser {
    fn new() -> MyParser {
        MyParser { cur: None, prev: None }
    }
}

impl<'a> VM<'a> {
    pub fn new(tokenizer: Tokenizer<'a>) -> VM<'a> {
        // TODO fix emitted compiler error messages to be less ugly
        VM { instruction_pointer: 0, chunk: Chunk::new(), stack: Vec::new(), parser: MyParser::new(), tokenizer, debug: false }
    }

    pub fn set_debug(&mut self, d: bool) -> &mut VM<'a> {
        self.debug = d;
        self
    }

    fn advance(&mut self) -> Result<(), InterpretError> {
        println!("{:?}", self.chunk.code);
        match self.tokenizer.parse() {
            Ok(tok) => {
                if let Some(t) = self.parser.cur {
                    self.parser.prev = Some(t);
                    self.parser.cur = Some(tok);
                } else {
                    self.parser.cur = Some(tok);
                }
                println!("Advance finished, prev: {:?} - curr: {:?}", self.parser.prev, self.parser.cur);
                return Ok(())
            }
            Err(e) => return Err(self.create_compile_error(ErrorSource::E(e)))
        }                
    }

    fn execute_rule(&mut self, rule: ExprParserType)-> Result<(), InterpretError> {
        let token = self.parser.prev.unwrap();
        match rule {
            ExprParserType::None => Err(self.create_compile_error(ErrorSource::T(("Expected valid parsing rule", token)))),
            ExprParserType::Binary => self.binary(),
            ExprParserType::Unary => self.unary(),
            ExprParserType::Literal => self.literal(),
            ExprParserType::Grouping => self.grouping()
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), InterpretError> {
        println!("Prec is {:?}", precedence);
        self.advance()?;
        if let Some(t) = self.parser.prev {
            let pre = t.get_prefix_parse_rule();
            if pre == ExprParserType::None {
                return Err(self.create_compile_error(ErrorSource::T(("Expected prefix parsing rule", t))));
            }
            println!("Executing prefix rule {:?} for {:?}", pre, t);
            self.execute_rule(pre)?;
            loop {
                if let (Some(c), Some(p)) = (self.parser.cur, self.parser.prev) {
                    if precedence > c.get_precedence() {
                        break;
                    }
                    self.advance()?;
                    let inf = c.get_infix_parse_rule();
                    println!("Executing infix rule {:?} for {:?}", inf, p);
                    self.execute_rule(inf)?;
                } else {
                    break;
                }
            }
            Ok(())
        } else {
            Err(self.create_internal_error("Expected to find previous token"))
        }
    }

    fn expression(&mut self) -> Result<(), InterpretError> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn unary(&mut self) -> Result<(), InterpretError>  {
        if let Some(t) = self.parser.prev {
            self.parse_precedence(Precedence::Unary)?;
            match t.token {
                Token::Minus => {
                    self.chunk.write(Opcode::Negate, t.line_information);
                    Ok(())
                },
                Token::Bang => {
                    self.chunk.write(Opcode::Not, t.line_information);
                    Ok(())
                }
                t => Err(self.create_internal_error("Expected to find - or !"))
            }
        } else {
            Err(self.create_internal_error("Expected to find a token in parser.prev"))
        }
    }

    fn literal(&mut self) -> Result<(), InterpretError> {
        if let Some(t) = self.parser.prev {
            match t.token {
                Token::Number(n) => {
                    self.chunk.emit_constant(Value::Number(n), t.line_information);
                    Ok(())
                },
                Token::Boolean(b) => {
                    self.chunk.emit_constant(Value::Boolean(b), t.line_information);
                    Ok(())
                },
                Token::Null => {
                    self.chunk.emit_constant(Value::Null, t.line_information);
                    Ok(())
                },
                _ => Err(self.create_internal_error("Expected to find number, boolean or null"))
            }
        } else {
            Err(self.create_internal_error("Expected to find a token in parser.prev"))
        }
    }

    fn binary(&mut self) -> Result<(), InterpretError> {
        let t = self.parser.prev.ok_or(self.create_internal_error("Expected to find a token in parser.prev"))?;
        let prec = t.get_precedence();
        self.parse_precedence(prec.next())?;
        match t.token {
            Token::Plus => {
                self.chunk.write(Opcode::Add, t.line_information);
                Ok(())
            },
            Token::Minus => {
                self.chunk.write(Opcode::Subtract, t.line_information);
                Ok(())
            },
            Token::Asterisk => {
                self.chunk.write(Opcode::Multiply, t.line_information);
                Ok(())
            },
            Token::Slash => {
                self.chunk.write(Opcode::Divide, t.line_information);
                Ok(())
            },
            Token::Equal => {
                self.chunk.write(Opcode::Equal, t.line_information);
                Ok(())
            },
            Token::BangEqual => {
                self.chunk.write(Opcode::Equal, t.line_information);
                self.chunk.write(Opcode::Not, t.line_information);
                Ok(())
            },
            Token::Greater => {
                self.chunk.write(Opcode::Greater, t.line_information);
                Ok(())
            },
            Token::Less => {
                self.chunk.write(Opcode::Less, t.line_information);
                Ok(())
            },
            Token::GEqual => {
                self.chunk.write(Opcode::Less, t.line_information);
                self.chunk.write(Opcode::Not, t.line_information);
                Ok(())
            },
            Token::LEqual => {
                self.chunk.write(Opcode::Greater, t.line_information);
                self.chunk.write(Opcode::Not, t.line_information);
                Ok(())
            },
            _ => Err(self.create_internal_error("Expected to find a binary operator in parser.prev"))
        }
    }

    fn consume(&mut self, t: Token) -> Result<(), InterpretError> {
        if matches!(self.parser.cur, Some(TokenWithLine { token, line_information }) if token == t) {
            self.advance()
        } else {
            match self.parser.cur {
                None => Err(self.create_internal_error("Expected to find a token in parser.cur")),
                Some(t) => Err(self.create_compile_error(ErrorSource::T(("Expected to find closing parenthesis, found", t))))
            }
            
        }
    }

    fn grouping(&mut self) -> Result<(), InterpretError> {
        self.expression()?;
        self.consume(Token::ParenR)
    }

    pub fn compile(&mut self) -> Result<(), InterpretError> {
        self.instruction_pointer = 0;
        self.advance()?;
        self.expression()?;
        self.consume(Token::EOFt)?;
        self.chunk.write(Opcode::Return, self.parser.cur.unwrap().line_information);
        Ok(())
    }

    fn read_instruction(&mut self) {
        self.instruction_pointer += 1
    }

    fn pop(&mut self) -> InterpretResult {
        let opt = self.stack.pop();
        if let Some(val) = opt {
            return Result::Ok(val);
        } else {
            return Result::Err(self.create_runtime_error(String::from("empty stack?")));
        }
    }

    fn push(&mut self, v: Value) {
        self.stack.push(v);
    }

    fn create_runtime_error(&self, message: String) -> InterpretError {
        let (line, column) = self.chunk.get_line_and_column(self.instruction_pointer);
        InterpretError::RuntimeError(format!("Line {}, Col {}: {}", line, column, message))
    }

    fn create_compile_error(&self, source: ErrorSource) -> InterpretError {
        let (line, column) = match &source {
            ErrorSource::E(errorToken) => errorToken.line_information,
            ErrorSource::T((_, token)) => token.line_information
        };
        let msg = match &source {
            ErrorSource::E(errorToken) => format!("{:?}", errorToken.errors),
            ErrorSource::T((message, token)) => {
                format!("{}: {:?}", message, token)
            }
        };
        InterpretError::CompileError(format!("Line {}, Col {}: {:?}", line, column, msg))
    }

    fn create_internal_error(&self, message: &'static str) -> InterpretError {
        InterpretError::CompileError(format!("Internal compiler error: {}", message))
    }

    pub fn run(&mut self) -> InterpretResult {
        loop {
            let inst = self.chunk.code[self.instruction_pointer as usize];
            if self.debug {
                self.chunk.disassembleInstruction(self.instruction_pointer);
            }
            match inst {
                Opcode::Return => {
                    let val = self.pop()?;
                    println!("{:?}", val);
                    return Result::Ok(val);
                }
                Opcode::Constant(idx) => {
                    let constant = self.chunk.constants[idx as usize];
                    self.push(constant);
                }
                Opcode::Negate => {
                    let val = self.pop()?;
                    match val {
                        Value::Number(x) => self.push(Value::Number(-x)),
                        // TODO: get line-information for runtime errors
                        other => return Err(self.create_runtime_error(format!("expected Number when negating, got {:?}", other)))
                    }        
                }
                Opcode::Add => {
                    let val = self.pop()?;
                    let val2 = self.pop()?;
                    // val is top of stack, i.e. the second operand
                    match (val2, val) {
                        (Value::Number(x), Value::Number(y)) => self.push(Value::Number(x + y)),
                        other => return Err(self.create_runtime_error(format!("Trying to Add using a non-number, got {:?}", other)))
                    }
                },
                Opcode::Subtract => {
                    let val = self.pop()?;
                    let val2 = self.pop()?;
                    match (val2, val) {
                        (Value::Number(x), Value::Number(y)) => self.push(Value::Number(x - y)),
                        other => return Err(self.create_runtime_error(format!("Trying to Subtract using a non-number, got {:?}", other)))
                    }
                },
                Opcode::Multiply => {
                    let val = self.pop()?;
                    let val2 = self.pop()?;
                    match (val2, val) {
                        (Value::Number(x), Value::Number(y)) => self.push(Value::Number(x * y)),
                        other => return Err(self.create_runtime_error(format!("Trying to Multiply using a non-number, got {:?}", other)))
                    }
                },
                Opcode::Divide => {
                    let val = self.pop()?;
                    let val2 = self.pop()?;
                    match (val2, val) {
                        (Value::Number(x), Value::Number(y)) => self.push(Value::Number(x / y)),
                        other => return Err(self.create_runtime_error(format!("Trying to Divide using a non-number, got {:?}", other)))
                    }
                }
                Opcode::Not => {
                    let val = self.pop()?;
                    match val {
                        Value::Number(x) => self.push(Value::Boolean(if x == 0.0 { true } else { false })),
                        Value::Boolean(b) => self.push(Value::Boolean(!b)),
                        Value::Null => self.push(Value::Boolean(true))
                    }
                },
                Opcode::Equal => {
                    let val = self.pop()?;
                    let val2 = self.pop()?;
                    match (val2, val) {
                        (Value::Number(x), Value::Number(y)) => self.push(Value::Boolean(x == y)),
                        (Value::Boolean(x), Value::Boolean(y)) => self.push(Value::Boolean(x == y)),
                        (Value::Null, Value::Null) => self.push(Value::Boolean(true)),
                        other => return Err(self.create_runtime_error(format!("Trying to compare equality of different types, got {:?}", other)))
                    }                    
                },
                Opcode::Greater => {
                    let val = self.pop()?;
                    let val2 = self.pop()?;
                    println!("{:?} {:?}", val, val2);
                    match (val2, val) {
                        (Value::Number(x), Value::Number(y)) => self.push(Value::Boolean(x > y)),
                        other => return Err(self.create_runtime_error(format!("Trying to compare magnitude of non-numbers, got {:?}", other)))
                    }                    
                },
                Opcode::Less => {
                    let val = self.pop()?;
                    let val2 = self.pop()?;
                    match (val2, val) {
                        (Value::Number(x), Value::Number(y)) => self.push(Value::Boolean(x < y)),
                        other => return Err(self.create_runtime_error(format!("Trying to compare magnitude of non-numbers, got {:?}", other)))
                    }                    
                }
            }
            if self.debug {
                for val in &self.stack {
                    println!("[{:?}]", val)
                }
            }
            self.read_instruction();
        }

    }
}