use std::iter::Peekable;
use std::str::Chars;
use std::error::Error;
use std::fmt::{Display, Formatter, Error as FmtError};
use std::any::TypeId;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::any::Any;

use typedef::{Type, Unit, Const};

pub struct Location(pub Rc<RefCell<usize>>, pub Rc<RefCell<usize>>);

impl Location {
    pub fn line(&self) -> usize {
        *self.0.borrow()
    }

    pub fn column(&self) -> usize {
        *self.1.borrow()
    }
}

#[derive(Clone)]
pub struct StmtLoc {
    pub location: (usize, usize),
    pub statement: Statement
}

impl StmtLoc {
    pub fn new(statement: Statement, location: &Location) -> StmtLoc {
        StmtLoc {
            location: (location.line(), location.column()),
            statement
        }
    }
}

#[derive(Clone)]
pub struct ExprLoc {
    pub location: (usize, usize),
    pub expression: Expression
}

impl ExprLoc {
    pub fn new(expression: Expression, location: &Location) -> ExprLoc {
        ExprLoc {
            location: (location.line(), location.column()),
            expression
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Facing {
    Left, Right
}

#[derive(Debug, Clone, PartialEq)]
pub enum Paren {
    Curly, Round, Square
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Add, Sub, Mul, Div, Mod, LeftShift, RightShift, ByteAnd, ByteOr, Xor, And, Or,
    Inverse, Negate, LessThan, LessOrEqual, GreaterThan, GreaterOrEqual, Equal, NotEqual, Cast
}

impl Operator {
    pub fn sign(&self) -> String {
        String::from(match self {
            Operator::Add => "+",
            Operator::Sub | Operator::Negate => "-",
            Operator::Mul => "*",
            Operator::Div => "/",
            Operator::Mod => "%",
            Operator::LeftShift => "<<",
            Operator::RightShift => ">>",
            Operator::ByteAnd => "&",
            Operator::ByteOr => "|",
            Operator::Xor => "^",
            Operator::And => "&&",
            Operator::Or => "||",
            Operator::Inverse => "!",
            Operator::LessThan => "<",
            Operator::LessOrEqual => "<=",
            Operator::GreaterThan => ">",
            Operator::GreaterOrEqual => ">=",
            Operator::Equal => "==",
            Operator::NotEqual => "!=",
            Operator::Cast => "as",
        })
    }

    pub fn priority(&self) -> i32 {
        match self {
            Operator::Or | Operator::Xor | Operator::ByteOr => 11,
            Operator::And | Operator::ByteAnd => 12,
            Operator::LessThan | Operator::LessOrEqual | Operator::GreaterThan |
            Operator::GreaterOrEqual | Operator::Equal | Operator::NotEqual => 15,
            Operator::Add | Operator::Sub => 20,
            Operator::Mul | Operator::Div => 40,
            Operator::LeftShift | Operator::RightShift => 50,
            Operator::Mod => 60,
            Operator::Cast => 110,
            _ => -1
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Reference {
    Immutable, Mutable
}

impl Reference {
    pub fn format(&self, ident: &str) -> String {
        format!("&{}{}", if *self == Reference::Mutable { "mut " } else { "" }, ident)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Paren(Paren, Facing),
    Const(Const),
    Assign(Option<Operator>),
    Operator(Operator),
    Reference(Reference, String),
    Identifier(String),
    Comma,
    Dot,
    Colon,
    Semicolon,
    DoubleColon,
    If,
    Else,
    Let,
    While,
    Loop,
    For,
    Break,
    Return,
    Fn,
    Arrow
}

impl Token {
    pub fn priority(&self) -> i32 {
        match self {
            Token::Assign(_) => 10,
            Token::Operator(o) => o.priority(),
            Token::Dot | Token::DoubleColon => 100,
            _ => -1
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Token::Const(c) => c.to_string(),
            Token::Assign(op) => {
                if let Some(op) = op {
                    format!("{}=", op.sign())
                } else {
                    String::from("=")
                }
            },
            Token::Paren(p, f) => {
                String::from(match p {
                    Paren::Square => if *f == Facing::Left { "[" } else { "]" },
                    Paren::Round => if *f == Facing::Left { "(" } else { ")" }
                    Paren::Curly => if *f == Facing::Left { "{" } else { "}" }
                })
            },
            Token::Operator(op) => op.sign(),
            Token::Reference(r, ident) => r.format(ident),
            Token::Identifier(i) => i.clone(),
            Token::Comma => String::from(","),
            Token::Dot => String::from("."),
            Token::Colon => String::from(":"),
            Token::DoubleColon => String::from("::"),
            Token::Semicolon => String::from(";"),
            Token::Arrow => String::from("->"),
            Token::If => String::from("if"),
            Token::Else => String::from("else"),
            Token::Let => String::from("let"),
            Token::While => String::from("while"),
            Token::Loop => String::from("loop"),
            Token::For => String::from("for"),
            Token::Break => String::from("break"),
            Token::Return => String::from("return"),
            Token::Fn => String::from("fn")
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FmtError> {
        f.write_str(&self.to_string())
    }
}

pub struct TokenIterator<'a> {
    line: Rc<RefCell<usize>>,
    column: Rc<RefCell<usize>>,
    last: Option<Result<Token, LexerError>>,
    chars: Peekable<Chars<'a>>
}

#[derive(Debug, Clone, PartialEq)]
pub enum LexerError {
    UnterminatedStringConstant(usize, usize),
    MalformedCharacterCode(String),
    MalformedNumber(String),
    UnexpectedChar(char)
}

impl Error for LexerError {}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FmtError> {
        f.write_str(&match self {
            LexerError::UnterminatedStringConstant(line, column) => format!("Unterminated string constant (line: {}, column: {})", line, column),
            LexerError::MalformedCharacterCode(raw) => format!("Malformed character code: '{}'", raw),
            LexerError::MalformedNumber(raw) => format!("Malformed number: '{}'", raw),
            LexerError::UnexpectedChar(c) => format!("Unexpected character: '{}'", c)
        })
    }
}

impl<'a> TokenIterator<'a> {
    pub fn from(raw: &'a str, line: &mut Rc<RefCell<usize>>, column: &mut Rc<RefCell<usize>>) -> TokenIterator<'a> {
        TokenIterator {
            line: line.clone(),
            column: column.clone(),
            last: None,
            chars: raw.chars().peekable()
        }
    }

    pub fn last(&self) -> &Option<Result<Token, LexerError>> {
        &self.last
    }

    fn next_char(&mut self) -> Option<char> {
        match self.chars.next() {
            Some(c) => {
                if c == '\n' {
                    *self.line.borrow_mut() += 1;
                    *self.column.borrow_mut() = 1;
                } else {
                    *self.column.borrow_mut() +=1;
                }
                Some(c)
            }
            None => None
        }
    }

    fn parse_reference(&mut self) -> Token {
        let mut ident = self.parse_identifier();
        let mut ref_type = Reference::Immutable;

        if "mut" == ident {
            while let Some(&c) = self.chars.peek() {
                if c.is_whitespace() {
                    self.next_char();
                } else {
                    break;
                }
            }
            ident = self.parse_identifier();
            ref_type = Reference::Mutable;
        }
        Token::Reference(ref_type, ident)
    }

    fn parse_identifier(&mut self) -> String {
        let mut ident = String::new();
        while let Some(&c) = self.chars.peek() {
            match c {
                x if x.is_alphanumeric() || x == '_' => {
                    self.next_char();
                    ident.push(c);
                },
                _ => break
            }
        }
        ident
    }

    fn parse_assign_operator(&mut self, operator: Operator) -> Result<Token, LexerError> {
        Ok(if let Some(&'=') = self.chars.peek() {
            self.next_char();
            Token::Assign(Some(operator))
        } else {
            Token::Operator(operator)
        })
    }

    fn parse_number(&mut self, first: char) -> Result<Token, LexerError> {
        let mut result = Vec::new();
        result.push(first);
        let mut radix: Option<u32> = None;
        while let Some(&n) = self.chars.peek() {
            match n {
                '0'...'9' => {
                    result.push(n);
                    self.next_char();
                },
                '.' => {
                    result.push(n);
                    self.next_char();
                    while let Some(&f) = self.chars.peek() {
                        match f {
                            '0'...'9' => {
                                result.push(f);
                                self.next_char();
                            },
                            _ => break
                        }
                    }
                },
                'x' | 'X' => {
                    result.push(n);
                    self.next_char();
                    while let Some(&h) = self.chars.peek() {
                        match h {
                            '0'..='9' | 'a'..='f' | 'A'..='F' => {
                                result.push(h);
                                self.next_char();
                            },
                            _ => break
                        }
                    }
                    radix = Some(16);
                },
                'o' | 'O' => {
                    result.push(n);
                    self.next_char();
                    while let Some(&o) = self.chars.peek() {
                        match o {
                            '0'..='8' => {
                                result.push(o);
                                self.next_char();
                            },
                            _ => break
                        }
                    }
                    radix = Some(8);
                },
                'b' | 'B' => {
                    result.push(n);
                    self.next_char();
                    while let Some(&b) = self.chars.peek() {
                        match b {
                            '0' | '1' => {
                                result.push(b);
                                self.next_char();
                            },
                            _ => break
                        }
                    }
                    radix = Some(2);
                },
                _ => break
            }
        }
        if let Some(radix) = radix {
            let raw: String = result.iter().cloned().skip(2).collect();
            if let Ok(val) = i64::from_str_radix(&raw, radix) {
                Ok(Token::Const(Const::Int(val)))
            } else {
                Err(LexerError::MalformedNumber(result.iter().collect()))
            }
        } else {
            let raw: String = result.iter().cloned().collect();
            if let Ok(val) = raw.parse::<i64>() {
                Ok(Token::Const(Const::Int(val)))
            } else {
                if let Ok(val) = raw.parse::<f64>() {
                    Ok(Token::Const(Const::Float(val)))
                } else {
                    Err(LexerError::MalformedNumber(raw))
                }
            }
        }
    }

    fn inner_next(&mut self) -> Option<Result<Token, LexerError>> {
        while let Some(c) = self.next_char() {
            return Some(match c {
                '0'..='9' => self.parse_number(c),
                '.' => {
                    if let Some(&c) = self.chars.peek() {
                        match c {
                            '0'..='9' => self.parse_number('0'),
                            _ => Ok(Token::Dot)
                        }
                    } else {
                        Ok(Token::Dot)
                    }
                },
                ',' => Ok(Token::Comma),
                ';' => Ok(Token::Semicolon),
                ':' => {
                    if let Some(&':') = self.chars.peek() {
                        self.next_char();
                        Ok(Token::DoubleColon)
                    } else {
                        Ok(Token::Colon)
                    }
                },
                '{' => Ok(Token::Paren(Paren::Curly, Facing::Left)),
                '}' => Ok(Token::Paren(Paren::Curly, Facing::Right)),
                '(' => Ok(Token::Paren(Paren::Round, Facing::Left)),
                ')' => Ok(Token::Paren(Paren::Round, Facing::Right)),
                '[' => Ok(Token::Paren(Paren::Square, Facing::Left)),
                ']' => Ok(Token::Paren(Paren::Square, Facing::Right)),
                '=' => {
                    if let Some(&'=') = self.chars.peek() {
                        self.next_char();
                        Ok(Token::Operator(Operator::Equal))
                    } else {
                        Ok(Token::Assign(None))
                    }
                },
                '>' => {
                    match self.chars.peek() {
                        Some(&'=') => {
                            self.next_char();
                            Ok(Token::Operator(Operator::GreaterOrEqual))
                        },
                        Some(&'>') => {
                            self.next_char();
                            Ok(Token::Operator(Operator::RightShift))
                        },
                        _ => Ok(Token::Operator(Operator::GreaterThan))
                    }
                },
                '<' => {
                    match self.chars.peek() {
                        Some(&'=') => {
                            self.next_char();
                            Ok(Token::Operator(Operator::LessOrEqual))
                        },
                        Some(&'<') => {
                            self.next_char();
                            Ok(Token::Operator(Operator::LeftShift))
                        },
                        Some(&'>') => {
                            self.next_char();
                            Ok(Token::Operator(Operator::NotEqual))
                        }
                        _ => Ok(Token::Operator(Operator::LessThan))
                    }
                },
                '!' => {
                    if let Some(&'=') = self.chars.peek() {
                        self.next_char();
                        Ok(Token::Operator(Operator::NotEqual))
                    } else {
                        Ok(Token::Operator(Operator::Inverse))
                    }
                },
                '^' => self.parse_assign_operator(Operator::Xor),
                '+' => self.parse_assign_operator(Operator::Add),
                '-' => {
                    if let Some(&'>') = self.chars.peek() {
                        self.next_char();
                        Ok(Token::Arrow)
                    } else {
                        if let Some(Ok(last)) = self.last.clone() {
                            if let Token::Operator(_) = last {
                                Ok(Token::Operator(Operator::Negate))
                            } else if let Token::Paren(_, Facing::Left) = last {
                                Ok(Token::Operator(Operator::Negate))
                            } else if let Token::Assign(Some(_)) = last {
                                Ok(Token::Operator(Operator::Negate))
                            } else if Token::If == last || Token::While == last {
                                Ok(Token::Operator(Operator::Negate))
                            } else {
                                self.parse_assign_operator(Operator::Sub)
                            }
                        } else {
                            self.parse_assign_operator(Operator::Sub)
                        }
                    }
                },
                '*' => self.parse_assign_operator(Operator::Mul),
                '/' => {
                    match self.chars.peek() {
                        Some(&'/') => {
                            self.next_char();
                            while let Some(c) = self.next_char() {
                                if c == '\n' {
                                    break;
                                }
                            }
                            continue;
                        }
                        Some(&'*') => {
                            self.next_char();
                            let mut level = 1;
                            while let Some(c) = self.next_char() {
                                match c {
                                    '/' => if let Some('*') = self.next_char() {
                                        level += 1;
                                    },
                                    '*' => if let Some('/') = self.next_char() {
                                        level -= 1;
                                    },
                                    _ => {}
                                }

                                if level == 0 {
                                    break;
                                }
                            }
                            continue;
                        }
                        _ => self.parse_assign_operator(Operator::Div)
                    }
                },
                '%' => self.parse_assign_operator(Operator::Mod),
                '|' => {
                    if let Some(&'|') = self.chars.peek() {
                        self.next_char();
                        Ok(Token::Operator(Operator::Or))
                    } else {
                        self.parse_assign_operator(Operator::ByteOr)
                    }
                },
                '&' => {
                    if let Some(&c) = self.chars.peek() {
                        match c {
                            '&' => {
                                self.next_char();
                                Ok(Token::Operator(Operator::And))
                            },
                            'a'...'z' | 'A'...'Z' | '_' => { //Reference
                                Ok(self.parse_reference())
                            },
                            _ => self.parse_assign_operator(Operator::ByteAnd)
                        }
                    } else {
                        Ok(Token::Operator(Operator::ByteAnd))
                    }
                },
                'a'...'z' | 'A'...'Z' | '_' => {
                    let ident = c.to_string() + &self.parse_identifier();
                    match ident.as_ref() {
                        "true" => Ok(Token::Const(Const::Bool(true))),
                        "false" => Ok(Token::Const(Const::Bool(false))),
                        "if" => Ok(Token::If),
                        "else" => Ok(Token::Else),
                        "let" => Ok(Token::Let),
                        "while" => Ok(Token::While),
                        "loop" => Ok(Token::Loop),
                        "for" => Ok(Token::For),
                        "break" => Ok(Token::Break),
                        "return" => Ok(Token::Return),
                        "fn" => Ok(Token::Fn),
                        "as" => Ok(Token::Operator(Operator::Cast)),
                        x => Ok(Token::Identifier(x.to_string()))
                    }
                },
                '"' => {
                    let mut escape = false;
                    let mut string = String::new();
                    's: while let Some(c) = self.chars.peek().cloned() {
                        self.next_char();
                        if c == '"' && !escape {
                            return Some(Ok(Token::Const(Const::String(string))));
                        } else if c == '\\' {
                            escape = if escape {
                                string.push('\\');
                                false
                            } else {
                                true
                            }
                        } else if escape {
                            escape = false;
                            match c {
                                'n' => string.push_str("\n"),
                                'r' => string.push_str("\r"),
                                't' => string.push_str("\t"),
                                'u' => {
                                    if let Some(&'{') = self.chars.peek() {
                                        self.next_char();
                                        let mut num = String::new();
                                        while let Some(&h) = self.chars.peek() {
                                            match h {
                                                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                                                    num.push(h);
                                                    self.next_char();
                                                },
                                                '}' => {
                                                    self.next_char();
                                                    match u8::from_str_radix(&num, 16) {
                                                        Ok(n) => {
                                                            string.push_str(&format!("{}", n as char));
                                                            break;
                                                        },
                                                        Err(_) => return Some(Err(LexerError::MalformedCharacterCode(num)))
                                                    }
                                                }
                                                o => {
                                                    string.push_str(&num);
                                                    continue 's;
                                                }
                                            }
                                        }
                                    } else {
                                        string.push(c);
                                    }
                                }
                                _ => string.push(c)
                            }
                        } else {
                            string.push(c);
                        }
                    }
                    Err(LexerError::UnterminatedStringConstant(*self.line.borrow(), *self.column.borrow()))
                },
                x if x.is_whitespace() => continue,
                x => Err(LexerError::UnexpectedChar(x))
            })
        }
        None
    }
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner_next() {
            Some(t) => {
                self.last = Some(t);
            },
            None => return None
        }
        self.last.clone()
    }
}

#[derive(Debug, Clone)]
pub struct FnArgDef {
    pub location: (usize, usize),
    pub name: String,
    pub ty: Option<String>,
    pub reference: Option<Reference>
}

impl Display for FnArgDef {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FmtError> {
        let ty = self.ty.clone().unwrap_or(String::from("Any"));
        if let Some(r) = self.reference.clone() {
            let r = if r == Reference::Mutable { "mut " } else { "" };
            write!(f, "{}: &{}{}", self.name, r, ty)
        } else {
            write!(f, "{}: {}", self.name, ty)
        }
    }
}

#[derive(Clone)]
pub struct FnDef {
    pub location: (usize, usize),
    pub name: String,
    pub args: Vec<FnArgDef>,
    pub ret: Option<(String, usize, usize)>,
    pub body: Box<StmtLoc>
}

#[derive(Clone)]
pub enum Expression {
    Const(Const),
    Assign(Box<ExprLoc>, Box<ExprLoc>),
    UnaryOperator(Operator, Box<ExprLoc>),
    BinaryOperator(Operator, Box<ExprLoc>, Box<ExprLoc>),
    Invoke(Option<String>, String, Vec<ExprLoc>, Option<String>, bool),
    Index(String, Box<ExprLoc>),
    Dot(Box<ExprLoc>, Box<ExprLoc>),
    Identifier(String),
    Reference(String, Reference),
    Array(Vec<ExprLoc>),
    Object(HashMap<String, ExprLoc>),
    Cast(Box<ExprLoc>, String)
}

#[derive(Clone)]
pub enum Statement {
    Break(Option<Box<ExprLoc>>),
    Return(Option<Box<ExprLoc>>),
    Block(Vec<StmtLoc>),
    Expression(Box<ExprLoc>),
    If(Box<ExprLoc>, Box<StmtLoc>),
    IfElse(Box<ExprLoc>, Box<StmtLoc>, Box<StmtLoc>),
    Let(String, VarType, Box<ExprLoc>)
}

#[derive(Debug, PartialEq)]
pub enum ParserError {
    LexicalError(LexerError),
    MissingFunctionName(usize, usize),
    MissingParen(char, usize, usize),
    MissingTypeDefinition(usize, usize),
    MissingVariableName(usize, usize),
    MissingArgumentName(usize, usize),
    MissingColon(usize, usize),
    MissingSemicolon(usize, usize),
    MissingPropertyValue(usize, usize),
    MalformedIndexation(usize, usize),
    MalformedPropertyName(usize, usize),
    UnexpectedToken(Token, usize, usize),
    UninitializedVariable(String, usize, usize),
    VariableTypeMismatch(String, String, usize, usize),
    UnexpectedEndOfInput,
}

impl From<LexerError> for ParserError {
    fn from(e: LexerError) -> Self {
        ParserError::LexicalError(e)
    }
}

impl Error for ParserError {}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FmtError> {
        match self {
            ParserError::LexicalError(e) => write!(f, "Lexical error: {}", e),
            ParserError::MissingFunctionName(line, column) => write!(f, "Missing function name (line: {}, column: {})", line, column),
            ParserError::MissingParen(p, line, column) => write!(f, "Missing '{}' (line: {}, column: {})", p, line, column),
            ParserError::MissingTypeDefinition(line, column) => write!(f, "Missing type definition (line: {}, column: {})", line, column),
            ParserError::MissingVariableName(line, column) => write!(f, "Missing variable name (line: {}, column: {})", line, column),
            ParserError::MissingArgumentName(line, column) => write!(f, "Missing argument name (line: {}, column: {})", line, column),
            ParserError::MissingPropertyValue(line, column) => write!(f, "Missing property value (line: {}, column: {})", line, column),
            ParserError::MissingColon(line, column) => write!(f, "Missing ':' (line: {}, column: {})", line, column),
            ParserError::MissingSemicolon(line, column) => write!(f, "Missing ';' (line: {}, column: {})", line, column),
            ParserError::MalformedIndexation(line, column) => write!(f, "Malformed indexation (line: {}, column: {})", line, column),
            ParserError::MalformedPropertyName(line, column) => write!(f, "Malformed property name (line: {}, column: {})", line, column),
            ParserError::UnexpectedToken(t, line, column) => write!(f, "Unexpected token: '{}' (line: {}, column: {})", t, line, column),
            ParserError::UninitializedVariable(v, line, column) => write!(f, "Variable '{}' should be initialized (line: {}, column: {})", v, line, column),
            ParserError::VariableTypeMismatch(r, fd, line, column) => write!(f, "Variable type mismatch: required '{}' but got '{}' (line: {}, column: {})", r, fd, line, column),
            ParserError::UnexpectedEndOfInput => write!(f, "Unexpected end of input")
        }
    }
}

fn parse_expression(input: &mut Peekable<TokenIterator>, token: Token, location: &Location) -> Result<ExprLoc, ParserError> {
    let left = parse_unary(input, token, location)?;
    parse_binary(input, 0, left, location)
}

fn parse_invocation(input: &mut Peekable<TokenIterator>, static_type: Option<String>, ident: String, location: &Location) -> Result<ExprLoc, ParserError> {
    let mut args = Vec::new();

    let empty = Some(&Ok(Token::Paren(Paren::Round, Facing::Right))) == input.peek();

    if !empty {
        while let Some(token) = input.next() {
            let token = token?;
            args.push(parse_expression(input, token.clone(), location)?);
            if let Some(token) = input.peek().cloned() {
                let token = token?;
                if Token::Comma == token {
                    input.next();
                } else if Token::Paren(Paren::Round, Facing::Right) == token {
                    input.next();
                    return if Some(&Ok(Token::Arrow)) == input.peek() {
                        input.next();
                        if let Some(token) = input.peek().cloned() {
                            if let Token::Identifier(ty) = token? {
                                input.next();
                                return Ok(ExprLoc::new(Expression::Invoke(static_type, ident, args, Some(ty), false), location));
                            }
                        }
                        Err(ParserError::MissingTypeDefinition(location.line(), location.column()))
                    } else {
                        Ok(ExprLoc::new(Expression::Invoke(static_type, ident,args, None, false), location))
                    }
                } else {
                    return Err(ParserError::UnexpectedToken(token, location.line(), location.column()))
                }
            }
        }
    }

    match input.peek().cloned() {
        Some(Ok(Token::Paren(Paren::Round, Facing::Right))) => {
            input.next();
            if Some(&Ok(Token::Arrow)) == input.peek() {
                input.next();
                if let Some(token) = input.next() {
                    let ty = parse_type(input, token?, location)?;
                    Ok(ExprLoc::new(Expression::Invoke(static_type, ident, args, Some(ty), false), location))
                } else {
                    Err(ParserError::MissingTypeDefinition(location.line(), location.column()))
                }
            } else {
                Ok(ExprLoc::new(Expression::Invoke(static_type, ident, args, None, false), location))
            }
        }
        _ => Err(ParserError::MissingParen(')', location.line(), location.column())),
    }
}

fn parse_indexation(input: &mut Peekable<TokenIterator>, ident: String, location: &Location) -> Result<ExprLoc, ParserError> {
    if let Some(token) = input.next() {
        let index = parse_expression(input, token?, location)?;
        if let Some(&Ok(Token::Paren(Paren::Square, Facing::Right))) = input.peek() {
            return Ok(ExprLoc::new(Expression::Index(ident, Box::new(index)), location))
        }
    }
    Err(ParserError::MalformedIndexation(location.line(), location.column()))
}

fn parse_identifier(input: &mut Peekable<TokenIterator>, ident: String, location: &Location) -> Result<ExprLoc, ParserError> {
    if let Some(token) = input.peek().cloned() {
        let token = token?;
        match token {
            Token::Paren(par, Facing::Left) => {
                match par {
                    Paren::Round => {
                        input.next();
                        return parse_invocation(input, None, ident, location);
                    },
                    Paren::Square => {
                        input.next();
                        return parse_indexation(input, ident, location);
                    },
                    _ => {}
                }
            },
            Token::DoubleColon => {
                input.next();
                if let Some(token) = input.peek().cloned() {
                    let token = token?;
                    return if let Token::Identifier(name) = token {
                        input.next();
                        if let Some(&Ok(Token::Paren(Paren::Round, Facing::Left))) = input.peek() {
                            input.next();
                            parse_invocation(input, Some(ident), name, location)
                        } else {
                            Err(ParserError::MissingParen('(', location.line(), location.column()))
                        }
                    } else {
                        Err(ParserError::MissingFunctionName(location.line(), location.column()))
                    }
                }
            },
            _ => {}
        }
    }
    Ok(ExprLoc::new(Expression::Identifier(ident), location))
}

fn parse_paren_expr(input: &mut Peekable<TokenIterator>, location: &Location) -> Result<ExprLoc, ParserError> {
    if let Some(token) = input.next() {
        let token = token?;
        if Token::Paren(Paren::Round, Facing::Right) == token {
            return Ok(ExprLoc::new(Expression::Const(Const::Unit), location));
        } else {
            let expr = parse_expression(input, token, location)?;
            if let Some(token) = input.peek().cloned() {
                if Token::Paren(Paren::Round, Facing::Right) == token? {
                    input.next();
                    return Ok(expr);
                }
            }
        }
    }
    Err(ParserError::MissingParen(')', location.line(), location.column()))
}

fn parse_array_expr(input: &mut Peekable<TokenIterator>, location: &Location) -> Result<ExprLoc, ParserError> {
    let mut elements = Vec::new();

    let empty = Some(&Ok(Token::Paren(Paren::Square, Facing::Right))) == input.peek();

    if !empty {
        while let Some(token) = input.next() {
            let token = token?;
            elements.push(parse_expression(input, token.clone(), location)?);
            if let Some(token) = input.peek().cloned() {
                let token = token?;
                if Token::Comma == token {
                    input.next();
                } else if Token::Paren(Paren::Square, Facing::Right) == token {
                    input.next();
                    return Ok(ExprLoc::new(Expression::Array(elements), location));
                } else {
                    return Err(ParserError::UnexpectedToken(token, location.line(), location.column()))
                }
            }
        }
    }

    match input.peek() {
        Some(&Ok(Token::Paren(Paren::Square, Facing::Right))) => {
            input.next();
            Ok(ExprLoc::new(Expression::Array(elements), location))
        }
        _ => Err(ParserError::MissingParen(']', location.line(), location.column())),
    }
}

fn parse_object_expr(input: &mut Peekable<TokenIterator>, location: &Location) -> Result<ExprLoc, ParserError> {
    let mut elements = HashMap::new();
    while let Some(token) = input.next() {
        let token = token?;
        if let Token::Paren(Paren::Curly, Facing::Right) = token {
            break;
        } else if Token::Comma == token {
            continue;
        } else {
            let key = parse_expression(input, token, location)?;
            if let ExprLoc { expression: Expression::Identifier(key), location: (line, column) } = key {
                if let Some(Ok(Token::Colon)) = input.next() {
                    if let Some(token) = input.next() {
                        let value = parse_expression(input, token?, location)?;
                        elements.insert(key, value);
                    } else {
                        return Err(ParserError::MissingPropertyValue(line, column))
                    }
                } else {
                    return Err(ParserError::MissingColon(line, column))
                }
            } else {
                return Err(ParserError::MalformedPropertyName(location.line(), location.column()))
            }

        }
    }
    Ok(ExprLoc::new(Expression::Object(elements), location))
}

fn parse_primary(input: &mut Peekable<TokenIterator>, token: Token, location: &Location) -> Result<ExprLoc, ParserError> {
    match token {
        Token::Const(c) => Ok(ExprLoc::new(Expression::Const(c), location)),
        Token::Identifier(i) => parse_identifier(input, i, location),
        Token::Reference(r, i) => Ok(ExprLoc::new(Expression::Reference(i, r), location)),
        Token::Paren(par, Facing::Left) => {
            match par {
                Paren::Round => parse_paren_expr(input, location),
                Paren::Square => parse_array_expr(input, location),
                Paren::Curly => parse_object_expr(input, location)
            }
        }
        t => {
            println!("prim");
            Err(ParserError::UnexpectedToken(t, location.line(), location.column()))
        },
    }
}

fn parse_unary(input: &mut Peekable<TokenIterator>, token: Token, location: &Location) -> Result<ExprLoc, ParserError> {
    if let Token::Operator(operator) = token.clone() {
        if Operator::Inverse == operator || Operator::Negate == operator {
            if let Some(token) = input.next() {
                let arg = parse_primary(input, token?, location)?;
                return Ok(ExprLoc::new(Expression::UnaryOperator(operator, Box::new(arg)), location));
            }
        }
    }
    parse_primary(input, token, location)
}

fn parse_binary(input: &mut Peekable<TokenIterator>, priority: i32, left: ExprLoc, location: &Location) -> Result<ExprLoc, ParserError> {
    let mut left = left;

    loop {
        let curr_priority = if let Some(token) = input.peek().cloned() {
            token?.priority()
        } else {
            -1
        };
        if curr_priority < priority {
            return Ok(left);
        }
        if let Some(op) = input.next() {
            let op = op?;

            if Token::Operator(Operator::Cast) == op {
                if let Some(Ok(Token::Identifier(ty))) = input.peek().cloned() {
                    input.next();
                    left = ExprLoc::new(Expression::Cast(Box::new(left), ty), location);
                    continue;
                } else {
                    return Err(ParserError::MissingTypeDefinition(location.line(), location.column()));
                }
            }

            let mut right = if let Some(token) = input.next() {
                parse_unary(input, token?, location)?
            } else {
                return Err(ParserError::UnexpectedEndOfInput)
            };

            let next_priority = if let Some(token) = input.peek().cloned() {
                token?.priority()
            } else {
                -1
            };

            if curr_priority < next_priority {
                right = parse_binary(input, curr_priority + 1, right, location)?;
            } else if curr_priority >= 100 {
                right = parse_binary(input, curr_priority, right, location)?;
            }

            left = ExprLoc::new(match op {
                Token::Assign(op) => {
                    let mut to = if let Some(op) = op {
                        Box::new(ExprLoc {
                            expression: Expression::BinaryOperator(op, Box::new(left.clone()), Box::new(right)),
                            location: left.location.clone()
                        })
                    } else {
                        Box::new(right)
                    };
                    Expression::Assign(Box::new(left), to)
                },
                Token::Operator(op) => Expression::BinaryOperator(op, Box::new(left), Box::new(right)),
                Token::Dot => Expression::Dot(Box::new(left), Box::new(right)),
                _ => {
                    println!("binop");
                    return Err(ParserError::UnexpectedToken(op, location.line(), location.column()))
                }
            }, location);
        }
    }
}

fn parse_block(input: &mut Peekable<TokenIterator>, opened: bool, location: &Location) -> Result<StmtLoc, ParserError> {
    if !opened {
        if let Some(token) = input.peek().cloned() {
            let token = token?;
            if Token::Paren(Paren::Curly, Facing::Left) == token {
                input.next();
            }
        } else {
            return Err(ParserError::MissingParen('{', location.line(), location.column()));
        }
    }
    let mut body = Vec::new();
    while let Some(token) = input.next() {
        let token: Token = token?;
        if Token::Paren(Paren::Curly, Facing::Right) == token {
            break;
        } else if Token::Semicolon == token {
            continue;
        } else {
            body.push(parse_statement(input, token, location)?)
        }
    }
    Ok(StmtLoc::new(Statement::Block(body), location))
}

fn parse_type(input: &mut Peekable<TokenIterator>, token: Token, location: &Location) -> Result<String, ParserError> {
    if let Token::Identifier(name) = token {
        Ok(name)
    } else if let Token::Reference(r, name) = token {
        Ok(r.format(&name))
    } else {
        Err(ParserError::UnexpectedToken(token, location.line(), location.column()))
    }
}

fn parse_arg(input: &mut Peekable<TokenIterator>, token: Token, location: &Location) -> Result<FnArgDef, ParserError> {
    if let Token::Identifier(name) = token {
        if let Some(&Ok(Token::Colon)) = input.peek() {
            input.next();
            if let Some(token) = input.next() {
                let token = token?;
                let ty = parse_type(input, token, location)?;
                Ok(FnArgDef {
                    location: (location.line(), location.column()),
                    name,
                    ty: Some(ty),
                    reference: None
                })
            } else {
                Err(ParserError::MissingTypeDefinition(location.line(), location.column()))
            }
        } else {
            Ok(FnArgDef {
                location: (location.line(), location.column()),
                name,
                ty: None,
                reference: None
            })
        }
    } else if let Token::Reference(r, name) = token {
        Ok(FnArgDef {
            location: (location.line(), location.column()),
            name,
            ty: None,
            reference: Some(r)
        })
    } else {
        Err(ParserError::MissingArgumentName(location.line(), location.column()))
    }
}

fn parse_args(input: &mut Peekable<TokenIterator>, location: &Location) -> Result<Vec<FnArgDef>, ParserError> {
    if let Some(token) = input.peek().cloned() {
        if Token::Paren(Paren::Round, Facing::Right) == token? {
            input.next();
            return Ok(Vec::new());
        }
    }
    let mut args = Vec::new();
    while let Some(token) = input.next() {
        args.push(parse_arg(input, token?, location)?);
        if let Some(token) = input.peek().cloned() {
            let token = token?;
            if Token::Comma == token {
                input.next();
            } else if Token::Paren(Paren::Round, Facing::Right) == token {
                input.next();
                return Ok(args);
            } else {
                return Err(ParserError::UnexpectedToken(token, location.line(), location.column()))
            }
        }
    }
    Ok(args)
}

fn parse_return_type(input: &mut Peekable<TokenIterator>, location: &Location) -> Result<Option<(String, usize, usize)>, ParserError> {
    if let Some(token) = input.peek().cloned() {
        if Token::Arrow == token? {
            input.next();
            if let Some(token) = input.next() {
                let ty = parse_type(input, token?, location)?;
                return Ok(Some((ty, location.line(), location.column())));
            }
        }
    }
    Ok(None)
}

fn parse_function(input: &mut Peekable<TokenIterator>, location: &Location) -> Result<FnDef, ParserError> {
    let fn_location = (location.line(), location.column());
    let name: String = if let Some(token) = input.next() {
        if let Token::Identifier(ident) = token? {
            ident
        } else {
            return Err(ParserError::MissingFunctionName(location.line(), location.column()))
        }
    } else {
        return Err(ParserError::MissingFunctionName(location.line(), location.column()))
    };
    if let Some(token) = input.peek().cloned() {
        let token = token?;
        if Token::Paren(Paren::Round, Facing::Left) == token {
            input.next();
            let args = parse_args(input, location)?;
            let ret = parse_return_type(input, location)?;
            let body = parse_block(input, false, location)?;
            Ok(FnDef { location: fn_location, name, args, ret, body: Box::new(body) })
        } else {
            Err(ParserError::UnexpectedToken(token, location.line(), location.column()))
        }
    } else {
        Err(ParserError::MissingParen('(', location.line(), location.column()))
    }
}

fn parse_if(input: &mut Peekable<TokenIterator>, location: &Location) -> Result<StmtLoc, ParserError> {
    if let Some(token) = input.next() {
        let guard = parse_expression(input, token?, location)?;
        let main_branch = parse_block(input, false, location)?;
        if let Some(&Ok(Token::Else)) = input.peek() {
            input.next();
            let else_branch = parse_block(input, false, location)?;
            Ok(StmtLoc::new(Statement::IfElse(Box::new(guard), Box::new(main_branch), Box::new(else_branch)), location))
        } else {
            Ok(StmtLoc::new(Statement::If(Box::new(guard), Box::new(main_branch)), location))
        }
    } else {
        Err(ParserError::UnexpectedEndOfInput)
    }
}

fn parse_let(input: &mut Peekable<TokenIterator>, location: &Location) -> Result<StmtLoc, ParserError> {
    let name = if let Some(Ok(Token::Identifier(ident))) = input.next() {
        ident
    } else {
        return Err(ParserError::MissingVariableName(location.line(), location.column()))
    };
    let mut ty = VarType::Undefined;
    if let Some(token) = input.peek().cloned() {
        let token = token?;
        if Token::Colon == token {
            input.next();
            if let Some(Ok(Token::Identifier(ident))) = input.peek().cloned() {
                input.next();
                ty = if ident == "Any" {
                    VarType::Undefined
                } else {
                    VarType::Defined(ident, None)
                }
            } else if let Some(Ok(Token::Reference(r, ident))) = input.peek().cloned() {
                input.next();
                ty = VarType::Defined(ident, Some(r))
            }
        }
    }
    let (line, column) = (location.line(), location.column());
    if let Some(token) = input.peek().cloned() {
        if Token::Assign(None) == token? {
            input.next();
            if let Some(token) = input.peek().cloned() {
                input.next();
                let val = parse_expression(input, token?, location)?;
                if let Expression::Const(ref c) = &val.expression {
                    let cty = c.simple_type_name();
                    match &ty {
                        VarType::Undefined => {},
                        VarType::Defined(ty, _) => {
                            if ty != &cty {
                                return Err(ParserError::VariableTypeMismatch(ty.clone(), cty, line, column))
                            }
                        }
                    };
                }
                return Ok(StmtLoc::new(Statement::Let(name, ty, Box::new(val)), location));
            }
        }
    }
    Err(ParserError::UninitializedVariable(name, line, column))
}

fn parse_statement(input: &mut Peekable<TokenIterator>, token: Token, location: &Location) -> Result<StmtLoc, ParserError> {
    match token {
        Token::If => parse_if(input, location),
        /*Token::For => parse_for(input),
        Token::While => parse_while(input),
        Token::Loop => parse_loop(input),*/
        Token::Let => parse_let(input, location),
        Token::Break => {
            if let Some(token) = input.peek().cloned() {
                let token: Token = token?;
                if Token::Semicolon == token {
                    Ok(StmtLoc::new(Statement::Break(None), location))
                } else {
                    let ret = parse_expression(input, token, location)?;
                    Ok(StmtLoc::new(Statement::Break(Some(Box::new(ret))), location))
                }
            } else {
                Err(ParserError::MissingSemicolon(location.line(), location.column()))
            }
        },
        Token::Return => {
            if let Some(token) = input.peek().cloned() {
                let token: Token = token?;
                if Token::Semicolon == token {
                    Ok(StmtLoc::new(Statement::Return(None), location))
                } else {
                    let ret = parse_expression(input, token, location)?;
                    Ok(StmtLoc::new(Statement::Return(Some(Box::new(ret))), location))
                }
            } else {
                Err(ParserError::MissingSemicolon(location.line(), location.column()))
            }
        },
        Token::Paren(Paren::Curly, Facing::Left) => parse_block(input, true, location),
        t => Ok(StmtLoc::new(Statement::Expression(Box::new(parse_expression(input, t, location)?)), location))
    }
}

pub fn parse(input: &mut Peekable<TokenIterator>, location: Location) -> Result<(Vec<FnDef>, Vec<StmtLoc>), ParserError> {
    let mut functions = Vec::new();
    let mut statements = Vec::new();

    while let Some(token) = input.next() {
        match token? {
            Token::Fn => functions.push(parse_function(input, &location)?),
            t => statements.push(parse_statement(input, t, &location)?)
        }
        if let Some(token) = input.peek().cloned() {
            if Token::Semicolon == token? {
                input.next();
            }
        }
    }

    Ok((functions, statements))
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarType {
    Undefined,
    Defined(String, Option<Reference>)
}

impl Display for VarType {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FmtError> {
        match self {
            VarType::Undefined => write!(f, "Any"),
            VarType::Defined(ty, r) => {
                if let Some(r) = r {
                    write!(f, "{}", r.format(ty))
                } else {
                    write!(f, "{}", ty)
                }
            }
        }
    }
}