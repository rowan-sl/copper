#[macro_use]
extern crate log;

use std::{io::Read, fs::OpenOptions};

use anyhow::Result;

fn main() -> Result<()> {
    pretty_env_logger::formatted_builder()
        .filter_level(log::LevelFilter::Trace)
        .init();
    let mut file = OpenOptions::new().read(true).write(false).open("test.mc")?;
    let mut raw = String::new();
    file.read_to_string(&mut raw)?;
    info!("{:#?}", parser::parse_base_tokens(raw));
    Ok(())
}

pub mod parser {
    #[derive(Debug, thiserror::Error)]
    pub enum ParseError {
        #[error("EOF while parsing string literal")]
        IncompleteStringLiteral,
        #[error("Incomplete comment expression (found single `/`)")]
        IncompleteCommentExpr,
        #[error("Number literal terminated with an invalid charecter")]
        InvalidNumberLiteral,
    }

    #[derive(Clone, Debug, PartialEq)]
    pub enum BaseToken {
        StringLiteral(String),
        NumberLiteral(f64),
        /// end of line / semicolon
        EOL,
        /// any other coherent charecter string (must be alphanumeric)
        Word(String),

    }

    pub fn parse_base_tokens(data: String) -> Result<Vec<BaseToken>, ParseError> {
        let mut base_tokens: Vec<BaseToken> = vec![];
        let mut chars = data.chars().peekable();
        let mut current_word = String::new();
        'base_parse: loop {
            let ch = chars.next();

            if let Some(ch) = ch {
                match ch {
                    '/' => {
                        trace!("starting to parse comment");
                        if let Some('/') = chars.next() {
                            'comment: loop {
                                match chars.next() {
                                    Some('\n') => break 'comment,
                                    Some(_) => {}
                                    None => break 'base_parse,
                                }
                            }
                        } else {
                            return Err(ParseError::IncompleteCommentExpr)
                        }
                    }
                    '\"' => {
                        trace!("parsing string literal");
                        let mut string_literal: String = String::new();
                        let mut escaped = false;
                        loop {
                            match chars.next() {
                                Some('\\') => {
                                    if escaped {
                                        // it is being escaped, so put it in as a literal value reset escaped
                                        string_literal.push('\\');
                                        escaped = false;
                                    } else {
                                        // not being escaped, so set escaped to true
                                        escaped = true
                                    }
                                }
                                Some('"') => {
                                    if escaped {
                                        // odd number of \ so it IS being escaped
                                        string_literal.push('"');
                                        escaped = false;
                                    } else {
                                        break;
                                    }
                                }
                                Some(sch) => {
                                    match (sch, escaped) {
                                        ('n', true) => {
                                            string_literal.push('\n');
                                            escaped = false;
                                        }
                                        ('t', true) => {
                                            string_literal.push('\t');
                                            escaped = false;
                                        }
                                        (other, true) => {
                                            warn!("unhandled/unnecessary escape value! (if you want more escape sequences, please open an issue)");
                                            escaped = false;
                                            string_literal.push(other);
                                        }
                                        (other, false) => {
                                            string_literal.push(other);
                                        }
                                    }
                                }
                                None => {
                                    return Err(ParseError::IncompleteStringLiteral)
                                }
                            }
                        }
                        base_tokens.push(BaseToken::StringLiteral(string_literal));
                    }
                    num if num.is_digit(10) => {
                        info!("parsing numeric literal");
                        let mut digits: String = String::from(num);
                        loop {
                            match chars.peek() {
                                Some(num) if num.is_digit(10) => {
                                    digits.push(chars.next().unwrap());
                                }
                                Some(&'.') => {
                                    digits.push(chars.next().unwrap());
                                    match chars.peek() {
                                        Some(num) if num.is_digit(10) => {}
                                        Some(_) => return Err(ParseError::InvalidNumberLiteral),
                                        None => break,
                                    }
                                }
                                Some(_) => return Err(ParseError::InvalidNumberLiteral),
                                None => break,
                            }
                        }
                        let number = digits.parse::<f64>().unwrap();
                        base_tokens.push(BaseToken::NumberLiteral(number));
                    }
                    ch if ch.is_ascii_whitespace() => {
                        info!("new word parsed: {}", current_word);
                        if !current_word.is_empty() {
                            base_tokens.push(BaseToken::Word(current_word));
                            current_word = String::new();
                        }
                    }
                    ';' => {
                        info!("reached EOL");
                        if !current_word.is_empty() {
                            base_tokens.push(BaseToken::Word(current_word));
                            current_word = String::new();
                        }
                        base_tokens.push(BaseToken::EOL);
                    }
                    other => {
                        info!("parsing next word");
                        current_word.push(other);
                        while let Some(next) = chars.peek() {
                            match next {
                                ch if ch.is_ascii_whitespace() => {
                                    break;
                                }
                                ';' => {
                                    break;
                                }
                                _ => {
                                    current_word.push(chars.next().unwrap());
                                }
                            }
                        }
                    }
                }
            } else {
                break;
            }
        }
        Ok(base_tokens)
    }

    pub fn tokenize(data: String) -> Result<impl Iterator<Item = Token>, ParseError> {
        let base_tokens = parse_base_tokens(data)?;
        Ok(vec![].into_iter())
    }

    pub enum Keyword {
        Const,
        /// object identifier (such as a bound block name. MUST BE COMPILE TIME KNOWN)
        ObjectIdent,
        Let,
    }

    pub enum Type {
        String,
        Number,
        Null,
    }

    pub enum Literal {
        String(String),
        Number(f64),
    }

    pub enum AssignmentOperator {
        Assign,
        // PlusEquals,
        // MinusEquals,
        // MulEquals,
        // DivEquals,
    }

    pub enum Token {
        ScopeBegin,
        ScopeEnd,
        /// end of line / semicolon
        EOL,
        /// Literals, such as "text" or 123.456
        Literal(Literal),
        /// A Keyword, such as `let` or `const`
        Keyword(Keyword),
        /// a variable type
        Type(Type),
        /// a ident, such as a variable name
        Identifier(String),
        /// a assignment operation, such as `=`
        Assignment(AssignmentOperator),
    }
}


// TODO convert information to a spreadsheet
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Instruction {
    Read,
    /*
    class: io
    priority: high
    status: unimplemented
    */
    Write,
    /*
    class: io
    priority: high
    status: unimplemented
    */
    Draw,
    /*
    class: graphics
    priority: mid
    status: unimplemented
    */
    Print,
    /*
    class: io, debug
    priority: VERY HIGH
    status: unimplemented
    */
    DrawFlush,
    /*
    class: graphics
    priority: mid,
    status: unimplemented
    */
    PrintFlush,
    /*
    class: io, debug
    priority: very high
    status: unimplemented
    */
    GetLink,
    /*
    class: ??
    priority: low
    status: unimplemented
    */
    Controll,
    /*
    class: io, unit interface
    priority: high
    status: unimplemented
    */
    Radar,
    /*
    class: ??
    priority: low
    status: unimplemented
    */
    Sensor,
    /*
    class: io, unit interface
    priority: high
    status: unimplemented
    */
    Set,
    /*
    class: basic operation
    priority: very very high
    status: unimplemented
    */
    Operation,
    /*
    class: basic operation
    priority: very very high
    status: unimplemented
    */
    Wait,
    /*
    class: basic operation
    priority: very very high
    status: unimplemented
    */
    Lookup,
    /*
    class: ??
    priority: low
    status: unimplemented
    */
    PackColor,
    /*
    class: basic operation, graphics
    priority: mid
    status: unimplemented
    */
    End {},
    /*
    class: basic operation, congroll flow
    priority: very very high
    status: done

    args: none
    mlog rep: "end"
    */
    Jump {
        addr: usize,
        condition: JumpCondition,
    },
    /*
    class: basic operation, controll flow
    priority: very very high
    status: in progress

    args: {addr} {conditional} {value1} {value2}

    mlog rep:
    ```
    jump 8 equal x false
    jump 8 notEqual x false
    jump 8 lessThan x false
    jump 8 lessThanEq x false
    jump 8 greaterThan x false
    jump 8 greaterThanEq x false
    jump 8 strictEqual x false
    jump 8 always x false
    draw clear 0 0 0 0 0 0
    ```
    */
    UnitBind,
    /*
    class: unit controll
    priority: very low
    status: unimplemented
    */
    UnitControll,
    /*
    class: unit controll
    priority: very low
    status: unimplemented
    */
    UnitRadar,
    /*
    class: unit controll
    priority: very low
    status: unimplemented
    */
    UnitLocate,
    /*
    class: unit controll
    priority: very low
    status: unimplemented
    */
    NoOp,
    /*
    class: basic operation, not in mindustry
    priority: very very high
    status: unimplemented

    args: none
    mlog rep: does not exist in mlog
    */
}

impl ToString for Instruction {
    fn to_string(&self) -> String {
        use Instruction::*;
        match self {
            End {} => {
                String::from("end")
            }

            _ => todo!()
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum JumpCondition {
    Equal,
    NotEqual,
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    StrictEqual,
    /// Note: this is the only one that does NOT take arguments
    Always,
}

impl ToString for JumpCondition {
    fn to_string(&self) -> String {
        use JumpCondition::*;
        match self {
            Equal => String::from("equal"),
            NotEqual => String::from("notEqual"),
            LessThan => String::from("lessThan"),
            LessThanEq => String::from("lessThanEq"),
            GreaterThan => String::from("greaterThan"),
            GreaterThanEq => String::from("greaterThanEq"),
            StrictEqual => String::from("strictEqual"),
            Always => String::from("always"),
        }
    }
}
