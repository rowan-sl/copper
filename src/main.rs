#[macro_use]
extern crate log;

use std::{io::Read, fs::OpenOptions};

use anyhow::Result;

fn main() -> Result<()> {
    pretty_env_logger::formatted_builder()
        .filter_level(log::LevelFilter::Debug)
        .init();
    let mut file = OpenOptions::new().read(true).write(false).open("test.mc")?;
    let mut raw = String::new();
    file.read_to_string(&mut raw)?;
    let _tokens = parser::tokenize(raw)?;
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
        #[error("Invalid token folowing const, const ident, or let expression (not a valid variable name)")]
        InavlidVarName,
        #[error("Invalid type name")]
        InvalidType,
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

    fn qualifies_for_var(dat: &String) -> bool {
        let mut chars = dat.chars().peekable();
        (chars.peek().unwrap().is_ascii_alphabetic() || *chars.peek().unwrap() == '_') && chars.fold(true, |acc, elem| acc && (elem.is_ascii_alphanumeric() || elem == '_'))
    }

    pub fn base_lexer(data: String) -> Result<Vec<BaseToken>, ParseError> {
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
                        trace!("parsing numeric literal");
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
                        trace!("new word parsed: {}", current_word);
                        if !current_word.is_empty() {
                            base_tokens.push(BaseToken::Word(current_word));
                            current_word = String::new();
                        }
                    }
                    ';' => {
                        trace!("reached EOL");
                        if !current_word.is_empty() {
                            base_tokens.push(BaseToken::Word(current_word));
                            current_word = String::new();
                        }
                        base_tokens.push(BaseToken::EOL);
                    }
                    ':'  | '=' | '(' | ')' | '{' | '}' | '.' => {
                        if !current_word.is_empty() {
                            base_tokens.push(BaseToken::Word(current_word));
                            current_word = String::new();
                        }
                        base_tokens.push(BaseToken::Word(ch.to_string()))
                    }
                    other => {
                        trace!("parsing next word");
                        current_word.push(other);
                        while let Some(next) = chars.peek() {
                            match next {
                                ch if ch.is_ascii_whitespace() => {
                                    break;
                                }
                                ';' => {
                                    break;
                                }
                                '"' | ':'  | '=' | '(' | ')' | '{' | '}' | '.' => {
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

    pub fn advanced_lexer(tokens: Vec<BaseToken>) -> Result<Vec<Token>, ParseError> {
        let mut token_stream = tokens.into_iter().peekable();
        let mut tokens: Vec<Token> = vec![];

        while let Some(next_token) = token_stream.next() {
            match next_token {
                BaseToken::EOL => tokens.push(Token::EOL),
                BaseToken::NumberLiteral(num) => tokens.push(Token::Literal(Literal::Number(num))),
                BaseToken::StringLiteral(string) => tokens.push(Token::Literal(Literal::String(string))),
                BaseToken::Word(word) => {
                    match &*word {
                        "const" => {
                            let t = match token_stream.peek() {
                                Some(BaseToken::Word(word)) if word == "ident" => {
                                    let _ = token_stream.next();
                                    Token::Keyword(Keyword::ObjectIdent)
                                }
                                _ => Token::Keyword(Keyword::Const)
                            };
                            tokens.push(t);
                        },
                        "let" => tokens.push(Token::Keyword(Keyword::Let)), //TODO intigrate this into the `const` branch
                        "=" => tokens.push(Token::Operator(Operator::Assign)),
                        "." => tokens.push(Token::Operator(Operator::Dot)),
                        ":" => tokens.push(Token::OtherGrammar(OtherGrammar::TypeHint)),
                        "(" | ")" | "{" | "}" => {
                            tokens.push(match &*word {
                                "(" => Token::ParenBegin,
                                ")" => Token::ParenEnd,
                                "{" => Token::ScopeBegin,
                                "}" => Token::ScopeEnd,
                                _ => unreachable!()
                            });
                        }
                        "true" => tokens.push(Token::Literal(Literal::Bool(true))),
                        "false" => tokens.push(Token::Literal(Literal::Bool(false))),
                        "if" => tokens.push(Token::Keyword(Keyword::If)),
                        "else" => tokens.push(Token::Keyword(Keyword::Else)),
                        other => {
                            tokens.push(Token::Word(other.to_string()))
                        }
                    }
                }
            }
        }

        let capacity = tokens.len();
        let mut token_stream = tokens.into_iter();
        let mut tokens: Vec<Token> = Vec::with_capacity(capacity);

        while let Some(next_token) = token_stream.next() {
            match next_token {
                Token::Keyword(Keyword::Let | Keyword::Const | Keyword::ObjectIdent) => {
                    tokens.push(next_token);
                    if let Some(Token::Word(var_name)) = token_stream.next() {
                        if qualifies_for_var(&var_name) {
                            tokens.push(Token::Ident(var_name))
                        } else {
                            return Err(ParseError::InavlidVarName)
                        }
                    } else {
                        return Err(ParseError::InavlidVarName)
                    }
                }
                Token::OtherGrammar(OtherGrammar::TypeHint) => {
                    tokens.push(Token::OtherGrammar(OtherGrammar::TypeHint));
                    if let Some(Token::Word(type_str)) = token_stream.next() {
                        if qualifies_for_var(&type_str) {
                            tokens.push(Token::Type(type_str));
                        }
                    } else {
                        return Err(ParseError::InvalidType)
                    }

                }
                Token::Word(word) if qualifies_for_var(&word) => {
                    tokens.push(Token::Ident(word))
                }
                a => tokens.push(a)
            }
        }

        Ok(tokens)
    }

    pub fn tokenize(data: String) -> Result<Vec<Token>, ParseError> {
        let base_tokens = base_lexer(data)?;
        debug!("{base_tokens:#?}");
        let tokens = advanced_lexer(base_tokens)?;
        debug!("{tokens:#?}");
        Ok(tokens)
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub enum Keyword {
        Const,
        /// object identifier (such as a bound block name. MUST BE COMPILE TIME KNOWN)
        ObjectIdent,
        Let,
        If,
        Else,
    }

    #[derive(Clone, Debug, PartialEq)]
    pub enum Literal {
        String(String),
        Number(f64),
        Bool(bool),
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub enum Operator {
        // `=`
        Assign,
        // `.`
        Dot,
        // PlusEquals,
        // MinusEquals,
        // MulEquals,
        // DivEquals,
    }

    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub enum OtherGrammar {
        /// type hint `:` symbol
        TypeHint,
    }

    #[derive(Clone, Debug, PartialEq)]
    pub enum Token {
        ScopeBegin,
        ScopeEnd,
        ParenBegin,
        ParenEnd,
        /// end of line / semicolon
        EOL,
        /// Literals, such as "text" or 123.456
        Literal(Literal),
        /// A Keyword, such as `let` or `const`
        Keyword(Keyword),
        /// some other grammar thing
        OtherGrammar(OtherGrammar),
        /// an operation, such as `=` or `.`
        Operator(Operator),
        /// something else
        Word(String),
        /// an identifer
        Ident(String),
        /// a type (as a string)
        Type(String),
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
