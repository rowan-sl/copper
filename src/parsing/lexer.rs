#[derive(Clone, Debug, thiserror::Error)]
pub enum LexError {
    #[error("EOF while parsing string literal")]
    IncompleteStringLiteral,
    #[error("Incomplete comment expression (found single `/`)")]
    IncompleteCommentExpr,
    #[error("Number literal terminated with an invalid charecter")]
    InvalidNumberLiteral,
    #[error(
        "Invalid token folowing const, const ident, or let expression (not a valid variable name)"
    )]
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
    (chars.peek().unwrap().is_ascii_alphabetic() || *chars.peek().unwrap() == '_')
        && chars.fold(true, |acc, elem| {
            acc && (elem.is_ascii_alphanumeric() || elem == '_')
        })
}

pub fn base_lexer(data: String) -> Result<Vec<BaseToken>, LexError> {
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
                        return Err(LexError::IncompleteCommentExpr);
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
                            Some(sch) => match (sch, escaped) {
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
                            },
                            None => return Err(LexError::IncompleteStringLiteral),
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
                                    Some(_) => return Err(LexError::InvalidNumberLiteral),
                                    None => break,
                                }
                            }
                            // Some(_) => return Err(LexError::InvalidNumberLiteral),
                            Some(_) => break,
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
                ':' => {
                    if let Some(':') = chars.peek() {
                        chars.next();
                        if !current_word.is_empty() {
                            base_tokens.push(BaseToken::Word(current_word));
                            current_word = String::new();
                        }
                        base_tokens.push(BaseToken::Word("::".to_string()))
                    } else {
                        if !current_word.is_empty() {
                            base_tokens.push(BaseToken::Word(current_word));
                            current_word = String::new();
                        }
                        base_tokens.push(BaseToken::Word(ch.to_string()))
                    }
                }
                '=' | '(' | ')' | '{' | '}' | '.' | ',' => {
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
                            '"' | ':' | '=' | '(' | ')' | '{' | '}' | ',' => {
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

pub fn advanced_lexer(tokens: Vec<BaseToken>) -> Result<Vec<Token>, LexError> {
    //? first pass, parse the basic tokens
    let mut token_stream = tokens.into_iter().peekable();
    let mut tokens: Vec<Token> = vec![];

    while let Some(next_token) = token_stream.next() {
        match next_token {
            BaseToken::EOL => tokens.push(Token::EOL),
            BaseToken::NumberLiteral(num) => tokens.push(Token::Literal(Literal::Number(num))),
            BaseToken::StringLiteral(string) => {
                tokens.push(Token::Literal(Literal::String(string)))
            }
            BaseToken::Word(word) => {
                match &*word {
                    "const" => {
                        let t = match token_stream.peek() {
                            Some(BaseToken::Word(word)) if word == "ident" => {
                                let _ = token_stream.next();
                                Token::Keyword(Keyword::ObjectIdent)
                            }
                            _ => Token::Keyword(Keyword::Const),
                        };
                        tokens.push(t);
                    }
                    "let" => tokens.push(Token::Keyword(Keyword::Let)), //TODO intigrate this into the `const` branch
                    "=" => tokens.push(Token::Operator(Operator::Assign)),
                    "." => tokens.push(Token::Operator(Operator::Dot)),
                    ":" => tokens.push(Token::OtherGrammar(OtherGrammar::TypeHint)),
                    "," => tokens.push(Token::OtherGrammar(OtherGrammar::Seperator)),
                    "::" => tokens.push(Token::Operator(Operator::PathSeperator)),
                    "+" | "-" | "*" | "/" | "//" | "%" | "^" | "==" | "===" | "!" | "&&" | "||" | "<" | "<=" | ">" | ">=" => {
                        use Operator::*;
                        tokens.push(Token::Operator(match &*word {
                            "+" => Add,
                            "-" => Sub,
                            "*" => Mul,
                            "/" => Div,
                            "//" => FloorDiv,
                            "%" => Rem,
                            "^" => Pow,
                            "==" => Eq,
                            "===" => StrictEq,
                            "!" => Not,
                            "&&" => And,
                            "||" => Or,
                            "<" => Ltn,
                            "<=" => LtnEq,
                            ">" => Gtn,
                            ">=" => GtnEq,
                            _ => unreachable!()
                        }));
                    }
                    "(" | ")" | "{" | "}" => {
                        tokens.push(match &*word {
                            "(" => Token::ParenBegin,
                            ")" => Token::ParenEnd,
                            "{" => Token::ScopeBegin,
                            "}" => Token::ScopeEnd,
                            _ => unreachable!(),
                        });
                    }
                    "true" => tokens.push(Token::Literal(Literal::Bool(true))),
                    "false" => tokens.push(Token::Literal(Literal::Bool(false))),
                    "if" => tokens.push(Token::Keyword(Keyword::If)),
                    "else" => tokens.push(Token::Keyword(Keyword::Else)),
                    "fn" => tokens.push(Token::Keyword(Keyword::Fn)),
                    "->" => tokens.push(Token::Keyword(Keyword::SmallArrow)),
                    other => tokens.push(Token::Word(other.to_string())),
                }
            }
        }
    }

    //? second pass, parse things like Idents and types and stuff
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
                        return Err(LexError::InavlidVarName);
                    }
                } else {
                    return Err(LexError::InavlidVarName);
                }
            }
            tk @ Token::OtherGrammar(OtherGrammar::TypeHint) | tk @ Token::Keyword(Keyword::SmallArrow) => {
                tokens.push(tk);
                if let Some(Token::Word(type_str)) = token_stream.next() {
                    if qualifies_for_var(&type_str) {
                        tokens.push(Token::Type(type_str));
                    }
                } else {
                    return Err(LexError::InvalidType);
                }
            }
            Token::Word(word) if qualifies_for_var(&word) => tokens.push(Token::Ident(word)),
            a => tokens.push(a),
        }
    }

    //? third pass, currently only used for paths
    let capacity = tokens.len();
    let mut token_stream = tokens.into_iter().peekable();
    let mut tokens: Vec<Token> = Vec::with_capacity(capacity);

    while let Some(next_token) = token_stream.next() {
        match next_token {
            Token::Operator(Operator::PathSeperator) => {
                let mut path: Vec<String> = vec![];
                let mut has_seperator: bool = true;
                loop {
                    match token_stream.peek() {
                        Some(Token::Ident(name)) => {
                            if has_seperator {
                                path.push(name.clone());
                                token_stream.next();
                                has_seperator = false;
                            } else {
                                panic!("invalid path!")
                            }
                        }
                        Some(Token::Operator(Operator::PathSeperator)) => {
                            if has_seperator {
                                panic!("invalid path")
                            } else {
                                has_seperator = true;
                                token_stream.next();
                            }
                        }
                        Some(..) => {
                            if has_seperator {
                                panic!("invalid path")
                            } else {
                                break;
                            }
                        }
                        None => {
                            if has_seperator {
                                panic!("invalid path")
                            } else {
                                break;
                            }
                        }
                    }
                }
                tokens.push(Token::Path(path))
            }
            a => tokens.push(a),
        }
    }

    Ok(tokens)
}

pub fn tokenize(data: String) -> Result<Vec<Token>, LexError> {
    let base_tokens = base_lexer(data)?;
    trace!("base tokens: {base_tokens:#?}");
    let tokens = advanced_lexer(base_tokens)?;
    trace!("tokens: {tokens:#?}");
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
    Fn,
    SmallArrow,
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
    // `::`
    PathSeperator,
    Add,
    Sub,
    Mul,
    Div,
    FloorDiv,
    Rem,
    Pow,
    Eq,
    StrictEq,
    Not,
    And,
    Or,
    Ltn,
    LtnEq,
    Gtn,
    GtnEq,
    // PlusEquals,
    // MinusEquals,
    // MulEquals,
    // DivEquals,
}

impl Operator {
    /// things like mathmatical or logical
    pub fn is_var_op(self) -> bool {
        use Operator::*;
        match self {
            PathSeperator => true,
            Add => true,
            Sub => true,
            Mul => true,
            Div => true,
            FloorDiv => true,
            Rem => true,
            Pow => true,
            Eq => true,
            StrictEq => true,
            Not => true,
            And => true,
            Or => true,
            Ltn => true,
            LtnEq => true,
            Gtn => true,
            GtnEq => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum OtherGrammar {
    /// type hint `:` symbol
    TypeHint,
    /// `,`
    Seperator,
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
    /// a path (::first_thing::second_part)
    Path(Vec<String>),
}
