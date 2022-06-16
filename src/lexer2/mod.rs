use std::fmt::Debug;

use regex::Regex;

#[allow(unused)]
fn ident_regex() -> Result<Regex, regex::Error> {
    Regex::new(r"^[_a-zA-Z][_a-zA-Z0-9]*$")
}

#[test]
fn validate_ident_regex() {
    let ident_re = ident_regex().unwrap();
    assert_eq!(true, ident_re.is_match("_"));
    assert_eq!(true, ident_re.is_match("_TesT12"));
    assert_eq!(true, ident_re.is_match("aseE"));
    assert_eq!(true, ident_re.is_match("Easd13"));
    assert_eq!(true, ident_re.is_match("ase_"));
    assert_eq!(true, ident_re.is_match("_ase_12"));
    assert_eq!(false, ident_re.is_match("9asdf"));
    assert_eq!(false, ident_re.is_match(" _TesT12"));
    assert_eq!(false, ident_re.is_match("_TesT12 "));
}

#[derive(Debug, Clone, PartialEq)]
pub enum BaseTokenVal<T: Debug + Clone + PartialEq> {
    NumLiteral(f64),
    StrLiteral(String),
    Semi,
    Token(T),
    Ident(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CodeLoc {
    pub line: u32,
    pub ch: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BaseToken<T: Debug + Clone + PartialEq> {
    pub val: BaseTokenVal<T>,
    pub loc: CodeLoc,
}

#[derive(Clone, Debug, thiserror::Error)]
pub enum LexErrorKind {
    #[error("EOF while parsing string literal")]
    IncompleteStringLiteral,
    #[error("Number literal terminated with an invalid charecter")]
    InvalidNumberLiteral,
    #[error("Invalid ident")]
    InvalidIdent,
    #[error("Reached unexpected input")]
    InvalidInput,
}

#[derive(Clone, Debug, thiserror::Error)]
#[error("Lexing error at {loc:?}: {kind:#?}")]
pub struct LexError {
    #[source]
    kind: LexErrorKind,
    loc: CodeLoc,
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum FindStrLitErr {
    #[error("EOF while parsing string literal")]
    IncompleteStringLiteral,
}

/// NOTE: SRC must be a the begining of a string literal, AFTER the first `"`
fn first_string_literal(
    src: &mut String,
    mut loc: CodeLoc,
) -> (CodeLoc, Result<String, FindStrLitErr>) {
    let mut string_literal: String = String::new();
    let mut escaped = false;
    loop {
        match borrow_n(src, 1) {
            Some("\\") => {
                loc.ch += 1;
                if escaped {
                    // it is being escaped, so put it in as a literal value reset escaped
                    string_literal.push('\\');
                    escaped = false;
                } else {
                    // not being escaped, so set escaped to true
                    escaped = true
                }
            }
            Some("\"") => {
                loc.ch += 1;
                if escaped {
                    // odd number of \ so it IS being escaped
                    string_literal.push('"');
                    escaped = false;
                } else {
                    break (loc, Ok(string_literal));
                }
            }
            Some(sch) => match (sch, escaped) {
                ("n", true) => {
                    loc.ch += 1;
                    string_literal.push('\n');
                    escaped = false;
                }
                ("t", true) => {
                    loc.ch += 1;
                    string_literal.push('\t');
                    escaped = false;
                }
                ("\n", true) => {
                    warn!("unhandled/unnecessary escape value! (if you want more escape sequences, please open an issue). {loc:?}");
                    loc.line += 1;
                    loc.ch = 0;
                    escaped = false;
                    string_literal.push_str("\n");
                }
                (other, true) => {
                    warn!("unhandled/unnecessary escape value! (if you want more escape sequences, please open an issue). {loc:?}");
                    loc.ch += 1;
                    escaped = false;
                    string_literal.push_str(other);
                }
                ("\n", false) => {
                    loc.line += 1;
                    loc.ch = 0;
                    string_literal.push_str("\n");
                }
                (other, false) => {
                    string_literal.push_str(other);
                }
            },
            None => break (loc, Err(FindStrLitErr::IncompleteStringLiteral)),
        }
    }
}

/// self contains [at, len), returned contains [0, at)
#[inline(always)]
#[must_use]
fn take_n(input: &mut String, n: usize) -> Option<String> {
    if n >= input.len() {
        return None;
    }
    let mut new_input = input.split_off(n);
    std::mem::swap(&mut new_input, input);
    Some(new_input)
}

#[inline(always)]
#[must_use]
fn borrow_n(input: &str, n: usize) -> Option<&str> {
    if n >= input.len() {
        return None;
    }
    Some(&input[..n])
}

#[derive(Clone, Debug)]
pub struct Rule<T: Debug + Clone + PartialEq + 'static> {
    /// token to emit
    pub token: Option<T>,
    /// validating regex
    pub validator: Regex,
    /// length of the pattern this matches (fixed-length, sorry)
    pub length: usize,
}

#[must_use]
pub fn base_lex<T: Debug + Clone + PartialEq + 'static>(
    mut data: String,
    rules: &[Rule<T>],
) -> Result<Vec<BaseToken<T>>, LexError> {
    let ident_re = ident_regex().unwrap();

    let mut tokens: Vec<BaseToken<T>> = vec![];
    // line, char
    let mut loc = CodeLoc { line: 1, ch: 0 };

    'lex: loop {
        // info!("tokens: {tokens:?}");
        match borrow_n(&data, 2) {
            Some("//") => {
                // debug!("parsing comment");
                let _ = take_n(&mut data, 2);
                'comment: loop {
                    match take_n(&mut data, 1).map(|s| s.chars().next().unwrap()) {
                        Some('\n') => break 'comment,
                        Some(..) => {}
                        None => break 'lex,
                    }
                }
                // debug!("done with comment");
                loc.line += 1;
                loc.ch = 0;
                continue 'lex;
            }
            Some(..) => {}
            None => {}
        }
        match borrow_n(&data, 1) {
            Some(" ") | Some("\t") | Some("\n") => {
                // debug!("whitespace");
                match borrow_n(&data, 1) {
                    Some(" ") | Some("\t") => {
                        loc.ch += 1;
                    }
                    Some("\n") => {
                        loc.line += 1;
                        loc.ch = 0;
                    }
                    _ => unreachable!(),
                }
                let _ = take_n(&mut data, 1);
                continue 'lex;
            }
            Some(";") => {
                let _ = take_n(&mut data, 1);
                // debug!("semicolon");
                tokens.push(BaseToken {
                    val: BaseTokenVal::Semi,
                    loc,
                });
                loc.ch += 1;
                continue 'lex;
            }
            Some("\"") => {
                let _ = take_n(&mut data, 1);
                let (new_loc, lit) = match first_string_literal(&mut data, loc) {
                    (new_loc, Ok(lit)) => (new_loc, lit),
                    (new_loc, Err(e)) => match e {
                        FindStrLitErr::IncompleteStringLiteral => {
                            return Err(LexError {
                                kind: LexErrorKind::IncompleteStringLiteral,
                                loc: new_loc,
                            })
                        }
                    },
                };
                tokens.push(BaseToken {
                    val: BaseTokenVal::StrLiteral(lit),
                    loc,
                });
                loc = new_loc;
                continue 'lex;
            }
            Some(num) if num.chars().next().unwrap().is_digit(10) => {
                // trace!("parsing numeric literal");
                let mut digits: String = String::from(num);
                let _ = take_n(&mut data, 1);
                loc.ch += 1;
                loop {
                    match borrow_n(&data, 1) {
                        Some(num) if num.chars().next().unwrap().is_digit(10) => {
                            loc.ch += 1;
                            digits.push_str(&take_n(&mut data, 1).unwrap());
                        }
                        Some(".") => {
                            loc.ch += 1;
                            digits.push_str(&take_n(&mut data, 1).unwrap());
                            match borrow_n(&data, 1) {
                                Some(num) if num.chars().next().unwrap().is_digit(10) => {}
                                Some(_) => {
                                    return Err(LexError {
                                        kind: LexErrorKind::InvalidNumberLiteral,
                                        loc,
                                    })
                                }
                                None => break,
                            }
                        }
                        Some(_) => break,
                        None => break,
                    }
                }
                let number = digits.parse::<f64>().unwrap();
                tokens.push(BaseToken {
                    val: BaseTokenVal::NumLiteral(number),
                    loc,
                });
                continue 'lex;
            }
            Some(..) => {}
            None => {
                break 'lex;
            }
        }

        for rule in rules {
            if let Some(text) = borrow_n(&data, rule.length) {
                let newlines = u32::try_from(text.chars().filter(|&c| c == '\n').count()).unwrap();
                let chars = u32::try_from(
                    text.split('\n')
                        .last()
                        .map(|l| l.len())
                        .unwrap_or(text.len()),
                )
                .unwrap();

                if rule.validator.is_match(text) {
                    // debug!("rule matches: {:#?}", rule);
                    let _ = take_n(&mut data, rule.length);
                    if let Some(tk) = &rule.token {
                        tokens.push(BaseToken {
                            val: BaseTokenVal::Token(tk.clone()),
                            loc,
                        });
                    }
                    if newlines == 0 {
                        loc.ch += chars;
                    } else {
                        loc.line += newlines;
                        loc.ch = chars;
                    }
                    continue 'lex;
                }
            }
        }
        // warn!("no rule applies! exiting...\ncurrent text:\n{}", data);
        // debug!("no rule applies, attempting to parse an ident");

        let mut i = 1;
        let mut ident = String::new();
        'ident: loop {
            match borrow_n(&data, i) {
                Some(txt) => {
                    if ident_re.is_match(txt) {
                        // info!("match\"{txt}\"");
                        i += 1;
                    } else {
                        i -= 1;
                        ident = take_n(&mut data, i).unwrap();
                        // info!("ident parsing done, ident: \"{ident}\"");
                        break 'ident;
                    }
                }
                None => {
                    break 'ident;
                }
            }
        }
        if ident.is_empty() {
            warn!("no rules (including ident) applies, exiting...\ncurrent text:\n{data:#?}");
            return Err(LexError {
                kind: LexErrorKind::InvalidInput,
                loc,
            });
        } else {
            tokens.push(BaseToken {
                val: BaseTokenVal::Ident(ident),
                loc,
            });
            loc.ch += u32::try_from(i).unwrap();
            continue 'lex;
        }
    }
    // debug!("done");
    Ok(tokens)
}
