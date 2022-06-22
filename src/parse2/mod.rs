pub mod lex_rules;
pub mod scoped;
pub mod temp_alloc;
pub mod types;

use crate::lir::ValueExpr;

pub use lex_rules::rules;
use scoped::Tokens;
use temp_alloc::TmpVarAllocator;
use types::{
    ast_types::ASTType,
    functions::{CallArguments, FunctionArguments},
    lexer_tokens::{BaseToken, BaseTokenVal, Op, Token},
};

pub type AstBlock = Vec<AstNode>;
pub type AstGroup = Vec<AstNode>;

#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    Block {
        inner: AstBlock,
    },
    // Group {
    //     inner: AstGroup,
    // },
    FunctionDef {
        name: String,
        args: Vec<FunctionArguments>,
        ret_typ: ASTType,
        /// will always be Some by the time it escapes the build_ast function
        code: Option<AstBlock>,
    },
    FunctionCall {
        name: String,
        args: Vec<CallArguments>,
    },
    Return {
        /// None is the same as typing `return;`, Some is `return <some thing here>`
        value: Option<Box<AstNode>>,
    },
    Break,
    Continue,
    Let {
        ident: String,
        typ: ASTType,
        value: Box<AstNode>,
    },
    /// a = something;
    ///
    /// does not create a binding, only updates it
    Set {
        ident: String,
        value: Box<AstNode>,
    },
    Const {
        ident: String,
        typ: ASTType,
        value: Box<AstNode>,
    },
    ConstIdent {
        ident: String,
        value: String,
    },
    Op {
        left: Box<AstNode>,
        oper: Op,
        right: Box<AstNode>,
    },
    Loop {
        /// will always be Some by the time it escapes the build_ast function
        code: Option<AstBlock>,
    },
    While {
        condition: Box<AstNode>,
        /// will always be Some by the time it escapes the build_ast function
        code: Option<AstBlock>,
    },
    If {
        /// this may or may not be populated, but if it is, then it MUST be included before evaluating `condition`
        /// and can be omitted if `condition` is never evaluated
        condition_code: Option<AstBlock>,
        condition: Box<AstNode>,
        /// will always be Some by the time it escapes the build_ast function
        code: Option<AstBlock>,
    },
    ElseIf {
        /// this may or may not be populated, but if it is, then it MUST be included before evaluating `condition`
        /// and can be omitted if `condition` is never evaluated
        condition_code: Option<AstBlock>,
        condition: Box<AstNode>,
        /// will always be Some by the time it escapes the build_ast function
        code: Option<AstBlock>,
    },
    Else {
        /// will always be Some by the time it escapes the build_ast function
        code: Option<AstBlock>,
    },
    NumLiteral(f64),
    StrLiteral(String),
    BoolLiteral(bool),
    Ident(String),
    //* not emitted directly, but rather through simplification
    SimplifiedGroup {
        inner: AstGroup,
    },
    /// creates a temporary binding
    BindTmp {
        local_id: u64,
        value: Box<AstNode>,
    },
    /// uses a temprary binding
    TmpBinding {
        local_id: u64,
    },
    /// "uses" the value of a node (effectively let _ = <node>)
    ///
    /// emitted when doing things like simplifying bare function calls - a binding is created but never used
    Used(Box<AstNode>),
}

impl AstNode {
    /// simplify the nodes AstNode::Op and AstNode::FunctionCall. does not do any work on other nodes, **even if they contain unsimplified nodes**
    pub fn traverse_simplify_group(
        self,
        output_binding: u64,
        tmp_alloc: &mut TmpVarAllocator,
        output: &mut Vec<AstNode>,
    ) {
        match self {
            AstNode::Op { left, oper, right } => {
                let left = if left.is_simple() {
                    *left
                } else {
                    let local_id = tmp_alloc.next();
                    left.traverse_simplify_group(local_id, tmp_alloc, output);
                    AstNode::TmpBinding { local_id }
                };
                let right = if right.is_simple() {
                    *right
                } else {
                    let local_id = tmp_alloc.next();
                    right.traverse_simplify_group(local_id, tmp_alloc, output);
                    AstNode::TmpBinding { local_id }
                };
                output.push(AstNode::BindTmp {
                    local_id: output_binding,
                    value: Box::new(AstNode::Op {
                        left: Box::new(left),
                        oper,
                        right: Box::new(right),
                    }),
                });
            }
            AstNode::FunctionCall { name, args } => {
                let simplified_args = args
                    .into_iter()
                    .map(|arg| {
                        let raw = arg.args;
                        let simplified = if raw.is_simple() {
                            raw
                        } else {
                            let local_id = tmp_alloc.next();
                            raw.traverse_simplify_group(local_id, tmp_alloc, output);
                            AstNode::TmpBinding { local_id }
                        };
                        CallArguments { args: simplified }
                    })
                    .collect::<Vec<_>>();

                output.push(AstNode::BindTmp {
                    local_id: output_binding,
                    value: Box::new(AstNode::FunctionCall {
                        name,
                        args: simplified_args,
                    }),
                });
            }
            other => {
                output.push(AstNode::BindTmp {
                    local_id: output_binding,
                    value: Box::new(other),
                });
            }
        }
    }

    /// one thing of note: this creates its own temp var allocator, so multiple temporary bindings can appear twice.
    ///
    /// however, thoes binding should not outlive the code produced by this, and therefore it is fine.
    ///
    /// **NOTE TO FUTURE SELF: MAKE TEMPORARY BINDINGS BE NAMED DIFFERENTLY BASED ON SCOPE! (blocks, else statements, etc). OTHERWISE THINGS WILL BREAK!**
    /// ! SECOND NOTE: THIS IS UNTRUE, as the only nesting case is in the traverse_simplify_groups function and that is completely contained
    ///
    /// TODO: **SECOND NOTE TO FUTURE SELF: IN CODEGEN, WRITE A LIFETIME/USE CHECKER FOR THESE TEMPORARY THINGS**
    /// ! this is still a valid point
    ///
    /// also, the bindings are overwritten anyway (before they are read again)
    pub fn simplify(self) -> Vec<AstNode> {
        match self {
            AstNode::Block { inner } => {
                vec![AstNode::Block {
                    inner: inner
                        .into_iter()
                        .flat_map(|n| n.simplify().into_iter())
                        .collect(),
                }]
            }
            AstNode::FunctionDef {
                name,
                args,
                ret_typ,
                code: Some(code),
            } => {
                let mut simple = AstNode::Block { inner: code }.simplify();
                debug_assert!(simple.len() == 1);
                let code = match simple.pop().unwrap() {
                    AstNode::Block { inner } => inner,
                    _ => unreachable!(),
                };
                vec![AstNode::FunctionDef {
                    name,
                    args,
                    ret_typ,
                    code: Some(code),
                }]
            }
            node @ AstNode::FunctionCall { .. } => {
                let mut alloc = TmpVarAllocator::new();
                let return_binding = alloc.next();
                let mut output = vec![];
                node.traverse_simplify_group(return_binding, &mut alloc, &mut output);
                output.push(AstNode::Used(Box::new(AstNode::TmpBinding {
                    local_id: return_binding,
                })));
                output
            }
            AstNode::Return {
                /// None is the same as typing `return;`, Some is `return <some thing here>`
                    value: Some(value),
            } => {
                let mut alloc = TmpVarAllocator::new();
                let return_binding = alloc.next();
                let mut output = vec![];
                value.traverse_simplify_group(return_binding, &mut alloc, &mut output);
                output.push(AstNode::Return {
                    value: Some(Box::new(AstNode::TmpBinding {
                        local_id: return_binding,
                    })),
                });
                output
            }
            AstNode::Let { ident, typ, value } => {
                let mut alloc = TmpVarAllocator::new();
                let return_binding = alloc.next();
                let mut output = vec![];
                value.traverse_simplify_group(return_binding, &mut alloc, &mut output);
                output.push(AstNode::Let {
                    ident,
                    typ,
                    value: Box::new(AstNode::TmpBinding {
                        local_id: return_binding,
                    }),
                });
                output
            }
            AstNode::Set { ident, value } => {
                let mut alloc = TmpVarAllocator::new();
                let return_binding = alloc.next();
                let mut output = vec![];
                value.traverse_simplify_group(return_binding, &mut alloc, &mut output);
                output.push(AstNode::Set {
                    ident,
                    value: Box::new(AstNode::TmpBinding {
                        local_id: return_binding,
                    }),
                });
                output
            }
            AstNode::Const { ident, typ, value } => {
                let mut alloc = TmpVarAllocator::new();
                let return_binding = alloc.next();
                let mut output = vec![];
                value.traverse_simplify_group(return_binding, &mut alloc, &mut output);
                output.push(AstNode::Const {
                    ident,
                    typ,
                    value: Box::new(AstNode::TmpBinding {
                        local_id: return_binding,
                    }),
                });
                output
            }
            node @ AstNode::Op { .. } => {
                let mut alloc = TmpVarAllocator::new();
                let return_binding = alloc.next();
                let mut output = vec![];
                node.traverse_simplify_group(return_binding, &mut alloc, &mut output);
                output
            }
            AstNode::Loop { code: Some(code) } => {
                vec![AstNode::Loop {
                    code: Some(
                        code.into_iter()
                            .flat_map(|n| n.simplify().into_iter())
                            .collect(),
                    ),
                }]
            }
            AstNode::While {
                condition,
                code: Some(code),
            } => {
                //condition simplification
                let mut alloc = TmpVarAllocator::new();
                let return_binding = alloc.next();
                let mut output = vec![];
                condition.traverse_simplify_group(return_binding, &mut alloc, &mut output);
                let condition = Box::new(AstNode::TmpBinding {
                    local_id: return_binding,
                });
                //code simplification
                let code = code
                    .into_iter()
                    .flat_map(|n| n.simplify().into_iter())
                    .collect();
                //output
                output.push(AstNode::While {
                    condition,
                    code: Some(code),
                });
                output
            }
            AstNode::If {
                condition_code: None,
                condition,
                code: Some(code),
            } => {
                //condition simplification
                let mut alloc = TmpVarAllocator::new();
                let return_binding = alloc.next();
                let mut output = vec![];
                condition.traverse_simplify_group(return_binding, &mut alloc, &mut output);
                let condition = Box::new(AstNode::TmpBinding {
                    local_id: return_binding,
                });
                //code simplification
                let code = code
                    .into_iter()
                    .flat_map(|n| n.simplify().into_iter())
                    .collect();
                //output
                vec![AstNode::If {
                    condition_code: Some(output),
                    condition,
                    code: Some(code),
                }]
            }
            AstNode::ElseIf {
                condition_code: None,
                condition,
                code: Some(code),
            } => {
                //condition simplification
                let mut alloc = TmpVarAllocator::new();
                let return_binding = alloc.next();
                let mut output = vec![];
                condition.traverse_simplify_group(return_binding, &mut alloc, &mut output);
                let condition = Box::new(AstNode::TmpBinding {
                    local_id: return_binding,
                });
                //code simplification
                let code = code
                    .into_iter()
                    .flat_map(|n| n.simplify().into_iter())
                    .collect();
                //output
                vec![AstNode::ElseIf {
                    condition_code: Some(output),
                    condition,
                    code: Some(code),
                }]
            }
            AstNode::Else { code: Some(code) } => {
                vec![AstNode::Else {
                    code: Some(
                        code.into_iter()
                            .flat_map(|n| n.simplify().into_iter())
                            .collect(),
                    ),
                }]
            }
            tk @ (AstNode::NumLiteral(..)
            | AstNode::StrLiteral(..)
            | AstNode::BoolLiteral(..)
            | AstNode::Ident(..)
            | AstNode::Break
            | AstNode::Continue
            | AstNode::Return { value: None }
            | AstNode::ConstIdent { .. }) => vec![tk],
            AstNode::SimplifiedGroup { .. }
            | AstNode::BindTmp { .. }
            | AstNode::TmpBinding { .. } => panic!("cannot simplify a simplification symbol"),
            _ => unreachable!(),
        }
    }

    pub fn is_simple(&self) -> bool {
        match self {
            AstNode::NumLiteral(..) |
            AstNode::StrLiteral(..) |
            AstNode::BoolLiteral(..) |
            // AstNode::FunctionCall { .. } |
            AstNode::Ident(..) => {
                true
            }
            _ => false
        }
    }

    /// is the node a value expr (has a value that it directly returns)
    pub fn is_value_expr(&self) -> bool {
        ValueExpr::ast_node_is_value_expr(self)
    }

    pub fn is_math(&self) -> bool {
        if let AstNode::Op { .. } = self {
            true
        } else {
            false
        }
    }

    fn needs_trailing_block(&self) -> bool {
        if let AstNode::FunctionDef { .. }
        | AstNode::Loop { .. }
        | AstNode::While { .. }
        | AstNode::If { .. }
        | AstNode::Else { .. }
        | AstNode::ElseIf { .. } = self
        {
            true
        } else {
            false
        }
    }

    fn accept_trailing_block(&mut self, block: AstBlock) {
        match self {
            AstNode::FunctionDef { code, .. } => {
                *code = Some(block);
            }
            AstNode::Loop { code } => {
                *code = Some(block);
            }
            AstNode::While { code, .. } => {
                *code = Some(block);
            }
            AstNode::If { code, .. } => {
                *code = Some(block);
            }
            AstNode::Else { code } => {
                *code = Some(block);
            }
            AstNode::ElseIf { code, .. } => {
                *code = Some(block);
            }
            _ => unreachable!(
                "called accept_trailing_block on a node that does not accept trailing blocks!"
            ),
        }
    }

    pub fn parse(tk: Tokens) -> Result<AstNode, BuildAstError> {
        //TODO panics -> errors
        match tk {
            Tokens::Block(inner) => {
                let mut block = AstBlock::new();

                let mut needing_block = None;

                for tokens in inner {
                    match tokens {
                        expr @ Tokens::Expr(..) => {
                            let node = AstNode::parse(expr)?;
                            if node.needs_trailing_block() {
                                needing_block = Some(node);
                            } else {
                                block.push(node);
                            }
                        }
                        blk @ Tokens::Block(..) => {
                            if let Some(mut node) = needing_block {
                                let inner = if let AstNode::Block {
                                    inner: parsed_blk_inner,
                                } = AstNode::parse(blk)?
                                {
                                    parsed_blk_inner
                                } else {
                                    unreachable!()
                                };
                                node.accept_trailing_block(inner);
                                block.push(node);
                                needing_block = None;
                            }
                        }
                        _ => {
                            unreachable!("block cannot congain a group or token")
                        }
                    }
                }
                Ok(AstNode::Block { inner: block })
            }
            Tokens::Group(mut inner) => {
                Tokens::simplify_inner(&mut inner);
                match inner.len() {
                    0 => panic!("empty group expr!"),
                    1 => match inner.pop().unwrap() {
                        Tokens::Block(..) => panic!("Blocks may not exist within a group"),
                        gr @ Tokens::Group(..) => AstNode::parse(gr),
                        Tokens::Expr(..) => unreachable!("probably, please tell me if the happens"),
                        Tokens::Token(token) => match token.val {
                            BaseTokenVal::NumLiteral(num) => Ok(AstNode::NumLiteral(num)),
                            BaseTokenVal::StrLiteral(s) => Ok(AstNode::StrLiteral(s)),
                            BaseTokenVal::Token(Token::BoolLiteral(b)) => {
                                Ok(AstNode::BoolLiteral(b))
                            }
                            BaseTokenVal::Ident(id) => Ok(AstNode::Ident(id)),
                            BaseTokenVal::Token(tk) => {
                                panic!("unparsed single token in group: {tk:?}")
                            }
                            BaseTokenVal::Semi => unreachable!(),
                        },
                    },
                    _ => {
                        match &inner[..] {
                            //* function calls (duplicate exists in the Tokens::Expr branch, EDIT BOLTH IF ONE CHANGES)
                            [Tokens::Token(BaseToken {
                                val: BaseTokenVal::Ident(name),
                                ..
                            }), Tokens::Group(raw_args)] => {
                                let args = CallArguments::parse(raw_args)?;
                                Ok(AstNode::FunctionCall {
                                    name: name.clone(),
                                    args,
                                })
                            }
                            //* new math things
                            tks if tks
                                .iter()
                                .filter(|e| {
                                    if let Tokens::Token(BaseToken {
                                        val: BaseTokenVal::Token(Token::Op(..)),
                                        ..
                                    }) = e
                                    {
                                        true
                                    } else {
                                        false
                                    }
                                })
                                .count()
                                >= 1 =>
                            {
                                if tks
                                    .iter()
                                    .filter(|e| {
                                        if let Tokens::Token(BaseToken {
                                            val: BaseTokenVal::Token(Token::Op(..)),
                                            ..
                                        }) = e
                                        {
                                            true
                                        } else {
                                            false
                                        }
                                    })
                                    .count()
                                    > 1
                                {
                                    panic!("Complex mathmatical expressions are not currently supported (one operator per group)");
                                } else {
                                    let mut parts = tks
                                        .split_inclusive(|e| match e {
                                            Tokens::Token(BaseToken {
                                                val: BaseTokenVal::Token(Token::Op(..)),
                                                ..
                                            }) => true,
                                            _ => false,
                                        })
                                        .map(|x| x.to_vec())
                                        .collect::<Vec<_>>();
                                    debug_assert!(parts.len() == 2);
                                    let right = parts.pop().unwrap();
                                    let mut left = parts.pop().unwrap();
                                    let op = if let Some(Tokens::Token(BaseToken {
                                        val: BaseTokenVal::Token(Token::Op(op)),
                                        ..
                                    })) = left.pop()
                                    {
                                        op
                                    } else {
                                        unreachable!();
                                    };
                                    Ok(AstNode::Op {
                                        left: Box::new(AstNode::parse(Tokens::Group(left))?),
                                        oper: op,
                                        right: Box::new(AstNode::parse(Tokens::Group(right))?),
                                    })
                                }
                            }
                            _ => todo!("unhandled group expression {inner:?}"),
                        }
                    }
                }
            }
            Tokens::Expr(inner) => match &inner[..] {
                //* function definitions
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::Fn),
                    ..
                }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Ident(name),
                    ..
                }), Tokens::Group(raw_args), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::SmallArrow),
                    ..
                }), Tokens::Token(BaseToken { val: ret_type, .. })]
                    if {
                        FunctionArguments::parse(raw_args).is_some()
                            && ASTType::try_from_token(ret_type.clone()).is_some()
                    } =>
                {
                    Ok(AstNode::FunctionDef {
                        name: name.clone(),
                        args: FunctionArguments::parse(raw_args).unwrap(),
                        ret_typ: ASTType::try_from_token(ret_type.clone()).unwrap(),
                        code: None,
                    })
                }
                //* function calls (duplicate exists in the Tokens::Group branch, EDIT BOLTH IF ONE CHANGES)
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Ident(name),
                    ..
                }), Tokens::Group(raw_args)] => {
                    let args = CallArguments::parse(raw_args)?;
                    Ok(AstNode::FunctionCall {
                        name: name.clone(),
                        args,
                    })
                }
                //* return statement (no value)
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::Return),
                    ..
                })] => Ok(AstNode::Return { value: None }),
                //* break statement
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::Break),
                    ..
                })] => Ok(AstNode::Break),
                //* continue statement
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::Continue),
                    ..
                })] => Ok(AstNode::Continue),
                //* return statement (with value)
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::Return),
                    ..
                }), rest @ ..] => Ok(AstNode::Return {
                    value: Some(Box::new(AstNode::parse(Tokens::Group(rest.to_vec()))?)),
                }),
                //* let statement
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::Let),
                    ..
                }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Ident(ident),
                    ..
                }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::TypeHint),
                    ..
                }), Tokens::Token(BaseToken { val: raw_typ, .. }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::OpAssign),
                    ..
                }), rest @ ..]
                    if ASTType::try_from_token(raw_typ.clone()).is_some() =>
                {
                    Ok(AstNode::Let {
                        ident: ident.clone(),
                        typ: ASTType::try_from_token(raw_typ.clone()).unwrap(),
                        value: Box::new(AstNode::parse(Tokens::Group(rest.to_vec()))?),
                    })
                }
                //* let statement (with no type hinting, for easier error correcting)
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::Let),
                    loc,
                }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Ident(..),
                    ..
                }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::OpAssign),
                    ..
                }), ..] => {
                    panic!("Error @ {loc:?} Let statements must have types");
                }
                //* set statement
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Ident(binding),
                    ..
                }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::OpAssign),
                    ..
                }), rest @ ..] => Ok(AstNode::Set {
                    ident: binding.clone(),
                    value: Box::new(AstNode::parse(Tokens::Group(rest.to_vec()))?),
                }),
                //* const statement
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::Const),
                    ..
                }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Ident(ident),
                    ..
                }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::TypeHint),
                    ..
                }), Tokens::Token(BaseToken { val: raw_typ, .. }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::OpAssign),
                    ..
                }), rest @ ..]
                    if ASTType::try_from_token(raw_typ.clone()).is_some() =>
                {
                    Ok(AstNode::Const {
                        ident: ident.clone(),
                        typ: ASTType::try_from_token(raw_typ.clone()).unwrap(),
                        value: Box::new(AstNode::parse(Tokens::Group(rest.to_vec()))?),
                    })
                }
                //* const ident statement
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::Const),
                    ..
                }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::ConstIdent),
                    ..
                }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Ident(ident),
                    ..
                }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::OpAssign),
                    ..
                }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::StrLiteral(value),
                    ..
                })] => Ok(AstNode::ConstIdent {
                    ident: ident.clone(),
                    value: value.clone(),
                }),
                //* loop statement
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::Loop),
                    ..
                })] => Ok(AstNode::Loop { code: None }),
                //* while statement
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::While),
                    ..
                }), condition_tks @ ..] => Ok(AstNode::While {
                    condition: Box::new(AstNode::parse(Tokens::Group(condition_tks.to_vec()))?),
                    code: None,
                }),
                //* if statement
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::If),
                    ..
                }), condition_tks @ ..] => Ok(AstNode::If {
                    condition_code: None,
                    condition: Box::new(AstNode::parse(Tokens::Group(condition_tks.to_vec()))?),
                    code: None,
                }),
                //* else statement
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::Else),
                    ..
                })] => Ok(AstNode::Else { code: None }),
                //* else if statement
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::Else),
                    ..
                }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::If),
                    ..
                }), condition_tks @ ..] => Ok(AstNode::ElseIf {
                    condition_code: None,
                    condition: Box::new(AstNode::parse(Tokens::Group(condition_tks.to_vec()))?),
                    code: None,
                }),
                //* catch all
                unmatched => {
                    todo!("unmatched expr:\n{unmatched:#?}")
                }
            },
            Tokens::Token(..) => {
                unreachable!("AstNode::parse may not be called with a value of Tokens::Token")
            }
        }
    }
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum BuildAstError {}
