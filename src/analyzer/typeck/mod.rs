use std::collections::HashMap;

use crate::{
    analyzer::Program,
    parse2::{
        types::{functions::CallArguments, lexer_tokens::Op},
        AstNode,
    },
    typing::{
        functions::{FunctionDecl, FunctionRegistry},
        types::{BuiltinTypeKind, TypeId, TypeKind, TypeRegistry},
    },
};

macro_rules! typek {
    (num) => {
        TypeKind::Builtin(BuiltinTypeKind::Num)
    };
    (str) => {
        TypeKind::Builtin(BuiltinTypeKind::Str)
    };
    (bool) => {
        TypeKind::Builtin(BuiltinTypeKind::Bool)
    };
    (null) => {
        TypeKind::Builtin(BuiltinTypeKind::Null)
    };
    (never) => {
        TypeKind::Builtin(BuiltinTypeKind::Never)
    };
    (mobj) => {
        TypeKind::Builtin(BuiltinTypeKind::MlogObject)
    };
}

macro_rules! math_ops {
    () => {
        Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Mod
    };
}

macro_rules! comparison_ops {
    () => {
        Op::Eq | Op::Ltn | Op::Gtn | Op::LtnEq | Op::GtnEq
    };
}

macro_rules! logical_ops {
    () => {
        Op::And | Op::Or
    };
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Global {
    ty: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Local {
    ty: TypeId,
}

pub fn perform(program: &Program) {
    let mut types = TypeRegistry::new();
    types.fillin_builtins();
    let types = types;

    let mut fns = FunctionRegistry::new();
    for (name, raw) in &program.raw_functions {
        let decl = FunctionDecl::try_from(raw.clone()).unwrap();
        for (param_name, ty) in &decl.arguments {
            assert!(types.id_of_type(ty).is_some(), "Function `{name}` includes a argument `{param_name}` of type `{ty:?}`, but that type does not exist!");
        }
        assert!(
            types.id_of_type(&decl.return_t).is_some(),
            "Function `{name}` has the return type of `{:?}`, but that type does not exist!",
            decl.return_t
        );

        fns.register(name.clone(), decl);
    }
    let fns = fns;

    let mut globals: HashMap<String, Global> = HashMap::new();
    for (name, super::visitors::constants::Constant { typ, value }) in &program.global_const {
        let expected = TypeKind::try_from(typ.clone()).unwrap();
        assert!(types.id_of_type(&expected).is_some(), "Global constant `{name}` has an explicit type of `{expected:?}`, but that type does not exist!");
        let found = type_of(value, &fns, &HashMap::new(), &HashMap::new(), &types);
        assert_eq!(
            expected, found,
            "Type of global was different than labeled! (expected {expected:?}, found {found:?}"
        );
        assert!(
            globals
                .insert(
                    name.clone(),
                    Global {
                        ty: *types.id_of_type(&found).unwrap()
                    }
                )
                .is_none(),
            "global {name} defined twice!"
        );
    }
    let globals = globals;

    for (fn_name, raw) in &program.raw_functions {
        let mut locals: HashMap<String, Local> = HashMap::new();
        let decl = FunctionDecl::try_from(raw.clone()).unwrap();
        let expected_return_t = types.id_of_type(&decl.return_t).unwrap();
        for (name, typ) in decl.arguments {
            locals.insert(
                name,
                Local {
                    ty: *types.id_of_type(&typ).unwrap(),
                },
            );
        }
        traverse_check_section(
            fn_name,
            &types,
            &fns,
            &globals,
            raw.code.clone(),
            &mut locals,
            *expected_return_t,
        )
    }
}

fn traverse_check_section(
    // for debugging ONLY
    fn_name: &str,
    type_reg: &TypeRegistry,
    fn_reg: &FunctionRegistry,
    globals: &HashMap<String, Global>,
    code: Vec<AstNode>,
    locals: &mut HashMap<String, Local>,
    expected_return_t: TypeId,
) {
    for next in code {
        match next {
            AstNode::Const { .. } |
            AstNode::ConstIdent { .. } |
            AstNode::FunctionDef { .. } => unreachable!("code cannot contain constant or function definitions!"),
            AstNode::BindTmp { .. } | AstNode::TmpBinding { .. } | AstNode::SimplifiedGroup { .. } | AstNode::Used(..) => unreachable!("traverse_check_section must be used before simplification (encountered a temporary binding node)"),
            AstNode::NumLiteral(..) | AstNode::StrLiteral(..) | AstNode::BoolLiteral(..) | AstNode::Ident(..) => panic!("Found bare literal/ident value (meaningless, and also invalid)"),
            AstNode::Op { .. } => panic!("Found bare operation (meaningless, and also invalid)"),
            AstNode::Let { ident, typ, value } => {
                let expected_kind = TypeKind::try_from(typ).unwrap();
                let expected = *type_reg.id_of_type(&expected_kind)
                    .expect(&format!("Let statement for variable `{ident}` has an explicit type of `{expected_kind:?}`, but that type does not exist!"));
                let found_kind = type_of(&value, fn_reg, locals, globals, type_reg);
                let found = *type_reg.id_of_type(&found_kind)
                    .expect(&format!("Let statement for variable `{ident}` has an elided type of `{found_kind:?}`, but that type does not exist!"));
                assert_eq!(expected, found, "Type of let statement for variable `{ident}` was different than labeled! (elided: `{found_kind:?}`, provided: `{expected_kind:?}`)");
                let _ = locals.insert(ident, Local { ty: found });
            }
            AstNode::Set { ident, value } => {
                let found_kind = type_of(&value, fn_reg, locals, globals, type_reg);
                let found = *type_reg.id_of_type(&found_kind)
                    .expect(&format!("Set statement for variable `{ident}` has an elided type of `{found_kind:?}`, but that type does not exist!"));
                let pre_existing = locals.get(&ident)
                    .expect(&format!("Attempted to set a local varaible that does not exist (name: `{ident}`)"));
                let pre_existing_typekind = type_reg.type_of_id(&pre_existing.ty).unwrap();
                assert_eq!(pre_existing.ty, found, "Attempted to assign a value of type `{found_kind:?}` to a the variable `{ident}` with the type `{pre_existing_typekind:?}`");
            }
            AstNode::Block { .. } => unreachable!("Blocks should not exist at this point...???"),
            AstNode::Break | AstNode::Continue | AstNode::Return { value: None } => {/* nothing to do */}
            AstNode::Return { value: Some(value) } => {
                let found_kind = type_of(&value, fn_reg, locals, globals, type_reg);
                let found = *type_reg.id_of_type(&found_kind)
                    .expect(&format!("Return statement has an elided type of `{found_kind:?}`, but that type does not exist!"));
                assert_eq!(expected_return_t, found, "Attempted to return a value of type `{found_kind:?}` from function `{fn_name}`, but `{fn_name}` has a return type of `{:?}`", type_reg.type_of_id(&expected_return_t).unwrap())
            }
            AstNode::FunctionCall { name, args } => {
                let decl = fn_reg.get_def(&name)
                    .expect(&format!("Attempted to call fn `{name}`, but it is not defined!"));
                if !decl.no_validation {
                    assert_eq!(decl.arguments.len(), args.len(), "Mismatched argument count while calling fn `{name}`. expected {}, found {}", decl.arguments.len(), args.len());
                    for ((decl_arg_name, decl_arg_t), CallArguments { args: value }) in decl.arguments.iter().zip(args.into_iter()) {
                        let found_kind = type_of(&value, fn_reg, locals, globals, type_reg);
                        let found = *type_reg.id_of_type(&found_kind)
                            .expect(&format!("Function call argument (fn `{name}`, arg `{decl_arg_name}`) has an elided type of `{found_kind:?}`, but that type does not exist!"));
                        assert_eq!(*type_reg.id_of_type(decl_arg_t).unwrap(), found, "Argument passed to function (fn `{name}`, arg `{decl_arg_name}`) has a type of `{found_kind:?}`, but the definition has a type of `{decl_arg_t:?}`");
                    }
                }
            }
            AstNode::Loop { code } | AstNode::Else { code }=> {
                let code = code.unwrap();
                traverse_check_section(fn_name, type_reg, fn_reg, globals, code, locals, expected_return_t);
            }
            AstNode::While { condition_code, condition, code }
            | AstNode::If { condition_code, condition, code }
            | AstNode::ElseIf { condition_code, condition, code } => {
                assert!(condition_code.is_none(), "Simplification must occur AFTER typeck");
                let found_kind = type_of(&condition, fn_reg, locals, globals, type_reg);
                let found = *type_reg.id_of_type(&found_kind)
                    .expect(&format!("Conditional statement condition has an elided type of `{found_kind:?}`, but that type does not exist!"));
                assert_eq!(*type_reg.type_of_id(&found).unwrap(), typek!(bool), "Condition does not evaluate to a bool value! (found `{found_kind:?}`)");
                let code = code.unwrap();
                traverse_check_section(fn_name, type_reg, fn_reg, globals, code, locals, expected_return_t);
            }
        }
    }
}

fn type_of(
    node: &AstNode,
    fns: &FunctionRegistry,
    locals: &HashMap<String, Local>,
    globals: &HashMap<String, Global>,
    type_registry: &TypeRegistry,
) -> TypeKind {
    let t = match node {
        AstNode::Block { .. }
        | AstNode::If { .. }
        | AstNode::ElseIf { .. }
        | AstNode::Else { .. }
        | AstNode::Const { .. }
        | AstNode::ConstIdent { .. }
        | AstNode::FunctionDef { .. }
        | AstNode::Let { .. }
        | AstNode::Loop { .. }
        | AstNode::While { .. }
        | AstNode::Set { .. } => unreachable!("Node does not have a type!"),
        AstNode::BindTmp { .. }
        | AstNode::TmpBinding { .. }
        | AstNode::SimplifiedGroup { .. }
        | AstNode::Used(..) => unreachable!(
            "type_of must be used before simplification (encountered a temporary binding node)"
        ),
        AstNode::FunctionCall { name, args: _ } => fns
            .get_def(name)
            .expect(&format!("Function {name} was called, but never defined!"))
            .return_t
            .clone(),
        AstNode::Return { .. } | AstNode::Break | AstNode::Continue => typek!(never),
        AstNode::NumLiteral(..) => typek!(num),
        AstNode::StrLiteral(..) => typek!(str),
        AstNode::BoolLiteral(..) => typek!(bool),
        AstNode::Ident(id) => type_registry
            .type_of_id(&match (locals.get(id), globals.get(id)) {
                (Some(local), None) => local.ty.clone(),
                (None, Some(global)) => global.ty.clone(),
                (Some(..), Some(..)) => {
                    panic!("Variable {id} was defined as a global and as a local!")
                }
                (None, None) => panic!("Variable {id} was used before it was defined!"),
            })
            .unwrap()
            .clone(),
        AstNode::Op { left, oper, right } => {
            match (
                type_of(left, fns, locals, globals, type_registry),
                oper,
                type_of(right, fns, locals, globals, type_registry),
            ) {
                (typek!(num), math_ops!(), typek!(num)) => typek!(num),
                (typek!(num), comparison_ops!(), typek!(num)) => typek!(bool),
                (typek!(bool), logical_ops!() | Op::Eq, typek!(bool)) => typek!(bool),
                (typek!(str), Op::Eq, typek!(str)) => typek!(bool),
                (left, op, right) => panic!("Invalid operation {left:?} {op:?} {right:?}"),
            }
        }
    };
    assert!(
        type_registry.id_of_type(&t).is_some(),
        "Expr has an elided type of `{t:?}`, but that type does not exist!"
    );
    t
}
