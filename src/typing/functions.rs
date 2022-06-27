use std::collections::HashMap;

use crate::{analyzer::visitors::raw_functions::RawFn, typing::types::{TypeKind, BuiltinTypeKind}};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDecl {
    // do not validate args
    pub no_validation: bool,
    // (name (for better errors), type)
    pub arguments: Vec<(String, TypeKind)>,
    pub return_t: TypeKind,
}

impl TryFrom<RawFn> for FunctionDecl {
    type Error = &'static str;
    fn try_from(raw: RawFn) -> Result<Self, Self::Error> {
        Ok(Self {
            no_validation: false,
            arguments: raw
                .args
                .into_iter()
                .map::<Result<(String, TypeKind), &'static str>, _>(|arg| {
                    Ok((arg.ident, TypeKind::try_from(arg.typ)?))
                })
                .collect::<Result<_, _>>()?,
            return_t: TypeKind::try_from(raw.ret_typ)?,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct FunctionRegistry {
    map: HashMap<String, FunctionDecl>,
}

impl FunctionRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register(&mut self, name: String, decl: FunctionDecl) {
        assert!(
            self.map.insert(name.clone(), decl).is_none(),
            "Function {name} defined multiple times!"
        );
    }

    pub fn get_def(&self, name: &str) -> Option<&FunctionDecl> {
        const PRINT_SIG: &'static FunctionDecl = &FunctionDecl { no_validation: true, arguments: vec![], return_t: TypeKind::Builtin(BuiltinTypeKind::Null) };
        const PRINTFLUSH_SIG: &'static FunctionDecl = &FunctionDecl { no_validation: true, arguments: vec![], return_t: TypeKind::Builtin(BuiltinTypeKind::Null) };

        match name {
            "println" | "print" => Some(PRINT_SIG),
            "printflush" => Some(PRINTFLUSH_SIG),
            name => self.map.get(name),
        }
    }
}
