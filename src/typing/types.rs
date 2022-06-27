use std::collections::HashMap;

use uuid::Uuid;

use crate::parse2::types::ast_types::ASTType;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub Uuid);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BuiltinTypeKind {
    Num,
    Bool,
    Str,
    Null,
    Never,
    MlogObject,
}

impl ToString for BuiltinTypeKind {
    fn to_string(&self) -> String {
        match self {
            Self::Num => "num",
            Self::Bool => "bool",
            Self::Str => "str",
            Self::Null => "null",
            Self::Never => "!",
            Self::MlogObject => "mobj",
        }
        .to_string()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Builtin(BuiltinTypeKind),
}

impl TryFrom<ASTType> for TypeKind {
    type Error = &'static str;
    fn try_from(value: ASTType) -> Result<Self, Self::Error> {
        Ok(match value {
            ASTType::Never => TypeKind::Builtin(BuiltinTypeKind::Never),
            ASTType::Null => TypeKind::Builtin(BuiltinTypeKind::Null),
            ASTType::Named(name) => match name.as_str() {
                "num" => TypeKind::Builtin(BuiltinTypeKind::Num),
                "bool" => TypeKind::Builtin(BuiltinTypeKind::Bool),
                "str" => TypeKind::Builtin(BuiltinTypeKind::Str),
                "mobj" => TypeKind::Builtin(BuiltinTypeKind::MlogObject),
                _ => Err("Cannot use non-bultin types (yet).")?,
            },
        })
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct TypeRegistry {
    kinds_by_id: HashMap<TypeId, TypeKind>,
    ids_by_kind: HashMap<TypeKind, TypeId>,
}

impl TypeRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register(&mut self, desc: TypeKind) -> TypeId {
        let id = TypeId(Uuid::new_v4());
        self.kinds_by_id.insert(id, desc.clone());
        assert!(
            self.ids_by_kind.insert(desc, id).is_none(),
            "Attempted to register the same type twice!"
        );
        id
    }

    pub fn fillin_builtins(&mut self) {
        self.register(TypeKind::Builtin(BuiltinTypeKind::Num));
        self.register(TypeKind::Builtin(BuiltinTypeKind::Bool));
        self.register(TypeKind::Builtin(BuiltinTypeKind::Str));
        self.register(TypeKind::Builtin(BuiltinTypeKind::Null));
        self.register(TypeKind::Builtin(BuiltinTypeKind::Never));
        self.register(TypeKind::Builtin(BuiltinTypeKind::MlogObject));
    }

    pub fn id_of_type(&self, t: &TypeKind) -> Option<&TypeId> {
        self.ids_by_kind.get(t)
    }

    pub fn type_of_id(&self, t: &TypeId) -> Option<&TypeKind> {
        self.kinds_by_id.get(t)
    }
}
