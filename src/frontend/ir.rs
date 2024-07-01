use super::mitetype::{FunctionInformation, MiteType, TypeInformation};
use crate::frontend::parser::*;
use std::collections::HashMap;

pub struct ResolvedImport {
    pub is_mite: bool,
    pub source: String,
}

pub struct Options {
    pub resolve_import: fn(String) -> ResolvedImport,
}

pub struct IRFunction {
    pub name: String,
    pub implementation: FunctionInformation,
    pub locals: Vec<(String, TypeInformation)>,
    pub body: IRExpression,
}

pub(crate) struct IRModule {
    pub imports: Vec,
    pub exports: Vec,
    pub structs: Vec,
    pub globals: Vec,
    pub functions: Vec<IRFunction>,
}

pub(crate) enum IRExpression {
    Void(Box<IRExpression>),
    LocalGet {
        variable: String,
    },
    LocalSet {
        variable: String,
        value: Box<IRExpression>,
    },
    GlobalGet {
        variable: String,
    },
    GlobalSet {
        variable: String,
        value: Box<IRExpression>,
    },
}

pub fn ast_to_ir(program: Program, options: Options) -> IRModule {
    IRModule {}
}

pub(super) struct IRContext {
    locals: HashMap<String, Box<dyn MiteType>>,
    globals: HashMap<String, Box<dyn MiteType>>,
}

impl IRContext {}
