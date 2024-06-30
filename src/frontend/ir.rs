use crate::frontend::parser::*;

pub struct ResolvedImport {
    pub is_mite: bool,
    pub source: String,
}

pub struct Options {
    pub resolve_import: fn(String) -> ResolvedImport,
}

pub(crate) struct ModuleIR {}

pub fn ast_to_ir(program: Program, options: Options) -> ModuleIR {
    ModuleIR {}
}

struct IRContext {}

impl IRContext {
    
}

