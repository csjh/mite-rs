use super::{
    mitetype::{
        ArrayTypeInformation, FunctionInformation, FunctionTypeInformation, MiteType,
        PrimitiveTypeInformation, StructTypeInformation, TypeInformation,
    },
    type_initialization::{build_types, Types},
};
use crate::frontend::{parser::*, tokenizer::tokenize};
use std::collections::HashMap;

pub struct ResolvedImport {
    pub is_mite: bool,
    pub source: String,
}

#[derive(Debug, Clone)]
pub struct Options {
    pub resolve_import: fn(&String) -> ResolvedImport,
}

pub struct IRFunction {
    pub name: String,
    pub implementation: FunctionInformation,
    pub locals: HashMap<String, TypeInformation>,
    pub body: IRExpression,
}

#[derive(Clone)]
pub enum Export {
    Function(FunctionTypeInformation),
    Struct(StructTypeInformation),
    Variable(TypeInformation),
}

pub(crate) struct IRModule {
    pub exports: HashMap<String, Export>,
    pub globals: HashMap<String, TypeInformation>,
    pub structs: HashMap<String, StructTypeInformation>,
    pub imports: HashMap<String, HashMap<String, Export>>,
    pub functions: Vec<IRFunction>,
}

pub(crate) enum IRExpression {
    // same as parser
    Literal {
        ty: PrimitiveTypeInformation,
        value: Literal,
    },
    Block {
        ty: TypeInformation,
        body: Vec<IRExpression>,
    },
    Break,
    Continue,
    While {
        test: Box<IRExpression>,
        body: Box<IRExpression>,
    },
    DoWhile {
        body: Box<IRExpression>,
        test: Box<IRExpression>,
    },
    For {
        init: Option<Box<IRExpression>>,
        test: Option<Box<IRExpression>>,
        update: Option<Box<IRExpression>>,
        body: Box<IRExpression>,
    },
    Empty,
    Array {
        ty: ArrayTypeInformation,
        elements: Vec<IRExpression>,
    },
    Object {
        ty: StructTypeInformation,
        properties: Vec<Property>,
    },
    // this is turned into a block
    // Sequence {
    //     ty: TypeInformation,
    //     expressions: Vec<IRExpression>,
    // },
    Unary {
        ty: TypeInformation,
        operator: UnaryOperator,
        operand: Box<IRExpression>,
    },
    Binary {
        ty: TypeInformation,
        operator: BinaryOperator,
        left: Box<IRExpression>,
        right: Box<IRExpression>,
    },
    Assignment {
        ty: TypeInformation,
        operator: AssignmentOperator,
        left: Box<IRExpression>,
        right: Box<IRExpression>,
    },
    Logical {
        ty: TypeInformation,
        operator: LogicalOperator,
        left: Box<IRExpression>,
        right: Box<IRExpression>,
    },
    If {
        ty: TypeInformation,
        test: Box<IRExpression>,
        consequent: Box<IRExpression>,
        alternate: Option<Box<IRExpression>>,
    },
    Member {
        ty: TypeInformation,
        object: Box<IRExpression>,
        property: String,
    },
    Index {
        ty: TypeInformation,
        object: Box<IRExpression>,
        index: Box<IRExpression>,
    },
    Return {
        value: Box<IRExpression>,
    },
    // new
    Void(Box<IRExpression>),
    DirectCall {
        ty: TypeInformation,
        callee: FunctionTypeInformation,
        arguments: Vec<IRExpression>,
    },
    IndirectCall {
        ty: TypeInformation,
        callee: Box<IRExpression>,
        arguments: Vec<IRExpression>,
    },
}

impl IRExpression {
    pub fn ty(&self) -> TypeInformation {
        let void = TypeInformation::Primitive(PrimitiveTypeInformation {
            name: "void".to_string(),
            sizeof: 0,
        });

        match self {
            IRExpression::Literal { ty, .. } => TypeInformation::Primitive(ty.clone()),
            IRExpression::Array { ty, .. } => TypeInformation::Array(ty.clone()),
            IRExpression::Object { ty, .. } => TypeInformation::Struct(ty.clone()),
            IRExpression::Block { ty, .. } => ty.clone(),
            IRExpression::Unary { ty, .. } => ty.clone(),
            IRExpression::Binary { ty, .. } => ty.clone(),
            IRExpression::Assignment { ty, .. } => ty.clone(),
            IRExpression::Logical { ty, .. } => ty.clone(),
            IRExpression::If { ty, .. } => ty.clone(),
            IRExpression::Member { ty, .. } => ty.clone(),
            IRExpression::Index { ty, .. } => ty.clone(),
            IRExpression::DirectCall { ty, .. } => ty.clone(),
            IRExpression::IndirectCall { ty, .. } => ty.clone(),
            IRExpression::Break => void,
            IRExpression::Continue => void,
            IRExpression::While { .. } => void,
            IRExpression::DoWhile { .. } => void,
            IRExpression::For { .. } => void,
            IRExpression::Empty => void,
            IRExpression::Return { .. } => void,
            IRExpression::Void(_) => void,
        }
    }
}

pub fn ast_to_ir(program: Program, options: Options) -> IRModule {
    let mut ctx = IRContext::from_program(&program);

    let mut module = IRModule {
        imports: HashMap::new(),
        exports: HashMap::new(),
        structs: HashMap::new(),
        globals: HashMap::new(),
        functions: Vec::new(),
    };

    macro_rules! for_each_decl {
        ($type:ident, $cb:expr) => {
            program
                .body
                .iter()
                .filter_map(|decl| match decl {
                    Declaration::$type(decl) => Some(decl.clone()),
                    Declaration::Export(decl) => match *decl.clone() {
                        Declaration::$type(decl) => Some(decl),
                        _ => None,
                    },
                    _ => None,
                })
                .for_each($cb);
        };
    }

    for_each_decl!(Import, |decl| {
        let import_data = (options.resolve_import)(&decl.source);
        module.imports.insert(decl.source.clone(), HashMap::new());
        let imports = module.imports.get_mut(&decl.source).unwrap();

        if import_data.is_mite {
            let tokens = tokenize(&import_data.source);
            let ast = parse(tokens);
            let imported_module = ast_to_ir(ast, options.clone());

            for specifier in decl.specifiers {
                let export = imported_module.exports.get(&specifier.imported);
                if let Some(export) = export {
                    imports.insert(specifier.local, export.clone());
                    match export {
                        Export::Struct(info) => {
                            ctx.types.add(
                                specifier.local.clone(),
                                TypeInformation::Struct(info.clone()),
                            );
                        }
                        Export::Function(info) => {
                            ctx.globals.insert(specifier.local.clone(), info);
                        }
                        Export::Variable(info) => {
                            ctx.globals.insert(specifier.local.clone(), info.clone());
                        }
                    }
                } else {
                    panic!("Imported symbol {} not found", specifier.imported);
                }
            }
        }
    });

    module
}

pub(super) struct Stacks {
    continues: Vec<String>,
    breaks: Vec<String>,
    depth: usize,
}

pub(super) struct IRContext {
    types: Types,
    stacks: Stacks,
    locals: HashMap<String, Box<dyn MiteType>>,
    globals: HashMap<String, Box<dyn MiteType>>,
    current_function: FunctionInformation,
}

impl IRContext {
    fn from_program(program: &Program) -> Self {
        let types = Types(build_types(program));

        IRContext {
            current_function: FunctionInformation {
                args: Vec::new(),
                ret: Box::new(types.get_known("void").clone()),
            },
            types,
            stacks: Stacks {
                continues: Vec::new(),
                breaks: Vec::new(),
                depth: 0,
            },
            locals: HashMap::new(),
            globals: HashMap::new(),
        }
    }
}
