use super::{
    mitetype::{
        ArrayTypeInformation, DirectFunction, FunctionInformation, FunctionTypeInformation,
        MiteType, PrimitiveTypeInformation, StructTypeInformation, TypeInformation,
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
    pub ty: FunctionTypeInformation,
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

#[derive(Debug, Clone)]
pub(crate) struct Literal {
    ty: PrimitiveTypeInformation,
    value: super::parser::Literal,
}

#[derive(Debug, Clone)]
pub(crate) struct Block {
    ty: TypeInformation,
    body: Vec<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct While {
    test: Box<IRExpression>,
    body: Box<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct DoWhile {
    body: Box<IRExpression>,
    test: Box<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct For {
    init: Option<Box<IRExpression>>,
    test: Option<Box<IRExpression>>,
    update: Option<Box<IRExpression>>,
    body: Box<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct Array {
    ty: ArrayTypeInformation,
    elements: Vec<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct Property {
    name: String,
    value: IRExpression,
}

#[derive(Debug, Clone)]
pub(crate) struct Object {
    ty: StructTypeInformation,
    properties: Vec<Property>,
}

#[derive(Debug, Clone)]
pub(crate) struct Unary {
    ty: TypeInformation,
    operator: UnaryOperator,
    operand: Box<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct Binary {
    ty: TypeInformation,
    operator: BinaryOperator,
    left: Box<IRExpression>,
    right: Box<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct Assignment {
    ty: TypeInformation,
    operator: AssignmentOperator,
    left: Box<IRExpression>,
    right: Box<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct Logical {
    ty: TypeInformation,
    operator: LogicalOperator,
    left: Box<IRExpression>,
    right: Box<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct If {
    ty: TypeInformation,
    test: Box<IRExpression>,
    consequent: Box<IRExpression>,
    alternate: Option<Box<IRExpression>>,
}

#[derive(Debug, Clone)]
pub(crate) struct Member {
    ty: TypeInformation,
    object: Box<IRExpression>,
    property: String,
}

#[derive(Debug, Clone)]
pub(crate) struct Index {
    ty: TypeInformation,
    object: Box<IRExpression>,
    index: Box<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct Return {
    value: Box<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct LocalGet {
    ty: TypeInformation,
    name: String,
}

#[derive(Debug, Clone)]
pub(crate) struct LocalSet {
    ty: TypeInformation,
    name: String,
    value: Box<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct GlobalGet {
    ty: TypeInformation,
    name: String,
}

#[derive(Debug, Clone)]
pub(crate) struct GlobalSet {
    ty: TypeInformation,
    name: String,
    value: Box<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct DirectCall {
    ty: TypeInformation,
    callee: FunctionTypeInformation,
    arguments: Vec<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct IndirectCall {
    ty: TypeInformation,
    callee: Box<IRExpression>,
    arguments: Vec<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) enum IRExpression {
    // same as parser
    Literal(Literal),
    Block(Block),
    Break,
    Continue,
    While(While),
    DoWhile(DoWhile),
    For(For),
    Empty,
    Array(Array),
    Object(Object),
    // this is turned into a block
    // Sequence {
    //     ty: TypeInformation,
    //     expressions: Vec<IRExpression>,
    // },
    Unary(Unary),
    Binary(Binary),
    Assignment(Assignment),
    Logical(Logical),
    If(If),
    Member(Member),
    Index(Index),
    Return(Return),
    // new
    Void(Box<IRExpression>),
    LocalGet(LocalGet),
    LocalSet(LocalSet),
    GlobalGet(GlobalGet),
    GlobalSet(GlobalSet),
    DirectCall(DirectCall),
    IndirectCall(IndirectCall),
}

impl IRExpression {
    pub fn ty(&self) -> TypeInformation {
        let void = TypeInformation::Primitive(PrimitiveTypeInformation {
            name: "void",
            sizeof: 0,
        });

        match self {
            IRExpression::Literal(Literal { ty, .. }) => TypeInformation::Primitive(ty.clone()),
            IRExpression::Array(Array { ty, .. }) => TypeInformation::Array(ty.clone()),
            IRExpression::Object(Object { ty, .. }) => TypeInformation::Struct(ty.clone()),
            IRExpression::Block(Block { ty, .. }) => ty.clone(),
            IRExpression::Unary(Unary { ty, .. }) => ty.clone(),
            IRExpression::Binary(Binary { ty, .. }) => ty.clone(),
            IRExpression::Assignment(Assignment { ty, .. }) => ty.clone(),
            IRExpression::Logical(Logical { ty, .. }) => ty.clone(),
            IRExpression::If(If { ty, .. }) => ty.clone(),
            IRExpression::Member(Member { ty, .. }) => ty.clone(),
            IRExpression::Index(Index { ty, .. }) => ty.clone(),
            IRExpression::LocalGet(LocalGet { ty, .. }) => ty.clone(),
            IRExpression::LocalSet(LocalSet { ty, .. }) => ty.clone(),
            IRExpression::GlobalGet(GlobalGet { ty, .. }) => ty.clone(),
            IRExpression::GlobalSet(GlobalSet { ty, .. }) => ty.clone(),
            IRExpression::DirectCall(DirectCall { ty, .. }) => ty.clone(),
            IRExpression::IndirectCall(IndirectCall { ty, .. }) => ty.clone(),
            IRExpression::Break => void,
            IRExpression::Continue => void,
            IRExpression::While(While { .. }) => void,
            IRExpression::DoWhile(DoWhile { .. }) => void,
            IRExpression::For(For { .. }) => void,
            IRExpression::Empty => void,
            IRExpression::Return(Return { .. }) => void,
            IRExpression::Void(_) => void,
        }
    }
}

#[macro_export]
macro_rules! for_each_decl {
    ($program:expr, $type:ident, $cb:expr) => {
        $program
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

pub fn ast_to_ir(program: Program, options: Options) -> IRModule {
    let mut ctx = IRContext::from_program(&program, options);

    let mut module = IRModule {
        imports: HashMap::new(),
        exports: HashMap::new(),
        structs: HashMap::new(),
        globals: HashMap::new(),
        functions: Vec::new(),
    };

    // order of declaration handling:
    // 1. imports (only structs, to make sure are available in types)
    // 2. structs (for types)
    // 3. imports (functions and variables)
    // 4. functions (only signatures)
    // 5. variables (after functions in case they are used in initializers)
    // 6. functions (implementations)
    // 7. exports
    // any duplicate steps could/should be avoided but realistically not that big of a deal

    for_each_decl!(program, Import, |decl| {
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
                        Export::Struct(info) => { /* already inserted to types */ }
                        Export::Function(info) => {
                            ctx.globals.insert(specifier.local.clone(), _);
                        }
                        Export::Variable(info) => {
                            ctx.globals.insert(specifier.local.clone(), _);
                        }
                    }
                } else {
                    panic!("Imported symbol {} not found", specifier.imported);
                }
            }
        }
    });

    for_each_decl!(program, Function, |decl| {
        ctx.globals.insert(
            decl.name.clone(),
            Box::new(DirectFunction::new(
                decl.to_type(&ctx.types),
                decl.name.clone(),
            )),
        );
    });

    for_each_decl!(program, Variable, |decl| {
        for var in decl.declarations {
            ctx.globals.insert(var.id.clone(), _);
        }
    });

    for_each_decl!(program, Function, |decl| {
        ctx.locals = HashMap::new();

        let body = ctx.to_ir(&decl.body);

        module.functions.push(IRFunction {
            name: decl.name.clone(),
            ty: decl.to_type(&ctx.types),
            locals: ctx
                .locals
                .iter()
                .map(|(k, v)| (k.clone(), v.ty()))
                .collect(),
            body,
        });
    });

    for_each_decl!(program, Export, |decl| -> () {
        match *decl {
            Declaration::Function(decl) => {
                module.exports.insert(
                    decl.name.clone(),
                    Export::Function(decl.to_type(&ctx.types)),
                );
            }
            Declaration::Variable(decl) => {
                for var in decl.declarations {
                    let ty = ctx.types.parse_type(match var.type_annotation {
                        Some(ty) => &ty,
                        None => panic!("Global variables must have a type annotation"),
                    });

                    module.exports.insert(var.id.clone(), Export::Variable(ty));
                }
            }
            Declaration::Struct(decl) => {
                let ty = match ctx.types.get(&decl.id).unwrap().clone() {
                    TypeInformation::Struct(info) => info,
                    _ => unreachable!(),
                };

                module.exports.insert(decl.id.clone(), Export::Struct(ty));
            }
            Declaration::Export(_) | Declaration::Import(_) => {
                panic!("Invalid export: cannot export exports or imports")
            }
        }
    });

    module
}

pub(super) struct Stacks {
    continues: Vec<String>,
    breaks: Vec<String>,
    depth: u32,
}

pub(super) struct IRContext {
    types: Types,
    stacks: Stacks,
    locals: HashMap<String, Box<dyn MiteType>>,
    globals: HashMap<String, Box<dyn MiteType>>,
    current_function: FunctionInformation,
}

impl IRContext {
    fn from_program(program: &Program, options: Options) -> Self {
        let types = Types(build_types(program, options));

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

    fn to_ir(&mut self, expr: &Expression) -> IRExpression {
        match expr {
            Expression::Identifier()
        }
    }
}
