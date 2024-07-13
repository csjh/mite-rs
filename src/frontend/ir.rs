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

#[derive(Debug, Clone)]
pub struct InitFunction {
    pub body: Vec<IRExpression>,
    pub locals: HashMap<String, TypeInformation>,
}

pub(crate) struct IRModule {
    pub init: InitFunction,
    pub exports: HashMap<String, Export>,
    pub globals: HashMap<String, TypeInformation>,
    pub structs: HashMap<String, StructTypeInformation>,
    pub imports: HashMap<String, HashMap<String, Export>>,
    pub functions: Vec<IRFunction>,
}

#[derive(Debug, Clone)]
pub(crate) struct Literal {
    pub ty: PrimitiveTypeInformation,
    pub value: super::parser::Literal,
}

#[derive(Debug, Clone)]
pub(crate) struct Block {
    pub ty: TypeInformation,
    pub body: Vec<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct While {
    pub test: Box<IRExpression>,
    pub body: Box<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct DoWhile {
    pub body: Box<IRExpression>,
    pub test: Box<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct For {
    pub init: Option<Box<IRExpression>>,
    pub test: Option<Box<IRExpression>>,
    pub update: Option<Box<IRExpression>>,
    pub body: Box<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct Array {
    pub ty: ArrayTypeInformation,
    pub elements: Vec<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct Property {
    pub name: String,
    pub value: IRExpression,
}

#[derive(Debug, Clone)]
pub(crate) struct Object {
    pub ty: StructTypeInformation,
    pub properties: Vec<Property>,
}

#[derive(Debug, Clone)]
pub(crate) struct Unary {
    pub ty: TypeInformation,
    pub operator: UnaryOperator,
    pub operand: Box<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct Binary {
    pub ty: TypeInformation,
    pub operator: BinaryOperator,
    pub left: Box<IRExpression>,
    pub right: Box<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct Assignment {
    pub ty: TypeInformation,
    pub operator: AssignmentOperator,
    pub left: Box<IRExpression>,
    pub right: Box<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct Logical {
    pub ty: TypeInformation,
    pub operator: LogicalOperator,
    pub left: Box<IRExpression>,
    pub right: Box<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct If {
    pub ty: TypeInformation,
    pub test: Box<IRExpression>,
    pub consequent: Box<IRExpression>,
    pub alternate: Option<Box<IRExpression>>,
}

#[derive(Debug, Clone)]
pub(crate) struct Member {
    pub ty: TypeInformation,
    pub object: Box<IRExpression>,
    pub property: String,
}

#[derive(Debug, Clone)]
pub(crate) struct Index {
    pub ty: TypeInformation,
    pub object: Box<IRExpression>,
    pub index: Box<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct Return {
    pub value: Box<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct LocalGet {
    pub ty: TypeInformation,
    pub name: String,
}

#[derive(Debug, Clone)]
pub(crate) struct LocalSet {
    pub ty: TypeInformation,
    pub name: String,
    pub value: Box<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct GlobalGet {
    pub ty: TypeInformation,
    pub name: String,
}

#[derive(Debug, Clone)]
pub(crate) struct GlobalSet {
    pub ty: TypeInformation,
    pub name: String,
    pub value: Box<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct DirectCall {
    pub ty: TypeInformation,
    pub callee: FunctionTypeInformation,
    pub arguments: Vec<IRExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct IndirectCall {
    pub ty: TypeInformation,
    pub callee: Box<IRExpression>,
    pub arguments: Vec<IRExpression>,
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
    //     pub ty: TypeInformation,
    //     pub expressions: Vec<IRExpression>,
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
                Declaration::$type(decl, exported) => Some((decl.clone(), *exported)),
                _ => None,
            })
            .for_each($cb);
    };
}

pub fn ast_to_ir(program: Program, options: Options) -> IRModule {
    let mut ctx = IRContext::from_program(&program, options);

    let mut module = IRModule {
        init: InitFunction {
            body: Vec::new(),
            locals: HashMap::new(),
        },
        imports: HashMap::new(),
        exports: HashMap::new(),
        structs: HashMap::new(),
        globals: HashMap::new(),
        functions: Vec::new(),
    };

    // order of declaration handling:
    // 1. imports (only structs, to make sure are available in types)
    // 2. structs (for types)
    // ---- we are here ----
    // 3. imports (functions and variables)
    // 4. functions (only signatures)
    // 5. variables (after functions in case they are used in initializers)
    // 6. functions (implementations)
    // any duplicate steps could/should be avoided but realistically not that big of a deal

    for_each_decl!(program, Import, |(decl, exported)| {
        if exported {
            panic!("Cannot export imports");
        }

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
                            ctx.globals.insert(
                                specifier.local.clone(),
                                Box::new(DirectFunction::new(*info, specifier.local.clone())),
                            );
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

    for_each_decl!(program, Function, |(decl, exported)| {
        let ty = decl.to_type(&ctx.types);

        ctx.globals.insert(
            decl.name.clone(),
            Box::new(DirectFunction::new(ty, decl.name.clone())),
        );

        if exported {
            module
                .exports
                .insert(decl.name.clone(), Export::Function(ty));
        }
    });

    for_each_decl!(program, Variable, |(decl, exported)| {
        for var in decl.declarations {
            if var.init.is_none() {
                panic!("Global variable {} must be initialized", var.id);
            }

            if var.type_annotation.is_none() {
                panic!("Global variable {} must have a type annotation", var.id);
            }

            let global = _;
            ctx.globals.insert(var.id.clone(), global);

            module
                .init
                .body
                .push(global.set(&to_ir(&mut ctx, var.init.as_ref().unwrap())));

            if exported {
                module.exports.insert(var.id.clone(), Export::Variable(_));
            }
        }
    });

    // copy the locals used in the global variable declarations
    module.init.locals = ctx
        .locals
        .iter()
        .map(|(k, v)| (k.clone(), v.ty()))
        .collect();

    for_each_decl!(program, Function, |(decl, exported)| {
        ctx.locals = HashMap::new();

        let body = to_ir(&mut ctx, &decl.body);

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

        if exported {
            module.exports.insert(
                decl.name.clone(),
                Export::Function(decl.to_type(&ctx.types)),
            );
        }
    });

    module
}

pub(super) struct Stacks {
    pub continues: Vec<String>,
    pub breaks: Vec<String>,
    pub depth: u32,
}

pub(super) struct IRContext {
    pub types: Types,
    pub stacks: Stacks,
    pub locals: HashMap<String, Box<dyn MiteType>>,
    pub globals: HashMap<String, Box<dyn MiteType>>,
    pub current_function: FunctionInformation,
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
}

fn to_ir(ctx: &mut IRContext, expr: &Expression) -> IRExpression {
    match expr {
        Expression::Identifier(x) => identifier_to_ir(ctx, x),
        Expression::Literal(x) => literal_to_ir(ctx, x),
        Expression::Block(x) => block_to_ir(ctx, x),
        Expression::While(x) => while_to_ir(ctx, x),
        Expression::DoWhile(x) => do_while_to_ir(ctx, x),
        Expression::For(x) => for_to_ir(ctx, x),
        Expression::Array(x) => array_to_ir(ctx, x),
        Expression::Object(x) => object_to_ir(ctx, x),
        Expression::Unary(x) => unary_to_ir(ctx, x),
        Expression::Binary(x) => binary_to_ir(ctx, x),
        Expression::Assignment(x) => assignment_to_ir(ctx, x),
        Expression::Logical(x) => logical_to_ir(ctx, x),
        Expression::If(x) => if_to_ir(ctx, x),
        Expression::Member(x) => member_to_ir(ctx, x),
        Expression::Index(x) => index_to_ir(ctx, x),
        Expression::Return(x) => return_to_ir(ctx, x),
        Expression::Break(x) => break_to_ir(ctx, x),
        Expression::Continue(x) => continue_to_ir(ctx, x),
        Expression::Empty(x) => empty_to_ir(ctx, x),
        Expression::Sequence(x) => sequence_to_ir(ctx, x),
        Expression::Call(x) => call_to_ir(ctx, x),
    }
}

fn identifier_to_ir(ctx: &IRContext, expr: &Identifier) -> IRExpression {
    if let Some(local) = ctx.locals.get(expr) {
        IRExpression::LocalGet(LocalGet {
            ty: local.ty(),
            name: expr.clone(),
        })
    } else if let Some(global) = ctx.globals.get(expr) {
        IRExpression::GlobalGet(GlobalGet {
            ty: global.ty(),
            name: expr.clone(),
        })
    } else {
        panic!("Unknown identifier {}", expr);
    }
}
fn literal_to_ir(ctx: &IRContext, expr: &super::parser::Literal) -> IRExpression {}
fn block_to_ir(ctx: &IRContext, expr: &super::parser::Block) -> IRExpression {}
fn while_to_ir(ctx: &IRContext, expr: &super::parser::While) -> IRExpression {}
fn do_while_to_ir(ctx: &IRContext, expr: &super::parser::DoWhile) -> IRExpression {}
fn for_to_ir(ctx: &IRContext, expr: &super::parser::For) -> IRExpression {}
fn array_to_ir(ctx: &IRContext, expr: &super::parser::Array) -> IRExpression {}
fn object_to_ir(ctx: &IRContext, expr: &super::parser::Object) -> IRExpression {}
fn unary_to_ir(ctx: &IRContext, expr: &super::parser::Unary) -> IRExpression {}
fn binary_to_ir(ctx: &IRContext, expr: &super::parser::Binary) -> IRExpression {}
fn assignment_to_ir(ctx: &IRContext, expr: &super::parser::Assignment) -> IRExpression {}
fn logical_to_ir(ctx: &IRContext, expr: &super::parser::Logical) -> IRExpression {}
fn if_to_ir(ctx: &IRContext, expr: &super::parser::If) -> IRExpression {}
fn member_to_ir(ctx: &IRContext, expr: &super::parser::Member) -> IRExpression {}
fn index_to_ir(ctx: &IRContext, expr: &super::parser::Index) -> IRExpression {}
fn return_to_ir(ctx: &IRContext, expr: &super::parser::Return) -> IRExpression {}
fn break_to_ir(ctx: &IRContext, expr: &super::parser::Break) -> IRExpression {}
fn continue_to_ir(ctx: &IRContext, expr: &super::parser::Continue) -> IRExpression {}
fn empty_to_ir(ctx: &IRContext, expr: &super::parser::Empty) -> IRExpression {}
fn sequence_to_ir(ctx: &IRContext, expr: &super::parser::Sequence) -> IRExpression {}
fn call_to_ir(ctx: &IRContext, expr: &super::parser::Call) -> IRExpression {}
