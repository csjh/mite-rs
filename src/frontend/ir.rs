use super::{
    mitetype::{
        ArrayTypeInformation, DirectFunction, FunctionInformation, FunctionTypeInformation,
        MiteType, PrimitiveTypeInformation, StructTypeInformation, TypeInformation,
    },
    type_initialization::{build_types, Types},
};
use crate::frontend::{mitetype::StringTypeInformation, parser::*, tokenizer::tokenize};
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

pub(crate) type Literal = super::parser::Literal;

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
pub(crate) struct Break;

#[derive(Debug, Clone)]
pub(crate) struct Continue;

#[derive(Debug, Clone)]
pub(crate) struct Empty;

#[derive(Debug, Clone)]
pub(crate) enum IRExpression {
    // same as parser
    Literal(Literal),
    Block(Block),
    Break(Break),
    Continue(Continue),
    While(While),
    DoWhile(DoWhile),
    For(For),
    Empty(Empty),
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
        macro_rules! primitive {
            ($name:expr, $sizeof:expr) => {
                TypeInformation::Primitive(PrimitiveTypeInformation {
                    name: $name,
                    sizeof: $sizeof,
                })
            };
        }

        let void = primitive!("void", 0);

        match self {
            IRExpression::Literal(lit) => match lit {
                Literal::I32(_) => primitive!("i32", 4),
                Literal::U32(_) => primitive!("u32", 4),
                Literal::I64(_) => primitive!("i64", 8),
                Literal::U64(_) => primitive!("u64", 8),
                Literal::F64(_) => primitive!("f64", 8),
                Literal::I8x16(_) => primitive!("i8x16", 16),
                Literal::U8x16(_) => primitive!("u8x16", 16),
                Literal::I16x8(_) => primitive!("i16x8", 16),
                Literal::U16x8(_) => primitive!("u16x8", 16),
                Literal::I32x4(_) => primitive!("i32x4", 16),
                Literal::U32x4(_) => primitive!("u32x4", 16),
                Literal::I64x2(_) => primitive!("i64x2", 16),
                Literal::U64x2(_) => primitive!("u64x2", 16),
                Literal::F32x4(_) => primitive!("f32x4", 16),
                Literal::F64x2(_) => primitive!("f64x2", 16),
                Literal::String(_) => TypeInformation::String(StringTypeInformation {}),
            },
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
            IRExpression::Break(_) => void,
            IRExpression::Continue(_) => void,
            IRExpression::While(While { .. }) => void,
            IRExpression::DoWhile(DoWhile { .. }) => void,
            IRExpression::For(For { .. }) => void,
            IRExpression::Empty(_) => void,
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
        module.imports.insert(decl.source, HashMap::new());
        let imports = module.imports.get_mut(&decl.source).unwrap();

        if import_data.is_mite {
            let tokens = tokenize(&import_data.source);
            let ast = parse(tokens);
            let imported_module = ast_to_ir(ast, options.clone());

            for specifier in decl.specifiers {
                let export = imported_module.exports.get(&specifier.imported);
                if let Some(export) = export {
                    let val = match export {
                        Export::Struct(info) => { /* already inserted to types */ }
                        Export::Function(info) => {
                            ctx.globals.insert(
                                specifier.local,
                                Box::new(DirectFunction::new(*info, specifier.local)),
                            );
                        }
                        Export::Variable(info) => {
                            ctx.globals.insert(specifier.local, _);
                        }
                    };

                    imports.insert(specifier.local, *export);

                    val
                } else {
                    panic!("Imported symbol {} not found", specifier.imported);
                }
            }
        }
    });

    for_each_decl!(program, Function, |(decl, exported)| {
        let ty = decl.to_type(&ctx.types);

        ctx.globals
            .insert(decl.name, Box::new(DirectFunction::new(ty, decl.name)));

        if exported {
            module.exports.insert(decl.name, Export::Function(ty));
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
            ctx.globals.insert(var.id, global);

            module
                .init
                .body
                .push(global.set(&to_ir(&mut ctx, var.init.unwrap(), None)));

            if exported {
                module.exports.insert(var.id, Export::Variable(_));
            }
        }
    });

    // copy the locals used in the global variable declarations
    module.init.locals = ctx.locals.into_iter().map(|(k, v)| (k, v.ty())).collect();

    for_each_decl!(program, Function, |(decl, exported)| {
        ctx.locals = HashMap::new();

        let ty = decl.to_type(&ctx.types);
        let body = to_ir(&mut ctx, decl.body, Some(*ty.implementation.ret));

        module.functions.push(IRFunction {
            name: decl.name,
            ty,
            locals: ctx.locals.into_iter().map(|(k, v)| (k, v.ty())).collect(),
            body,
        });

        if exported {
            module
                .exports
                .insert(decl.name, Export::Function(decl.to_type(&ctx.types)));
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
    pub expected: Option<TypeInformation>,
}

impl IRContext {
    fn from_program(program: &Program, options: Options) -> Self {
        let types = build_types(program, options);

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
            expected: None,
        }
    }
}

fn to_ir(
    ctx: &mut IRContext,
    expr: super::parser::Expression,
    mut expected: Option<TypeInformation>,
) -> IRExpression {
    std::mem::swap(&mut ctx.expected, &mut expected);

    let ret = match expr {
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
    };

    std::mem::swap(&mut ctx.expected, &mut expected);

    ret
}

fn statement_to_ir(ctx: &mut IRContext, expr: super::parser::Statement) -> IRExpression {
    match expr {
        Statement::Expression(x) => to_ir(ctx, x, None),
        Statement::VariableDeclaration(x) => variable_decl_to_ir(ctx, x),
    }
}

fn variable_decl_to_ir(
    ctx: &mut IRContext,
    expr: super::parser::VariableDeclaration,
) -> IRExpression {
    let mut exprs = Vec::new();

    for var in &expr.declarations {
        let (init, ty) = if let (&Some(ty), &Some(init)) = (&var.type_annotation, &var.init) {
            let ty = ctx.types.parse_type(ty);
            (to_ir(ctx, init, Some(ty)), ty)
        } else if let &Some(init) = &var.init {
            let init = to_ir(ctx, init, None);
            (init, init.ty())
        } else if let &Some(ty) = &var.type_annotation {
            let ty = ctx.types.parse_type(ty);
            (IRExpression::Empty(Empty {}), ty)
        } else {
            panic!("Variable declaration must have a type annotation or initializer");
        };

        let local = _;
        ctx.locals.insert(var.id, local);

        exprs.push(IRExpression::LocalSet(LocalSet {
            ty,
            name: var.id,
            value: Box::new(init),
        }));
    }

    IRExpression::Block(Block {
        ty: TypeInformation::Primitive(PrimitiveTypeInformation {
            name: "void",
            sizeof: 0,
        }),
        body: exprs,
    })
}

fn identifier_to_ir(ctx: &IRContext, expr: super::parser::Identifier) -> IRExpression {
    if let Some(local) = ctx.locals.get(&expr) {
        IRExpression::LocalGet(LocalGet {
            ty: local.ty(),
            name: expr,
        })
    } else if let Some(global) = ctx.globals.get(&expr) {
        IRExpression::GlobalGet(GlobalGet {
            ty: global.ty(),
            name: expr,
        })
    } else {
        panic!("Unknown identifier {}", expr);
    }
}

fn literal_to_ir(ctx: &IRContext, expr: super::parser::Literal) -> IRExpression {}

fn block_to_ir(ctx: &mut IRContext, expr: super::parser::Block) -> IRExpression {
    let body = expr
        .body
        .into_iter()
        // todo: handle ctx.expected
        .map(|expr| statement_to_ir(ctx, expr))
        .collect::<Vec<IRExpression>>();

    IRExpression::Block(Block {
        ty: body.last().unwrap().ty(),
        body,
    })
}

fn while_to_ir(ctx: &IRContext, expr: super::parser::While) -> IRExpression {}

fn do_while_to_ir(ctx: &IRContext, expr: super::parser::DoWhile) -> IRExpression {}

fn for_to_ir(ctx: &IRContext, expr: super::parser::For) -> IRExpression {}

fn array_to_ir(ctx: &IRContext, expr: super::parser::Array) -> IRExpression {}

fn object_to_ir(ctx: &IRContext, expr: super::parser::Object) -> IRExpression {}

fn unary_to_ir(ctx: &IRContext, expr: super::parser::Unary) -> IRExpression {}

fn binary_to_ir(ctx: &IRContext, expr: super::parser::Binary) -> IRExpression {}

fn assignment_to_ir(ctx: &IRContext, expr: super::parser::Assignment) -> IRExpression {}

fn logical_to_ir(ctx: &IRContext, expr: super::parser::Logical) -> IRExpression {}

fn if_to_ir(ctx: &IRContext, expr: super::parser::If) -> IRExpression {}

fn member_to_ir(ctx: &IRContext, expr: super::parser::Member) -> IRExpression {}

fn index_to_ir(ctx: &IRContext, expr: super::parser::Index) -> IRExpression {}

fn return_to_ir(ctx: &mut IRContext, expr: super::parser::Return) -> IRExpression {
    IRExpression::Return(Return {
        value: Box::new(expr.argument.map_or(IRExpression::Empty(Empty {}), |v| {
            to_ir(ctx, *v, Some(*ctx.current_function.ret.clone()))
        })),
    })
}

fn break_to_ir(_ctx: &IRContext, _expr: super::parser::Break) -> IRExpression {
    IRExpression::Break(Break {})
}

fn continue_to_ir(_ctx: &IRContext, _expr: super::parser::Continue) -> IRExpression {
    IRExpression::Continue(Continue {})
}

fn empty_to_ir(_ctx: &IRContext, _expr: super::parser::Empty) -> IRExpression {
    IRExpression::Empty(Empty {})
}

fn sequence_to_ir(ctx: &mut IRContext, expr: super::parser::Sequence) -> IRExpression {
    let body = expr
        .expressions
        .into_iter()
        .map(|expr| to_ir(ctx, expr, ctx.expected.clone()))
        .collect::<Vec<IRExpression>>();

    IRExpression::Block(Block {
        ty: body.last().unwrap().ty(),
        body,
    })
}

fn call_to_ir(ctx: &IRContext, expr: super::parser::Call) -> IRExpression {}
