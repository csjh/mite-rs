use super::{
    mitetype::{FunctionInformation, MiteType, StructTypeInformation, TypeInformation},
    type_initialization::{build_types, Types},
};
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
    pub locals: HashMap<String, TypeInformation>,
    pub body: IRExpression,
}

pub(crate) struct IRModule {
    pub imports: HashMap<String, HashMap<String, TypeInformation>>,
    pub exports: Vec<String>,
    pub structs: HashMap<String, StructTypeInformation>,
    pub globals: HashMap<String, TypeInformation>,
    pub functions: Vec<IRFunction>,
}

pub(crate) enum IRExpression {
    // same as parser
    Literal(Literal),
    Block {
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
        elements: Vec<IRExpression>,
    },
    Object {
        type_annotation: TypeIdentifier,
        properties: Vec<Property>,
    },
    Sequence {
        expressions: Vec<IRExpression>,
    },
    Unary {
        operator: UnaryOperator,
        operand: Box<IRExpression>,
    },
    Binary {
        operator: BinaryOperator,
        left: Box<IRExpression>,
        right: Box<IRExpression>,
    },
    Assignment {
        operator: AssignmentOperator,
        left: Box<IRExpression>,
        right: Box<IRExpression>,
    },
    Logical {
        operator: LogicalOperator,
        left: Box<IRExpression>,
        right: Box<IRExpression>,
    },
    If {
        test: Box<IRExpression>,
        consequent: Box<IRExpression>,
        alternate: Option<Box<IRExpression>>,
    },
    Member {
        object: Box<IRExpression>,
        property: String,
    },
    Index {
        object: Box<IRExpression>,
        index: Box<IRExpression>,
    },
    Return {
        value: Box<IRExpression>,
    },
    // new
    Void(Box<IRExpression>),
    DirectCall {
        callee: String,
        arguments: Vec<IRExpression>,
    },
    IndirectCall {
        callee: Box<IRExpression>,
        arguments: Vec<IRExpression>,
    },
}

pub fn ast_to_ir(program: Program, options: Options) -> IRModule {
    let ctx = IRContext::from_program(&program);

    let module = IRModule {
        imports: HashMap::new(),
        exports: Vec::new(),
        structs: HashMap::new(),
        globals: HashMap::new(),
        functions: Vec::new(),
    };

    for expr in program.body {
        match expr {
            Declaration::Import(decl) => {}
            Declaration::Function(decl) => {}
            Declaration::Struct(_) => {}
            Declaration::Variable(decl) => {}
            Declaration::Export(decl) => {}
        }
    }

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
    pub fn from_program(program: &Program) -> Self {
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
