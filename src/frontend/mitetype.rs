use std::{collections::HashMap, fmt};

use crate::frontend::ir::IRContext;

use super::parser::{BinaryOperator, UnaryOperator};

#[derive(Debug, Clone)]
pub(super) struct PrimitiveTypeInformation {
    pub name: String,
    pub sizeof: usize,
}

#[derive(Debug, Clone)]
pub(super) struct ArrayTypeInformation {
    pub name: String,
    pub element_type: Box<TypeInformation>,
    pub length: Option<usize>,
    pub is_ref: bool,
}

#[derive(Debug, Clone)]
pub(super) struct StructField {
    pub name: String,
    pub ty: TypeInformation,
    pub offset: usize,
}

#[derive(Debug, Clone)]
pub(super) struct StructTypeInformation {
    pub name: String,
    pub fields: HashMap<String, StructField>,
    pub methods: HashMap<String, FunctionTypeInformation>,
    pub sizeof: usize,
    pub is_ref: bool,
}

#[derive(Debug, Clone)]
pub(super) struct Parameter {
    pub name: String,
    pub ty: TypeInformation,
}

#[derive(Debug, Clone)]
pub(super) struct FunctionInformation {
    pub args: Vec<Parameter>,
    pub ret: Box<TypeInformation>,
}

#[derive(Debug, Clone)]
pub(super) struct FunctionTypeInformation {
    pub name: String,
    pub implementation: FunctionInformation,
    pub is_ref: bool,
}

#[derive(Debug, Clone)]
pub(super) struct StringTypeInformation {}

#[derive(Debug, Clone)]
pub(super) enum TypeInformation {
    Primitive(PrimitiveTypeInformation),
    Array(ArrayTypeInformation),
    Struct(StructTypeInformation),
    String(StringTypeInformation),
    Function(FunctionTypeInformation),
}

impl TypeInformation {
    pub fn is_ref(&self) -> bool {
        match self {
            TypeInformation::Primitive(_) => false,
            TypeInformation::Array(info) => info.is_ref,
            TypeInformation::Struct(info) => info.is_ref,
            TypeInformation::String(_) => true,
            TypeInformation::Function(info) => info.is_ref,
        }
    }

    pub fn sizeof(&self) -> usize {
        if self.is_ref() {
            return 4;
        } else {
            match self {
                TypeInformation::Primitive(info) => info.sizeof,
                TypeInformation::Array(info) => info.element_type.sizeof() * info.length.unwrap_or(0),
                TypeInformation::Struct(info) => info.sizeof,
                TypeInformation::String(_) => unreachable!(),
                TypeInformation::Function(_) => 8,
            }
        }
    }
}

impl fmt::Display for TypeInformation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeInformation::Primitive(info) => write!(f, "{}", info.name),
            TypeInformation::Array(info) => write!(f, "{}", info.name),
            TypeInformation::Struct(info) => write!(f, "{}", info.name),
            TypeInformation::String(_) => write!(f, "string"),
            TypeInformation::Function(info) => write!(f, "{}", info.name),
        }
    }
}

pub(super) trait MiteType {
    // get the value as a primitive (pointer for structs and arrays, value for locals)
    fn get(&self) -> Primitive;
    // set the value
    fn set(&mut self, value: dyn MiteType);
    // access with . operator
    fn access(&self, key: String) -> dyn MiteType;
    // access with [] operator
    fn index(&self, index: dyn MiteType) -> dyn MiteType;
    // call the value as a function
    fn call(&self, args: Vec<Box<dyn MiteType>>) -> dyn MiteType;
    // get the full size of the value
    fn sizeof(&self) -> Primitive;
    // get handler for unary operator
    fn unary_op(&self, op: UnaryOperator) -> dyn FnOnce() -> dyn MiteType;
    // get handler for binary operator
    fn binary_op(&self, op: BinaryOperator) -> dyn FnOnce(&Primitive) -> dyn MiteType;

    // get the type information
    fn ty(&self) -> TypeInformation;
}

pub(super) struct Primitive<'a> {
    pub ctx: &'a mut IRContext,
    pub ty: PrimitiveTypeInformation,
}
