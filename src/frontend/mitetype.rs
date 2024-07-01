use std::collections::HashMap;

use crate::frontend::ir::IRContext;
use crate::frontend::tokenizer::Token;

pub(super) struct PrimitiveTypeInformation {
    pub name: String,
    pub sizeof: usize,
}

pub(super) struct ArrayTypeInformation {
    pub element_type: Box<TypeInformation>,
    pub length: Option<usize>,
}

pub(super) struct StructField {
    pub name: String,
    pub ty: TypeInformation,
}

pub(super) struct StructTypeInformation {
    pub name: String,
    pub fields: HashMap<String, TypeInformation>,
    pub methods: HashMap<String, FunctionTypeInformation>,
}

pub(super) struct FunctionTypeInformation {
    pub name: String,
    pub implementation: FunctionInformation,
}

pub(super) struct StringTypeInformation {}

pub(super) enum TypeInformation {
    Primitive(PrimitiveTypeInformation),
    Array(ArrayTypeInformation),
    Struct(StructTypeInformation),
    String(StringTypeInformation),
    Function(FunctionTypeInformation),
}

pub(super) struct Parameter {
    pub name: String,
    pub ty: TypeInformation,
}

pub(super) struct FunctionInformation {
    pub args: Vec<TypeInformation>,
    pub ret: Box<TypeInformation>,
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
    fn unary_op(&self, op: Token) -> dyn FnOnce() -> dyn MiteType;
    // get handler for binary operator
    fn binary_op(&self, op: Token) -> dyn FnOnce(&Primitive) -> dyn MiteType;

    // get the type information
    fn ty(&self) -> TypeInformation;
}

pub(super) struct Primitive<'a> {
    pub ctx: &'a mut IRContext,
    pub ty: PrimitiveTypeInformation,
}
