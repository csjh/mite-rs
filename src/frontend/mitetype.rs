use std::{collections::HashMap, fmt};

use super::{
    ir::IRExpression,
    parser::{BinaryOperator, Literal, UnaryOperator},
};

#[derive(Debug, Clone)]
pub(super) struct PrimitiveTypeInformation {
    pub name: &'static str,
    pub sizeof: u32,
}

const PTR: PrimitiveTypeInformation = PrimitiveTypeInformation {
    name: "u32",
    sizeof: 4,
};

#[derive(Debug, Clone)]
pub(super) struct ArrayTypeInformation {
    pub name: String,
    pub element_type: Box<TypeInformation>,
    pub length: Option<u32>,
    pub is_ref: bool,
}

#[derive(Debug, Clone)]
pub(super) struct StructField {
    pub name: String,
    pub ty: TypeInformation,
    pub offset: u32,
}

#[derive(Debug, Clone)]
pub(super) struct StructTypeInformation {
    pub name: String,
    pub fields: HashMap<String, StructField>,
    pub methods: HashMap<String, FunctionTypeInformation>,
    pub sizeof: u32,
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

    pub fn sizeof(&self) -> u32 {
        if self.is_ref() {
            return 4;
        } else {
            match self {
                TypeInformation::Primitive(info) => info.sizeof,
                TypeInformation::Array(info) => {
                    assert!(
                        info.length.is_some(),
                        "Array length must be known in non-ref array"
                    );
                    info.element_type.sizeof() * info.length.unwrap()
                }
                TypeInformation::Struct(info) => info.sizeof,
                TypeInformation::String(_) => unreachable!(),
                TypeInformation::Function(_) => 8,
            }
        }
    }

    pub fn is_compatible(&self, other: &TypeInformation) -> bool {
        match (self, other) {
            (TypeInformation::Primitive(a), TypeInformation::Primitive(b)) => a.name == b.name,
            (TypeInformation::Array(a), TypeInformation::Array(b)) => {
                a.element_type.is_compatible(&b.element_type)
            }
            (TypeInformation::Struct(a), TypeInformation::Struct(b)) => a.name == b.name,
            (TypeInformation::String(_), TypeInformation::String(_)) => true,
            (TypeInformation::Function(a), TypeInformation::Function(b)) => {
                a.name == b.name
                    && a.implementation.args.len() == b.implementation.args.len()
                    && a.implementation
                        .args
                        .iter()
                        .zip(b.implementation.args.iter())
                        .all(|(a, b)| a.ty.is_compatible(&b.ty))
                    && a.implementation.ret.is_compatible(&b.implementation.ret)
            }
            _ => false,
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
    // get the value as IR (pointer for structs and arrays, value for locals)
    fn get(&self) -> IRExpression {
        panic!("Getting a value on a non-gettable type {}", self.ty());
    }
    // set the value
    fn set(&mut self, value: &dyn MiteType) -> IRExpression {
        panic!("Setting a value on a non-settable type {}", self.ty());
    }
    // access with . operator
    fn access(&self, key: String) -> &dyn MiteType {
        panic!("Accessing field {} on a non-struct type {}", key, self.ty());
    }
    // access with [] operator
    fn index(&self, index: &dyn MiteType) -> &dyn MiteType {
        panic!("Indexing on a non-array type {}", self.ty());
    }
    // call the value as a function
    fn call(&self, args: Vec<Box<&dyn MiteType>>) -> &dyn MiteType {
        panic!("Calling a non-function type {}", self.ty());
    }
    // get the full size of the value
    fn sizeof(&self) -> IRExpression {
        panic!("Getting the size of a non-sizeable type {}", self.ty());
    }
    // get handler for unary operator
    fn unary_op(&self, op: UnaryOperator) -> Box<dyn FnOnce() -> IRExpression> {
        panic!(
            "Unary operator {} not supported on type {}",
            match op {
                UnaryOperator::Not => "!",
                UnaryOperator::Plus => "+",
                UnaryOperator::Minus => "-",
                UnaryOperator::BitwiseNot => "~",
            },
            self.ty()
        );
    }
    // get handler for binary operator
    fn binary_op(&self, op: BinaryOperator) -> Box<dyn FnOnce(&dyn MiteType) -> IRExpression> {
        panic!(
            "Binary operator {} not supported on type {}",
            match op {
                BinaryOperator::Plus => "+",
                BinaryOperator::Minus => "-",
                BinaryOperator::Star => "*",
                BinaryOperator::Slash => "/",
                BinaryOperator::Remainder => "%",
                BinaryOperator::And => "&",
                BinaryOperator::Or => "|",
                BinaryOperator::Xor => "^",
                BinaryOperator::BitshiftLeft => "<<",
                BinaryOperator::BitshiftRight => ">>",
                BinaryOperator::Equals => "==",
                BinaryOperator::NotEquals => "!=",
                BinaryOperator::LessThan => "<",
                BinaryOperator::LessThanEquals => "<=",
                BinaryOperator::GreaterThan => ">",
                BinaryOperator::GreaterThanEquals => ">=",
            },
            self.ty()
        );
    }

    // get the type information
    fn ty(&self) -> TypeInformation;
}

impl MiteType for IRExpression {
    fn get(&self) -> IRExpression {
        self.clone()
    }

    fn ty(&self) -> TypeInformation {
        self.ty()
    }
}

struct LocalPrimitive {
    ty: PrimitiveTypeInformation,
    var: String,
}

impl MiteType for LocalPrimitive {
    fn get(&self) -> IRExpression {
        IRExpression::LocalGet {
            ty: self.ty(),
            name: self.var.clone(),
        }
    }

    fn set(&mut self, value: &dyn MiteType) -> IRExpression {
        IRExpression::LocalSet {
            ty: self.ty(),
            name: self.var.clone(),
            value: Box::new(value.get()),
        }
    }

    fn sizeof(&self) -> IRExpression {
        IRExpression::Literal {
            ty: PrimitiveTypeInformation {
                name: "u32",
                sizeof: 4,
            },
            value: Literal::U32(self.ty.sizeof),
        }
    }

    fn ty(&self) -> TypeInformation {
        TypeInformation::Primitive(self.ty.clone())
    }
}

struct LinearMemoryPrimitive {
    ty: PrimitiveTypeInformation,
    ptr: IRExpression,
}
