use std::collections::{HashMap, HashSet, VecDeque};

use crate::frontend::{
    mitetype::{FunctionInformation, FunctionTypeInformation, Parameter, StringTypeInformation},
    parser::FunctionDeclaration,
};

use super::{
    mitetype::{
        ArrayTypeInformation, PrimitiveTypeInformation, StructField, StructTypeInformation,
        TypeInformation,
    },
    parser::{Declaration, Program, StructDeclaration, TypeIdentifier},
};

fn identify_structs<'a>(
    program: &'a Program,
    types: &HashMap<String, TypeInformation>,
) -> (HashMap<String, TypeInformation>, Vec<&'a StructDeclaration>) {
    let struct_declarations = program
        .body
        .iter()
        .filter_map(|decl| match decl {
            Declaration::Export(decl) => match decl.as_ref() {
                Declaration::Struct(decl) => Some(decl),
                _ => None,
            },
            Declaration::Struct(decl) => Some(decl),
            _ => None,
        })
        .map(|decl| (decl.id.clone(), decl))
        .collect::<HashMap<String, &StructDeclaration>>();
    let adj_list = struct_declarations
        .iter()
        .map(|(id, decl)| {
            (
                id.clone(),
                decl.fields
                    .iter()
                    .filter_map(|field| match &field.type_annotation {
                        TypeIdentifier::Identifier { name, .. } => Some(name.clone()),
                        _ => None,
                    })
                    .collect::<HashSet<String>>(),
            )
        })
        .collect::<HashMap<String, HashSet<String>>>();

    has_cycle(&adj_list);

    let mut top_sorted_structs = topological_sort(&adj_list);
    top_sorted_structs.reverse();

    let mut types = types.clone();
    for struct_name in top_sorted_structs {
        let decl = struct_declarations.get(&struct_name).unwrap();
        let struct_type = struct_declaration_to_type_information(&types, decl);
        types.insert(struct_name.clone(), TypeInformation::Struct(struct_type));
    }

    (types, struct_declarations.values().cloned().collect())
}

fn has_cycle(adj_list: &HashMap<String, HashSet<String>>) {
    let mut seen = HashSet::new();

    for (node, _) in adj_list.iter() {
        if seen.contains(node) {
            continue;
        }
        let mut stack = vec![node.clone()];
        let mut visited = HashSet::new();
        while let Some(current) = stack.pop() {
            if visited.contains(&current) {
                eprintln!("Cycle detected at node {}", current);
                return;
            }
            visited.insert(current.clone());
            seen.insert(current.clone());
            for neighbor in adj_list.get(&current).unwrap() {
                stack.push(neighbor.clone());
            }
        }
    }
}

fn topological_sort(adj_list: &HashMap<String, HashSet<String>>) -> Vec<String> {
    let mut l = Vec::new();
    let mut has_incoming_edge = adj_list
        .iter()
        .flat_map(|(_, x)| Vec::from_iter(x.iter()))
        .collect::<HashSet<&String>>();
    let mut s = adj_list
        .iter()
        .filter_map(|(node, _)| {
            if !has_incoming_edge.contains(node) {
                Some(node)
            } else {
                None
            }
        })
        .collect::<VecDeque<&String>>();

    while let Some(n) = s.pop_front() {
        l.push(n.clone());
        for m in adj_list.get(n).unwrap() {
            has_incoming_edge.remove(m);
            if !adj_list.iter().any(|(_, x)| x.contains(m)) {
                s.push_back(m);
            }
        }
    }

    l
}

fn struct_declaration_to_type_information(
    types: &HashMap<String, TypeInformation>,
    decl: &StructDeclaration,
) -> StructTypeInformation {
    let mut struct_type = StructTypeInformation {
        name: decl.id.clone(),
        fields: HashMap::new(),
        methods: HashMap::new(),
        sizeof: 0,
        is_ref: false,
    };

    let mut offset = 0;
    for field in &decl.fields {
        // should probably make this not stupid
        let ty = Types(types.clone()).parse_type(&field.type_annotation);
        struct_type.fields.insert(
            field.name.clone(),
            StructField {
                name: field.name.clone(),
                ty: ty.clone(),
                offset,
            },
        );
        offset += ty.sizeof();
    }
    struct_type.sizeof = offset;

    struct_type
}

pub(super) fn build_types(program: &Program) -> HashMap<String, TypeInformation> {
    let mut types = HashMap::new();

    macro_rules! insert_primitive {
        ($name:expr, $sizeof:expr) => {
            types.insert(
                $name.to_string(),
                TypeInformation::Primitive(PrimitiveTypeInformation {
                    name: $name.to_string(),
                    sizeof: $sizeof,
                }),
            );
        };
    }

    insert_primitive!("void", 0);
    insert_primitive!("bool", 4);
    insert_primitive!("i8", 1);
    insert_primitive!("i16", 2);
    insert_primitive!("i32", 4);
    insert_primitive!("i64", 8);
    insert_primitive!("u8", 1);
    insert_primitive!("u16", 2);
    insert_primitive!("u32", 4);
    insert_primitive!("u64", 8);
    insert_primitive!("f32", 4);
    insert_primitive!("f64", 8);
    insert_primitive!("v128", 16);
    insert_primitive!("i8x16", 16);
    insert_primitive!("u8x16", 16);
    insert_primitive!("i16x8", 16);
    insert_primitive!("u16x8", 16);
    insert_primitive!("i32x4", 16);
    insert_primitive!("u32x4", 16);
    insert_primitive!("f32x4", 16);
    insert_primitive!("i64x2", 16);
    insert_primitive!("u64x2", 16);
    insert_primitive!("f64x2", 16);

    types.insert(
        "string".to_string(),
        TypeInformation::String(StringTypeInformation {}),
    );

    let (mut types, declarations) = identify_structs(&program, &types);
    // 🤔
    let typeser = Types(types.clone());

    for StructDeclaration { id, methods, .. } in declarations {
        for FunctionDeclaration {
            name,
            parameters,
            return_type,
            ..
        } in methods
        {
            let struct_type = match types.get_mut(id).unwrap() {
                TypeInformation::Struct(info) => info,
                _ => unreachable!(),
            };

            let func = FunctionTypeInformation {
                name: name.clone(),
                implementation: FunctionInformation {
                    args: parameters
                        .iter()
                        .map(|param| Parameter {
                            name: param.name.clone(),
                            ty: typeser.parse_type(&param.type_annotation),
                        })
                        .collect::<Vec<Parameter>>(),
                    ret: Box::new(typeser.parse_type(return_type)),
                },
                is_ref: false,
            };

            struct_type.methods.insert(name.clone(), func);
        }
    }

    types
}

pub(super) struct Types(pub HashMap<String, TypeInformation>);

impl Types {
    pub fn get(&self, name: &str) -> Option<&TypeInformation> {
        self.0.get(name)
    }

    pub fn get_known(&self, name: &'static str) -> &TypeInformation {
        self.0.get(name).unwrap()
    }

    pub fn add(&mut self, name: String, ty: TypeInformation) {
        self.0.insert(name, ty);
    }

    pub fn parse_type(&self, ty: &TypeIdentifier) -> TypeInformation {
        match ty {
            TypeIdentifier::Identifier { name, .. } => self.0.get(name).unwrap().clone(),
            TypeIdentifier::Array {
                element_type,
                size,
                is_ref,
            } => TypeInformation::Array(ArrayTypeInformation {
                name: if *size > 0 {
                    format!("[{}; {}]", self.parse_type(element_type), size)
                } else {
                    format!("[{}]", self.parse_type(element_type))
                },
                element_type: Box::new(self.parse_type(element_type)),
                length: Some(*size),
                is_ref: *is_ref,
            }),
            TypeIdentifier::Function {
                parameters,
                return_type,
                is_ref,
            } => {
                let args = parameters
                    .iter()
                    .map(|param| Parameter {
                        name: param.name.clone(),
                        ty: self.parse_type(&param.type_annotation),
                    })
                    .collect::<Vec<Parameter>>();
                let ret = Box::new(self.parse_type(return_type));

                let name = format!(
                    "({}) => {}",
                    args.iter()
                        .map(|arg| arg.ty.to_string())
                        .collect::<Vec<String>>()
                        .join(", "),
                    ret.to_string()
                );

                let implementation = FunctionInformation { args, ret };

                TypeInformation::Function(FunctionTypeInformation {
                    name,
                    implementation,
                    is_ref: *is_ref,
                })
            }
        }
    }
}
