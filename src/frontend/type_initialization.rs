use std::collections::{HashMap, HashSet, VecDeque};

use crate::frontend::{
    ir::{ast_to_ir, Export},
    mitetype::{FunctionInformation, FunctionTypeInformation, Parameter, StringTypeInformation},
    parser::parse,
    tokenizer::tokenize,
};

use super::{
    ir::Options,
    mitetype::{
        ArrayTypeInformation, PrimitiveTypeInformation, StructField, StructTypeInformation,
        TypeInformation,
    },
    parser::{Declaration, Program, StructDeclaration, TypeIdentifier},
};

fn identify_structs<'a>(
    program: &'a Program,
    types: &mut HashMap<String, TypeInformation>,
) -> Vec<&'a StructDeclaration> {
    let struct_declarations = program
        .body
        .iter()
        .filter_map(|decl| match decl {
            Declaration::Struct(decl, _) => Some(decl),
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

    for struct_name in top_sorted_structs.into_iter() {
        let decl = struct_declarations.get(struct_name).unwrap();
        let struct_type = struct_declaration_to_type_information(&types, decl);
        types.insert(struct_name.clone(), TypeInformation::Struct(struct_type));
    }

    struct_declarations.into_values().collect()
}

fn has_cycle(adj_list: &HashMap<String, HashSet<String>>) {
    let mut seen = HashSet::new();

    for (node, _) in adj_list.iter() {
        if seen.contains(node) {
            continue;
        }
        let mut stack = vec![node];
        let mut visited = HashSet::new();
        while let Some(current) = stack.pop() {
            if visited.contains(current) {
                eprintln!("Cycle detected at node {}", current);
                return;
            }
            visited.insert(current);
            seen.insert(current);
            for neighbor in adj_list.get(current).unwrap() {
                stack.push(neighbor);
            }
        }
    }
}

fn topological_sort(adj_list: &HashMap<String, HashSet<String>>) -> Vec<&String> {
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
        l.push(n);
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
        let ty = Types(types.clone()).parse_type(field.type_annotation.clone());

        let before = offset;
        offset += ty.sizeof();

        struct_type.fields.insert(
            field.name.clone(),
            StructField {
                name: field.name.clone(),
                ty,
                offset: before,
            },
        );
    }
    struct_type.sizeof = offset;

    struct_type
}

pub(super) fn build_types(program: &Program, options: Options) -> Types {
    let mut types = HashMap::new();

    macro_rules! insert_primitive {
        ($name:expr, $sizeof:expr) => {
            types.insert(
                $name.to_string(),
                TypeInformation::Primitive(PrimitiveTypeInformation {
                    name: $name,
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

    // todo: this really needs to be cleaned up to prevent imports being
    // parsed twice (here and in ast_to_ir)
    crate::for_each_decl!(program, Import, |(decl, _)| {
        let import_data = (options.resolve_import)(&decl.source);

        if import_data.is_mite {
            let tokens = tokenize(&import_data.source);
            let ast = parse(tokens);
            let mut imported_module = ast_to_ir(ast, options.clone());

            for specifier in decl.specifiers {
                let export = imported_module.exports.remove(&specifier.imported);
                if let Some(export) = export {
                    match export {
                        Export::Struct(info) => {
                            types.insert(specifier.local, TypeInformation::Struct(info));
                        }
                        _ => {}
                    }
                } else {
                    panic!("Imported symbol {} not found", specifier.imported);
                }
            }
        }
    });

    let declarations = identify_structs(&program, &mut types);
    let mut typeser = Types(types);

    for StructDeclaration { id, methods, .. } in declarations {
        for decl in methods {
            let ty = decl.to_type(&typeser);

            let struct_type = match typeser.0.get_mut(id).unwrap() {
                TypeInformation::Struct(info) => info,
                _ => unreachable!(),
            };

            struct_type.methods.insert(decl.name.clone(), ty);
        }
    }

    typeser
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

    pub fn parse_type(&self, ty: TypeIdentifier) -> TypeInformation {
        match ty {
            TypeIdentifier::Identifier { name, .. } => self.0.get(&name).unwrap().clone(),
            TypeIdentifier::Array {
                element_type,
                size,
                is_ref,
            } => {
                let ty = self.parse_type(*element_type);
                TypeInformation::Array(ArrayTypeInformation {
                    name: if size > 0 {
                        format!("[{}; {}]", ty, size)
                    } else {
                        format!("[{}]", ty)
                    },
                    element_type: Box::new(ty),
                    length: Some(size),
                    is_ref,
                })
            }
            TypeIdentifier::Function {
                parameters,
                return_type,
                is_ref,
            } => {
                let args = parameters
                    .iter()
                    .map(|param| param.to_parameter(&self))
                    .collect::<Vec<Parameter>>();
                let ret = Box::new(self.parse_type(*return_type));

                let name = format!(
                    "({}) => {}",
                    args.iter()
                        .map(|arg| arg.ty.to_string())
                        .collect::<Vec<String>>()
                        .join(", "),
                    ret.to_string()
                );

                TypeInformation::Function(FunctionTypeInformation {
                    name,
                    implementation: FunctionInformation { args, ret },
                    is_ref,
                })
            }
        }
    }
}
