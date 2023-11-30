use std::{ops::Deref, sync::Arc};

use ecow::EcoString;
use itertools::Itertools;

use crate::{
    ast::{
        self, ArgNames, BinOp, CustomType, Pattern, TypedAssignment, TypedExpr, TypedFunction,
        TypedStatement,
    },
    build::Module,
    error::Result,
    type_::{prelude, ModuleValueConstructor, Type, TypeVar, ValueConstructorVariant},
};

mod encoder;

type TypeIndexCache<T> = im::HashMap<T, encoder::Index<encoder::Type>>;

#[derive(Debug, Default)]
struct Program {
    type_indices: im::HashMap<(EcoString, EcoString), encoder::Index<encoder::Type>>,
    type_equality_indices:
        im::HashMap<encoder::Index<encoder::Type>, encoder::Index<encoder::Function>>,
    function_type_indices_cache: TypeIndexCache<(
        Vec<encoder::Index<encoder::Type>>,
        encoder::Index<encoder::Type>,
    )>,
    tuple_type_indices_cache: TypeIndexCache<Vec<encoder::Index<encoder::Type>>>,
    function_indices: im::HashMap<(EcoString, EcoString), encoder::Index<encoder::Function>>,
}

impl Program {
    fn register_function_index(
        &mut self,
        module: impl Into<EcoString>,
        name: impl Into<EcoString>,
        index: encoder::Index<encoder::Function>,
    ) {
        _ = self
            .function_indices
            .insert((module.into(), name.into()), index);
    }

    fn register_type_index(
        &mut self,
        module: impl Into<EcoString>,
        name: impl Into<EcoString>,
        index: encoder::Index<encoder::Type>,
    ) {
        _ = self
            .type_indices
            .insert((module.into(), name.into()), index);
    }

    fn register_equality_index(
        &mut self,
        type_index: encoder::Index<encoder::Type>,
        function_index: encoder::Index<encoder::Function>,
    ) {
        _ = self
            .type_equality_indices
            .insert(type_index, function_index);
    }

    fn resolve_equality_index(
        &self,
        type_index: encoder::Index<encoder::Type>,
    ) -> encoder::Index<encoder::Function> {
        self.type_equality_indices
            .get(&type_index)
            .copied()
            .expect("Type's equality function should already be declared")
    }

    fn resolve_type(&mut self, encoder: &mut encoder::Module, type_: &Type) -> encoder::ValType {
        match type_ {
            Type::Named { .. } | Type::Fn { .. } => encoder::ValType::Ref(encoder::RefType {
                nullable: true,
                heap_type: encoder::HeapType::Concrete(self.resolve_type_index(encoder, type_)),
            }),
            Type::Var { type_ } => match type_.borrow().deref() {
                TypeVar::Unbound { .. } | TypeVar::Generic { .. } => {
                    encoder::ValType::Ref(encoder::RefType {
                        nullable: true,
                        heap_type: encoder::HeapType::Any,
                    })
                }
                TypeVar::Link { type_ } => self.resolve_type(encoder, type_),
            },
            Type::Tuple { .. } => todo!(),
        }
    }

    fn resolve_type_index(
        &mut self,
        encoder: &mut encoder::Module,
        type_: &Type,
    ) -> encoder::Index<encoder::Type> {
        match type_ {
            Type::Named { module, name, .. } => {
                self.resolve_type_index_by_name(module.clone(), name.clone())
            }
            Type::Fn { args, retrn, .. } => {
                let arg_indices = (args.iter())
                    .map(|arg| self.resolve_type_index(encoder, arg))
                    .collect_vec();

                let return_index = self.resolve_type_index(encoder, &retrn);

                // TODO: This is only required in `or_insert_with`, however,
                //       due to lifetime rules, we cannot call `resolve_type`.
                //       In the future it might be worth revisiting this
                //       in case of any performance issues.
                let fn_type = encoder::Type::Function {
                    params: (args.iter())
                        .map(|arg| self.resolve_type(encoder, &arg))
                        .collect_vec(),
                    results: vec![self.resolve_type(encoder, retrn)],
                };

                *self
                    .function_type_indices_cache
                    .entry((arg_indices, return_index))
                    .or_insert_with(|| {
                        let fn_type_index = encoder.declare_type();

                        encoder.define_type(fn_type_index, fn_type);

                        fn_type_index
                    })
            }
            Type::Var { type_ } => match type_.borrow().deref() {
                TypeVar::Unbound { .. } => unimplemented!(),
                TypeVar::Link { type_ } => self.resolve_type_index(encoder, type_),
                TypeVar::Generic { .. } => unimplemented!(),
            },
            Type::Tuple { elems, .. } => {
                let elem_indices = (elems.iter())
                    .map(|elem| self.resolve_type_index(encoder, elem))
                    .collect_vec();

                let tuple_type = encoder::Type::Struct {
                    fields: (elems.iter())
                        .map(|elem| encoder::FieldType {
                            element_type: encoder::StorageType::Val(
                                self.resolve_type(encoder, &elem),
                            ),
                            mutable: false,
                        })
                        .collect_vec(),
                };

                *self
                    .tuple_type_indices_cache
                    .entry(elem_indices)
                    .or_insert_with(|| {
                        let tuple_type_index = encoder.declare_type();

                        encoder.define_type(tuple_type_index, tuple_type);

                        tuple_type_index
                    })
            }
        }
    }

    fn resolve_type_index_by_name(
        &self,
        module: EcoString,
        name: EcoString,
    ) -> encoder::Index<encoder::Type> {
        self.type_indices
            .get(&(module.clone(), name.clone()))
            .copied()
            .unwrap_or_else(|| panic!("Type index should be defined for '{module}.{name}'",))
    }

    fn resolve_function_index_by_name(
        &self,
        module: EcoString,
        name: EcoString,
    ) -> encoder::Index<encoder::Function> {
        self.function_indices
            .get(&(module.clone(), name.clone()))
            .copied()
            .unwrap_or_else(|| panic!("Function index should be defined for '{module}.{name}'",))
    }
}

pub fn program(modules: &[Module]) -> Result<Vec<u8>> {
    let mut program = Program::default();
    let mut encoder = encoder::Module::default();

    register_prelude(&mut program, &mut encoder);

    for module in modules {
        encode_module_declarations(&mut program, &mut encoder, module)?;
    }

    for module in modules {
        encode_module_definitions(&mut program, &mut encoder, module)?;
    }

    Ok(encoder.finish())
}

fn register_prelude(program: &mut Program, encoder: &mut encoder::Module) {
    register_prelude_int(program, encoder);
    register_prelude_float(program, encoder);
    register_prelude_bool(program, encoder);
    register_prelude_list(program, encoder);
    register_prelude_string(program, encoder);
}

fn register_prelude_int(program: &mut Program, encoder: &mut encoder::Module) {
    let gleam_int_index = encoder.declare_type();
    let gleam_int = encoder::Type::Struct {
        fields: vec![encoder::FieldType {
            element_type: encoder::StorageType::Val(encoder::ValType::I64),
            mutable: false,
        }],
    };

    encoder.define_type(gleam_int_index, gleam_int);
    program.register_type_index("gleam", "Int", gleam_int_index);

    let gleam_int_constructor_type_index = encoder.declare_type();
    let gleam_int_constructor_type = encoder::Type::Function {
        params: vec![encoder::ValType::I64],
        results: vec![encoder::ValType::Ref(encoder::RefType {
            nullable: true,
            heap_type: encoder::HeapType::Concrete(gleam_int_index),
        })],
    };

    encoder.define_type(gleam_int_constructor_type_index, gleam_int_constructor_type);

    let gleam_int_constructor_index = encoder.declare_function();
    let gleam_int_constructor = {
        let int_parameter = EcoString::from("x");

        let mut function = encoder::Function::new(
            gleam_int_constructor_index,
            gleam_int_constructor_type_index,
            EcoString::from("gleam/Int"),
            encoder::FunctionLinkage::Local,
            vec![Some(int_parameter.clone())],
        );

        let param_index = function.get_local_index(int_parameter);

        function.instruction(encoder::Instruction::LocalGet(param_index));
        function.instruction(encoder::Instruction::StructNew(gleam_int_index));
        function.instruction(encoder::Instruction::End);
        function
    };

    encoder.define_function(gleam_int_constructor_index, gleam_int_constructor);
    program.register_function_index("gleam", "Int", gleam_int_constructor_index);

    let is_eq_function_type_index = encoder.declare_type();
    let is_eq_function_type = encoder::Type::Function {
        params: vec![
            encoder::ValType::Ref(encoder::RefType {
                nullable: true,
                heap_type: encoder::HeapType::Concrete(gleam_int_index),
            }),
            encoder::ValType::Ref(encoder::RefType {
                nullable: true,
                heap_type: encoder::HeapType::Concrete(gleam_int_index),
            }),
        ],
        results: vec![encoder::ValType::I32],
    };

    encoder.define_type(is_eq_function_type_index, is_eq_function_type);

    let is_eq_function_index = encoder.declare_function();
    let is_eq_function = {
        let lhs_parameter = EcoString::from("lhs");
        let rhs_parameter = EcoString::from("rhs");

        let mut function = encoder::Function::new(
            is_eq_function_index,
            is_eq_function_type_index,
            EcoString::from("gleam/Int$eq"),
            encoder::FunctionLinkage::Export,
            vec![Some(lhs_parameter.clone()), Some(rhs_parameter.clone())],
        );

        let lhs_index = function.get_local_index(lhs_parameter);
        let rhs_index = function.get_local_index(rhs_parameter);

        function.instruction(encoder::Instruction::LocalGet(lhs_index));
        function.instruction(encoder::Instruction::StructGet(gleam_int_index, 0));
        function.instruction(encoder::Instruction::LocalGet(rhs_index));
        function.instruction(encoder::Instruction::StructGet(gleam_int_index, 0));
        function.instruction(encoder::Instruction::I64Eq);
        function.instruction(encoder::Instruction::End);
        function
    };

    encoder.define_function(is_eq_function_index, is_eq_function);
    program.register_equality_index(gleam_int_index, is_eq_function_index);

    let to_int_function_type_index = encoder.declare_type();
    let to_int_function_type = encoder::Type::Function {
        params: vec![encoder::ValType::Ref(encoder::RefType {
            nullable: true,
            heap_type: encoder::HeapType::Concrete(gleam_int_index),
        })],
        results: vec![encoder::ValType::I64],
    };

    encoder.define_type(to_int_function_type_index, to_int_function_type);

    let to_int_function_index = encoder.declare_function();
    let to_int_function = {
        let int_parameter = EcoString::from("x");

        let mut function = encoder::Function::new(
            to_int_function_index,
            to_int_function_type_index,
            EcoString::from("gleam/Int$to_int"),
            encoder::FunctionLinkage::Export,
            vec![Some(int_parameter.clone())],
        );

        let param_index = function.get_local_index(int_parameter);

        function.instruction(encoder::Instruction::LocalGet(param_index));
        function.instruction(encoder::Instruction::StructGet(gleam_int_index, 0));
        function.instruction(encoder::Instruction::End);
        function
    };

    encoder.define_function(to_int_function_index, to_int_function);
}

fn register_prelude_float(program: &mut Program, encoder: &mut encoder::Module) {
    let gleam_float_index = encoder.declare_type();
    let gleam_float = encoder::Type::Struct {
        fields: vec![encoder::FieldType {
            element_type: encoder::StorageType::Val(encoder::ValType::F64),
            mutable: false,
        }],
    };

    encoder.define_type(gleam_float_index, gleam_float);
    program.register_type_index("gleam", "Float", gleam_float_index);

    let gleam_float_constructor_type_index = encoder.declare_type();
    let gleam_float_constructor_type = encoder::Type::Function {
        params: vec![encoder::ValType::F64],
        results: vec![encoder::ValType::Ref(encoder::RefType {
            nullable: true,
            heap_type: encoder::HeapType::Concrete(gleam_float_index),
        })],
    };

    encoder.define_type(
        gleam_float_constructor_type_index,
        gleam_float_constructor_type,
    );

    let float_parameter = EcoString::from("x");

    let gleam_float_constructor_index = encoder.declare_function();
    let gleam_float_constructor = {
        let mut function = encoder::Function::new(
            gleam_float_constructor_index,
            gleam_float_constructor_type_index,
            EcoString::from("gleam/Float"),
            encoder::FunctionLinkage::Local,
            vec![Some(float_parameter.clone())],
        );

        let param_index = function.get_local_index(float_parameter);

        function.instruction(encoder::Instruction::LocalGet(param_index));
        function.instruction(encoder::Instruction::StructNew(gleam_float_index));
        function.instruction(encoder::Instruction::End);
        function
    };

    encoder.define_function(gleam_float_constructor_index, gleam_float_constructor);
    program.register_function_index("gleam", "Float", gleam_float_constructor_index);

    let is_eq_function_type_index = encoder.declare_type();
    let is_eq_function_type = encoder::Type::Function {
        params: vec![
            encoder::ValType::Ref(encoder::RefType {
                nullable: true,
                heap_type: encoder::HeapType::Concrete(gleam_float_index),
            }),
            encoder::ValType::Ref(encoder::RefType {
                nullable: true,
                heap_type: encoder::HeapType::Concrete(gleam_float_index),
            }),
        ],
        results: vec![encoder::ValType::I32],
    };

    encoder.define_type(is_eq_function_type_index, is_eq_function_type);

    let is_eq_function_index = encoder.declare_function();
    let is_eq_function = {
        let lhs_parameter = EcoString::from("lhs");
        let rhs_parameter = EcoString::from("rhs");

        let mut function = encoder::Function::new(
            is_eq_function_index,
            is_eq_function_type_index,
            EcoString::from("gleam/Float$eq"),
            encoder::FunctionLinkage::Export,
            vec![Some(lhs_parameter.clone()), Some(rhs_parameter.clone())],
        );

        let lhs_index = function.get_local_index(lhs_parameter);
        let rhs_index = function.get_local_index(rhs_parameter);

        function.instruction(encoder::Instruction::LocalGet(lhs_index));
        function.instruction(encoder::Instruction::StructGet(gleam_float_index, 0));
        function.instruction(encoder::Instruction::LocalGet(rhs_index));
        function.instruction(encoder::Instruction::StructGet(gleam_float_index, 0));
        function.instruction(encoder::Instruction::F64Eq);
        function.instruction(encoder::Instruction::End);
        function
    };

    encoder.define_function(is_eq_function_index, is_eq_function);
    program.register_equality_index(gleam_float_index, is_eq_function_index);

    let to_float_function_type_index = encoder.declare_type();
    let to_float_function_type = encoder::Type::Function {
        params: vec![encoder::ValType::Ref(encoder::RefType {
            nullable: true,
            heap_type: encoder::HeapType::Concrete(gleam_float_index),
        })],
        results: vec![encoder::ValType::F64],
    };

    encoder.define_type(to_float_function_type_index, to_float_function_type);

    let to_float_function_index = encoder.declare_function();
    let to_float_function = {
        let int_parameter = EcoString::from("x");

        let mut function = encoder::Function::new(
            to_float_function_index,
            to_float_function_type_index,
            EcoString::from("gleam/Float$to_float"),
            encoder::FunctionLinkage::Export,
            vec![Some(int_parameter.clone())],
        );

        let param_index = function.get_local_index(int_parameter);

        function.instruction(encoder::Instruction::LocalGet(param_index));
        function.instruction(encoder::Instruction::StructGet(gleam_float_index, 0));
        function.instruction(encoder::Instruction::End);
        function
    };

    encoder.define_function(to_float_function_index, to_float_function);
}

fn register_prelude_bool(program: &mut Program, encoder: &mut encoder::Module) {
    let gleam_bool_index = encoder.declare_type();
    let gleam_bool = encoder::Type::Struct {
        fields: vec![encoder::FieldType {
            element_type: encoder::StorageType::Val(encoder::ValType::I32),
            mutable: false,
        }],
    };

    encoder.define_type(gleam_bool_index, gleam_bool);
    program.register_type_index("gleam", "Bool", gleam_bool_index);

    let gleam_bool_constructor_type_index = encoder.declare_type();
    let gleam_bool_constructor_type = encoder::Type::Function {
        params: vec![],
        results: vec![encoder::ValType::Ref(encoder::RefType {
            nullable: true,
            heap_type: encoder::HeapType::Concrete(gleam_bool_index),
        })],
    };

    encoder.define_type(
        gleam_bool_constructor_type_index,
        gleam_bool_constructor_type,
    );

    let gleam_bool_true_constructor_index = encoder.declare_function();
    let gleam_bool_true_constructor = {
        let mut function = encoder::Function::new(
            gleam_bool_true_constructor_index,
            gleam_bool_constructor_type_index,
            EcoString::from("gleam/True"),
            encoder::FunctionLinkage::Local,
            vec![],
        );

        function.instruction(encoder::Instruction::I32Const(1));
        function.instruction(encoder::Instruction::StructNew(gleam_bool_index));
        function.instruction(encoder::Instruction::End);
        function
    };

    program.register_function_index("gleam", "True", gleam_bool_true_constructor_index);
    encoder.define_function(
        gleam_bool_true_constructor_index,
        gleam_bool_true_constructor,
    );

    let gleam_bool_false_constructor_index = encoder.declare_function();
    let gleam_bool_false_constructor = {
        let mut function = encoder::Function::new(
            gleam_bool_false_constructor_index,
            gleam_bool_constructor_type_index,
            EcoString::from("gleam/False"),
            encoder::FunctionLinkage::Local,
            vec![],
        );

        function.instruction(encoder::Instruction::I32Const(0));
        function.instruction(encoder::Instruction::StructNew(gleam_bool_index));
        function.instruction(encoder::Instruction::End);
        function
    };

    program.register_function_index("gleam", "False", gleam_bool_false_constructor_index);
    encoder.define_function(
        gleam_bool_false_constructor_index,
        gleam_bool_false_constructor,
    );

    let is_eq_function_type_index = encoder.declare_type();
    let is_eq_function_type = encoder::Type::Function {
        params: vec![
            encoder::ValType::Ref(encoder::RefType {
                nullable: true,
                heap_type: encoder::HeapType::Concrete(gleam_bool_index),
            }),
            encoder::ValType::Ref(encoder::RefType {
                nullable: true,
                heap_type: encoder::HeapType::Concrete(gleam_bool_index),
            }),
        ],
        results: vec![encoder::ValType::I32],
    };

    encoder.define_type(is_eq_function_type_index, is_eq_function_type);

    let is_eq_function_index = encoder.declare_function();
    let is_eq_function = {
        let lhs_parameter = EcoString::from("lhs");
        let rhs_parameter = EcoString::from("rhs");

        let mut function = encoder::Function::new(
            is_eq_function_index,
            is_eq_function_type_index,
            EcoString::from("gleam/Bool$eq"),
            encoder::FunctionLinkage::Export,
            vec![Some(lhs_parameter.clone()), Some(rhs_parameter.clone())],
        );

        let lhs_index = function.get_local_index(lhs_parameter);
        let rhs_index = function.get_local_index(rhs_parameter);

        function.instruction(encoder::Instruction::LocalGet(lhs_index));
        function.instruction(encoder::Instruction::StructGet(gleam_bool_index, 0));
        function.instruction(encoder::Instruction::LocalGet(rhs_index));
        function.instruction(encoder::Instruction::StructGet(gleam_bool_index, 0));
        function.instruction(encoder::Instruction::I32Eq);
        function.instruction(encoder::Instruction::End);
        function
    };

    encoder.define_function(is_eq_function_index, is_eq_function);
    program.register_equality_index(gleam_bool_index, is_eq_function_index);

    let to_bool_function_type_index = encoder.declare_type();
    let to_bool_function_type = encoder::Type::Function {
        params: vec![encoder::ValType::Ref(encoder::RefType {
            nullable: true,
            heap_type: encoder::HeapType::Concrete(gleam_bool_index),
        })],
        results: vec![encoder::ValType::I32],
    };

    encoder.define_type(to_bool_function_type_index, to_bool_function_type);

    let to_bool_function_index = encoder.declare_function();
    let to_bool_function = {
        let int_parameter = EcoString::from("x");

        let mut function = encoder::Function::new(
            to_bool_function_index,
            to_bool_function_type_index,
            EcoString::from("gleam/Bool$to_bool"),
            encoder::FunctionLinkage::Export,
            vec![Some(int_parameter.clone())],
        );

        let param_index = function.get_local_index(int_parameter);

        function.instruction(encoder::Instruction::LocalGet(param_index));
        function.instruction(encoder::Instruction::StructGet(gleam_bool_index, 0));
        function.instruction(encoder::Instruction::End);
        function
    };

    encoder.define_function(to_bool_function_index, to_bool_function);
}

fn register_prelude_list(program: &mut Program, encoder: &mut encoder::Module) {
    let gleam_list_index = encoder.declare_type();
    let gleam_list = encoder::Type::Struct {
        fields: vec![
            encoder::FieldType {
                element_type: encoder::StorageType::Val(encoder::ValType::Ref(encoder::RefType {
                    nullable: true,
                    heap_type: encoder::HeapType::Concrete(gleam_list_index),
                })),
                mutable: false,
            },
            encoder::FieldType {
                element_type: encoder::StorageType::Val(encoder::ValType::Ref(encoder::RefType {
                    nullable: true,
                    heap_type: encoder::HeapType::Any,
                })),
                mutable: false,
            },
        ],
    };

    program.register_type_index("gleam", "List", gleam_list_index);
    encoder.define_type(gleam_list_index, gleam_list);
}

fn register_prelude_string(program: &mut Program, encoder: &mut encoder::Module) {
    let gleam_string_index = encoder.declare_type();
    let gleam_string = encoder::Type::Array {
        element_type: encoder::StorageType::I8,
        mutable: true,
    };

    program.register_type_index("gleam", "String", gleam_string_index);
    encoder.define_type(gleam_string_index, gleam_string);
}

fn encode_module_declarations(
    program: &mut Program,
    encoder: &mut encoder::Module,
    module: &Module,
) -> Result<()> {
    for custom_type in get_custom_type_definitions(module) {
        encode_custom_type_declaration(program, encoder, module, custom_type)?;
    }

    for function in get_function_definitions(module) {
        encode_function_declaration(program, encoder, module, function)?;
    }

    Ok(())
}

fn encode_custom_type_declaration(
    program: &mut Program,
    encoder: &mut encoder::Module,
    module: &Module,
    custom_type: &CustomType<Arc<Type>>,
) -> Result<()> {
    let parent_type_index = encoder.declare_type();

    program.register_type_index(
        module.ast.name.clone(),
        custom_type.name.clone(),
        parent_type_index,
    );

    let equality_function_index = encoder.declare_function();

    program.register_equality_index(parent_type_index, equality_function_index);

    for constructor in &custom_type.constructors {
        let constructor_type_index = encoder.declare_type();

        program.register_type_index(
            module.ast.name.clone(),
            EcoString::from(format!("{}.{}", custom_type.name, constructor.name)),
            constructor_type_index,
        );
    }

    Ok(())
}

fn encode_function_declaration(
    program: &mut Program,
    encoder: &mut encoder::Module,
    module: &Module,
    function: &TypedFunction,
) -> Result<()> {
    let function_index = encoder.declare_function();

    program.register_function_index(
        module.ast.name.clone(),
        function.name.clone(),
        function_index,
    );

    Ok(())
}

fn encode_module_definitions(
    program: &mut Program,
    encoder: &mut encoder::Module,
    module: &Module,
) -> Result<()> {
    for custom_type in get_custom_type_definitions(module) {
        encode_custom_type_definition(program, encoder, module, custom_type)?;
    }

    for function in get_function_definitions(module) {
        encode_function_definition(program, encoder, module, function)?;
    }

    Ok(())
}

fn encode_custom_type_definition(
    program: &mut Program,
    encoder: &mut encoder::Module,
    module: &Module,
    custom_type: &CustomType<Arc<Type>>,
) -> Result<()> {
    let tag_field = encoder::FieldType {
        element_type: encoder::StorageType::Val(encoder::ValType::I32),
        mutable: false,
    };

    let common_fields = {
        let mut common_fields = vec![tag_field.clone()];

        let min_field_count =
            (custom_type.constructors.iter()).fold(usize::MAX, |min_field_count, constructor| {
                std::cmp::min(min_field_count, constructor.arguments.len())
            });

        for field_index in 0..min_field_count {
            let first_constructor = &custom_type.constructors[0];
            let first_constructor_field = &first_constructor.arguments[field_index];

            let all_fields_match = (custom_type.constructors.iter()).all(|constructor| {
                let field = &constructor.arguments[field_index];

                field.label == first_constructor_field.label
                    && field.type_ == first_constructor_field.type_
            });

            let field_type = if all_fields_match {
                encoder::FieldType {
                    element_type: encoder::StorageType::Val(
                        program.resolve_type(encoder, &first_constructor_field.type_),
                    ),
                    mutable: false,
                }
            } else {
                // If the field doesn't match, we set it to any. This is required
                // as the shared field may come *after* a non-shared field. Wasm
                // requires the shapes to match when we cast, so this satisfies
                // the runtime.
                encoder::FieldType {
                    element_type: encoder::StorageType::Val(encoder::ValType::Ref(
                        encoder::RefType {
                            nullable: true,
                            heap_type: encoder::HeapType::Any,
                        },
                    )),
                    mutable: false,
                }
            };

            common_fields.push(field_type);
        }

        common_fields
    };

    let parent_type_index =
        program.resolve_type_index_by_name(module.ast.name.clone(), custom_type.name.clone());

    let parent_type = encoder::Type::SubType(encoder::SubType {
        is_final: false,
        supertype_idx: None,
        structural_type: encoder::CompositeType::Struct(encoder::StructType {
            fields: common_fields,
        }),
    });

    encoder.define_type(parent_type_index, parent_type);

    for (constructor_index, constructor) in custom_type.constructors.iter().enumerate() {
        let mut fields = vec![tag_field.clone()];

        fields.extend(constructor.arguments.iter().map(|argument| {
            let type_ = program.resolve_type(encoder, &argument.type_);

            encoder::FieldType {
                element_type: encoder::StorageType::Val(type_),
                mutable: false,
            }
        }));

        let constructor_type_index = program.resolve_type_index_by_name(
            module.ast.name.clone(),
            EcoString::from(format!("{}.{}", custom_type.name, constructor.name)),
        );

        let constructor_type = encoder::Type::SubType(encoder::SubType {
            is_final: true,
            supertype_idx: Some(parent_type_index),
            structural_type: encoder::CompositeType::Struct(encoder::StructType { fields }),
        });

        encoder.define_type(constructor_type_index, constructor_type);

        let constructor_function_type_index = encoder.declare_type();
        let constructor_function_type = encoder::Type::Function {
            params: (constructor.arguments.iter())
                .map(|argument| program.resolve_type(encoder, &argument.type_))
                .collect_vec(),
            results: vec![encoder::ValType::Ref(encoder::RefType {
                nullable: true,
                heap_type: encoder::HeapType::Concrete(parent_type_index),
            })],
        };

        encoder.define_type(constructor_function_type_index, constructor_function_type);

        let constructor_function_index = encoder.declare_function();
        let constructor_function = {
            let argument_names = (0..constructor.arguments.len())
                .map(|index| EcoString::from(format!("{index}")))
                .collect_vec();

            let arguments = (argument_names.iter())
                .map(|name| Some(name.clone()))
                .collect_vec();

            let mut function = encoder::Function::new(
                constructor_function_index,
                constructor_function_type_index,
                EcoString::from(format!("{}/{}", module.ast.name, constructor.name)),
                encoder::FunctionLinkage::Export,
                arguments,
            );

            let constructor_index =
                i32::try_from(constructor_index).expect("Too many constructors");

            function.instruction(encoder::Instruction::I32Const(constructor_index));

            for argument in argument_names {
                let argument_index = function.get_local_index(argument);

                function.instruction(encoder::Instruction::LocalGet(argument_index));
            }

            function.instruction(encoder::Instruction::StructNew(constructor_type_index));
            function.instruction(encoder::Instruction::End);
            function
        };

        encoder.define_function(constructor_function_index, constructor_function);
        program.register_function_index(
            module.ast.name.clone(),
            constructor.name.clone(),
            constructor_function_index,
        );
    }

    let equality_function_type_index = encoder.declare_type();
    let equality_function_type = encoder::Type::Function {
        params: vec![
            encoder::ValType::Ref(encoder::RefType {
                nullable: true,
                heap_type: encoder::HeapType::Concrete(parent_type_index),
            }),
            encoder::ValType::Ref(encoder::RefType {
                nullable: true,
                heap_type: encoder::HeapType::Concrete(parent_type_index),
            }),
        ],
        results: vec![encoder::ValType::I32],
    };

    encoder.define_type(equality_function_type_index, equality_function_type);

    let equality_function_index = program.resolve_equality_index(parent_type_index);
    let equality_function = {
        let lhs_parameter = EcoString::from("lhs");
        let rhs_parameter = EcoString::from("rhs");

        let mut function = encoder::Function::new(
            equality_function_index,
            equality_function_type_index,
            EcoString::from(format!("{}/{}$eq", module.ast.name, custom_type.name)),
            encoder::FunctionLinkage::Export,
            vec![Some(lhs_parameter.clone()), Some(rhs_parameter.clone())],
        );

        let lhs_index = function.get_local_index(lhs_parameter);
        let rhs_index = function.get_local_index(rhs_parameter);

        {
            // If the given arguments are not the same variant,
            // then they are not equal.

            function.instruction(encoder::Instruction::LocalGet(lhs_index));
            function.instruction(encoder::Instruction::StructGet(parent_type_index, 0));

            function.instruction(encoder::Instruction::LocalGet(rhs_index));
            function.instruction(encoder::Instruction::StructGet(parent_type_index, 0));

            function.instruction(encoder::Instruction::I32Ne);
            function.instruction(encoder::Instruction::If(encoder::BlockType::Empty));
            function.instruction(encoder::Instruction::I32Const(0));
            function.instruction(encoder::Instruction::Return);
            function.instruction(encoder::Instruction::End);
        }

        // Create the blocks for the constructors.
        for _ in &custom_type.constructors {
            function.instruction(encoder::Instruction::Block(encoder::BlockType::Empty));
        }

        {
            // We want to branch based on which variant the type is
            function.instruction(encoder::Instruction::Block(encoder::BlockType::Empty));
            function.instruction(encoder::Instruction::LocalGet(lhs_index));
            function.instruction(encoder::Instruction::StructGet(parent_type_index, 0));
            function.instruction(encoder::Instruction::BrTable(
                (0..custom_type.constructors.len() as u32).collect(),
                custom_type.constructors.len() as u32,
            ));
            function.instruction(encoder::Instruction::End);
        }

        for constructor in custom_type.constructors.iter().rev() {
            let constructor_type_index = program.resolve_type_index_by_name(
                module.ast.name.clone(),
                EcoString::from(format!("{}.{}", custom_type.name, constructor.name)),
            );

            let constructor_ref = encoder::ValType::Ref(encoder::RefType {
                nullable: true,
                heap_type: encoder::HeapType::Concrete(constructor_type_index),
            });

            let lhs_casted_index = function.declare_local("lhs$".into(), constructor_ref.clone());
            let rhs_casted_index = function.declare_local("rhs$".into(), constructor_ref);

            function.instruction(encoder::Instruction::LocalGet(lhs_index));
            function.instruction(encoder::Instruction::RefCast(encoder::RefType {
                nullable: true,
                heap_type: encoder::HeapType::Concrete(constructor_type_index),
            }));
            function.instruction(encoder::Instruction::LocalSet(lhs_casted_index));

            function.instruction(encoder::Instruction::LocalGet(rhs_index));
            function.instruction(encoder::Instruction::RefCast(encoder::RefType {
                nullable: true,
                heap_type: encoder::HeapType::Concrete(constructor_type_index),
            }));
            function.instruction(encoder::Instruction::LocalSet(rhs_casted_index));

            for (argument_index, argument) in constructor.arguments.iter().enumerate() {
                let argument_type_index = program.resolve_type_index(encoder, &argument.type_);
                let field_index = (argument_index + 1) as u32;

                function.instruction(encoder::Instruction::LocalGet(lhs_casted_index));
                function.instruction(encoder::Instruction::StructGet(
                    constructor_type_index,
                    field_index,
                ));

                function.instruction(encoder::Instruction::LocalGet(rhs_casted_index));
                function.instruction(encoder::Instruction::StructGet(
                    constructor_type_index,
                    field_index,
                ));

                let equality_index = program.resolve_equality_index(argument_type_index);
                function.instruction(encoder::Instruction::Call(equality_index));

                function.instruction(encoder::Instruction::I32Eqz);
                function.instruction(encoder::Instruction::If(encoder::BlockType::Empty));
                function.instruction(encoder::Instruction::I32Const(0));
                function.instruction(encoder::Instruction::Return);
                function.instruction(encoder::Instruction::End);
            }

            function.instruction(encoder::Instruction::I32Const(1));
            function.instruction(encoder::Instruction::Return);
            function.instruction(encoder::Instruction::End);
        }

        function.instruction(encoder::Instruction::I32Const(1));

        function.instruction(encoder::Instruction::End);
        function
    };

    encoder.define_function(equality_function_index, equality_function);

    Ok(())
}

fn encode_function_definition(
    program: &mut Program,
    encoder: &mut encoder::Module,
    module: &Module,
    the_function: &TypedFunction,
) -> Result<()> {
    let function_type_index = encoder.declare_type();
    let function_type = encoder::Type::Function {
        params: the_function
            .arguments
            .iter()
            .map(|param| program.resolve_type(encoder, &param.type_))
            .collect_vec(),
        results: vec![program.resolve_type(encoder, &the_function.return_type)],
    };

    encoder.define_type(function_type_index, function_type);

    let function_index =
        program.resolve_function_index_by_name(module.ast.name.clone(), the_function.name.clone());

    let mut function = encoder::Function::new(
        function_index,
        function_type_index,
        EcoString::from(format!("{}/{}", module.ast.name, the_function.name)),
        encoder::FunctionLinkage::Export,
        (the_function.arguments.iter())
            .map(|argument| match &argument.names {
                ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => None,
                ArgNames::Named { name } | ArgNames::NamedLabelled { name, .. } => {
                    Some(name.clone())
                }
            })
            .collect_vec(),
    );

    for (statement_index, statement) in the_function.body.iter().enumerate() {
        let keep_or_drop = if statement_index == the_function.body.len() - 1 {
            KeepOrDrop::Keep
        } else {
            KeepOrDrop::Drop
        };

        encode_statement(
            program,
            encoder,
            module,
            &mut function,
            statement,
            keep_or_drop,
        )?;
    }

    function.instruction(encoder::Instruction::End);

    encoder.define_function(function_index, function);

    Ok(())
}

enum KeepOrDrop {
    Keep,
    Drop,
}

fn encode_statement(
    program: &mut Program,
    encoder: &mut encoder::Module,
    module: &Module,
    function: &mut encoder::Function,
    statement: &TypedStatement,
    keep_or_drop: KeepOrDrop,
) -> Result<()> {
    match statement {
        TypedStatement::Expression(expression) => {
            encode_expression(program, encoder, module, function, expression)?;
        }
        TypedStatement::Assignment(assignment) => {
            encode_assignment(program, encoder, module, function, assignment)?;
        }
        TypedStatement::Use(_) => todo!(),
    }

    match keep_or_drop {
        KeepOrDrop::Keep => {}
        KeepOrDrop::Drop => {
            function.instruction(encoder::Instruction::Drop);
        }
    }

    Ok(())
}

fn encode_assignment(
    program: &mut Program,
    encoder: &mut encoder::Module,
    module: &Module,
    function: &mut encoder::Function,
    assignment: &TypedAssignment,
) -> Result<()> {
    match &assignment.pattern {
        Pattern::Int { .. } => todo!(),
        Pattern::Float { .. } => todo!(),
        Pattern::String { .. } => todo!(),
        Pattern::Variable { name, .. } => {
            let local_type = program.resolve_type(encoder, &assignment.value.type_());
            let local_index = function.declare_local(name.clone(), local_type.clone());

            encode_expression(program, encoder, module, function, &assignment.value)?;

            function.instruction(encoder::Instruction::LocalTee(local_index));
        }
        Pattern::VarUsage { .. } => todo!(),
        Pattern::Assign { .. } => todo!(),
        Pattern::Discard { .. } => {
            encode_expression(program, encoder, module, function, &assignment.value)?;
        }
        Pattern::List { .. } => todo!(),
        Pattern::Constructor { .. } => todo!(),
        Pattern::Tuple { .. } => todo!(),
        Pattern::BitArray { .. } => todo!(),
        Pattern::StringPrefix { .. } => todo!(),
    }

    Ok(())
}

fn encode_expression(
    program: &mut Program,
    encoder: &mut encoder::Module,
    module: &Module,
    function: &mut encoder::Function,
    expression: &TypedExpr,
) -> Result<()> {
    match expression {
        TypedExpr::Int { value, .. } => {
            let gleam_int_constructor_index =
                program.resolve_function_index_by_name("gleam".into(), "Int".into());

            let value = value.parse::<i64>().expect("Value unsupported");

            function.instruction(encoder::Instruction::I64Const(value));
            function.instruction(encoder::Instruction::Call(gleam_int_constructor_index));
        }
        TypedExpr::Float { value, .. } => {
            let gleam_float_constructor_index =
                program.resolve_function_index_by_name("gleam".into(), "Float".into());

            let value = value.parse::<f64>().expect("Value unsupported");

            function.instruction(encoder::Instruction::F64Const(value));
            function.instruction(encoder::Instruction::Call(gleam_float_constructor_index));
        }
        TypedExpr::String { value, .. } => {
            let string_type_index = program.resolve_type_index(encoder, &expression.type_());

            let string_data_index = encoder.declare_data();
            let string_data = encoder::Data::new(value.bytes().collect_vec());

            encoder.define_data(string_data_index, string_data);

            let string_length = i32::try_from(value.as_bytes().len()).expect("String too long");

            function.instruction(encoder::Instruction::I32Const(0));
            function.instruction(encoder::Instruction::I32Const(string_length));
            function.instruction(encoder::Instruction::ArrayNewData(
                string_type_index,
                string_data_index,
            ));
        }
        TypedExpr::Block { statements, .. } => {
            let block_type = program.resolve_type(encoder, &expression.type_());

            function.instruction(encoder::Instruction::Block(encoder::BlockType::Result(
                block_type,
            )));

            for (statement_index, statement) in statements.iter().enumerate() {
                let keep_or_drop = if statement_index == statements.len() - 1 {
                    KeepOrDrop::Keep
                } else {
                    KeepOrDrop::Drop
                };

                encode_statement(program, encoder, module, function, statement, keep_or_drop)?;
            }

            function.instruction(encoder::Instruction::End);
        }
        TypedExpr::Pipeline {
            assignments,
            finally,
            ..
        } => {
            let block_type = program.resolve_type(encoder, &expression.type_());

            function.instruction(encoder::Instruction::Block(encoder::BlockType::Result(
                block_type,
            )));

            for assignment in assignments {
                encode_assignment(program, encoder, module, function, assignment)?;
                function.instruction(encoder::Instruction::Drop);
            }

            encode_expression(program, encoder, module, function, finally)?;

            function.instruction(encoder::Instruction::End);
        }
        TypedExpr::Var {
            constructor, name, ..
        } => match &constructor.variant {
            ValueConstructorVariant::LocalVariable { .. } => {
                let local_index = function.get_local_index(name.clone());

                function.instruction(encoder::Instruction::LocalGet(local_index));
            }
            ValueConstructorVariant::ModuleConstant { .. } => todo!(),
            ValueConstructorVariant::LocalConstant { .. } => todo!(),
            ValueConstructorVariant::ModuleFn { module, name, .. } => {
                let function_index =
                    program.resolve_function_index_by_name(module.clone(), name.clone());

                function.instruction(encoder::Instruction::RefFunc(function_index));
            }
            ValueConstructorVariant::Record {
                arity,
                module,
                name,
                ..
            } => {
                let function_index =
                    program.resolve_function_index_by_name(module.clone(), name.clone());

                match arity {
                    0 => function.instruction(encoder::Instruction::Call(function_index)),
                    _ => function.instruction(encoder::Instruction::RefFunc(function_index)),
                }
            }
        },
        TypedExpr::Fn { .. } => todo!(),
        TypedExpr::List { elements, tail, .. } => {
            let list_type_index = program.resolve_type_index_by_name("gleam".into(), "List".into());

            if let Some(tail) = tail {
                encode_expression(program, encoder, module, function, tail)?;
            } else {
                function.instruction(encoder::Instruction::RefNull(encoder::HeapType::Concrete(
                    list_type_index,
                )));
            }

            match &elements.as_slice() {
                [] => {
                    function.instruction(encoder::Instruction::RefNull(encoder::HeapType::Any));
                    function.instruction(encoder::Instruction::StructNew(list_type_index));
                }
                _otherwise => {
                    for element in elements.iter().rev() {
                        encode_expression(program, encoder, module, function, element)?;
                        function.instruction(encoder::Instruction::StructNew(list_type_index));
                    }
                }
            }
        }
        TypedExpr::Call { fun, args, .. } => {
            let call_type_index = program.resolve_type_index(encoder, &fun.type_());

            for arg in args {
                encode_expression(program, encoder, module, function, &arg.value)?;
            }

            encode_expression(program, encoder, module, function, &fun)?;

            function.instruction(encoder::Instruction::CallRef(call_type_index))
        }
        TypedExpr::BinOp {
            name, left, right, ..
        } => match name {
            BinOp::And => {
                let gleam_bool_type_index = program.resolve_type_index(encoder, &prelude::bool());
                let gleam_bool_type = program.resolve_type(encoder, &prelude::bool());

                encode_expression(program, encoder, module, function, left)?;

                function.instruction(encoder::Instruction::StructGet(gleam_bool_type_index, 0));
                function.instruction(encoder::Instruction::I32Const(1));
                function.instruction(encoder::Instruction::I32Eq);

                function.instruction(encoder::Instruction::If(encoder::BlockType::Result(
                    gleam_bool_type,
                )));

                encode_expression(program, encoder, module, function, right)?;

                function.instruction(encoder::Instruction::Else);

                function.instruction(encoder::Instruction::I32Const(0));
                function.instruction(encoder::Instruction::StructNew(gleam_bool_type_index));

                function.instruction(encoder::Instruction::End);
            }
            BinOp::Or => {
                let gleam_bool_type_index = program.resolve_type_index(encoder, &prelude::bool());
                let gleam_bool_type = program.resolve_type(encoder, &prelude::bool());

                encode_expression(program, encoder, module, function, left)?;

                function.instruction(encoder::Instruction::StructGet(gleam_bool_type_index, 0));
                function.instruction(encoder::Instruction::I32Eqz);

                function.instruction(encoder::Instruction::If(encoder::BlockType::Result(
                    gleam_bool_type,
                )));

                encode_expression(program, encoder, module, function, right)?;

                function.instruction(encoder::Instruction::Else);

                function.instruction(encoder::Instruction::I32Const(1));
                function.instruction(encoder::Instruction::StructNew(gleam_bool_type_index));

                function.instruction(encoder::Instruction::End);
            }
            BinOp::Eq => {
                let gleam_bool_type_index = program.resolve_type_index(encoder, &prelude::bool());
                let type_index = program.resolve_type_index(encoder, &left.type_());
                let equality_index = program.resolve_equality_index(type_index);

                encode_expression(program, encoder, module, function, left)?;
                encode_expression(program, encoder, module, function, right)?;

                function.instruction(encoder::Instruction::Call(equality_index));
                function.instruction(encoder::Instruction::StructNew(gleam_bool_type_index));
            }
            BinOp::NotEq => {
                let gleam_bool_type_index = program.resolve_type_index(encoder, &prelude::bool());
                let type_index = program.resolve_type_index(encoder, &left.type_());
                let equality_index = program.resolve_equality_index(type_index);

                encode_expression(program, encoder, module, function, left)?;
                encode_expression(program, encoder, module, function, right)?;

                function.instruction(encoder::Instruction::Call(equality_index));
                function.instruction(encoder::Instruction::I32Eqz);
                function.instruction(encoder::Instruction::StructNew(gleam_bool_type_index));
            }
            BinOp::LtInt | BinOp::LtEqInt | BinOp::GtEqInt | BinOp::GtInt => {
                let gleam_bool_type_index = program.resolve_type_index(encoder, &prelude::bool());
                let gleam_int_type_index = program.resolve_type_index(encoder, &prelude::int());

                encode_expression(program, encoder, module, function, left)?;
                function.instruction(encoder::Instruction::StructGet(gleam_int_type_index, 0));

                encode_expression(program, encoder, module, function, right)?;
                function.instruction(encoder::Instruction::StructGet(gleam_int_type_index, 0));

                match name {
                    BinOp::LtInt => function.instruction(encoder::Instruction::I64LtS),
                    BinOp::LtEqInt => function.instruction(encoder::Instruction::I64LeS),
                    BinOp::GtEqInt => function.instruction(encoder::Instruction::I64GeS),
                    BinOp::GtInt => function.instruction(encoder::Instruction::I64GtS),
                    _ => unreachable!(),
                };

                function.instruction(encoder::Instruction::StructNew(gleam_bool_type_index));
            }
            BinOp::LtFloat | BinOp::LtEqFloat | BinOp::GtEqFloat | BinOp::GtFloat => {
                let gleam_bool_type_index = program.resolve_type_index(encoder, &prelude::bool());
                let gleam_float_type_index = program.resolve_type_index(encoder, &prelude::float());

                encode_expression(program, encoder, module, function, left)?;
                function.instruction(encoder::Instruction::StructGet(gleam_float_type_index, 0));

                encode_expression(program, encoder, module, function, right)?;
                function.instruction(encoder::Instruction::StructGet(gleam_float_type_index, 0));

                match name {
                    BinOp::LtFloat => function.instruction(encoder::Instruction::F64Lt),
                    BinOp::LtEqFloat => function.instruction(encoder::Instruction::F64Le),
                    BinOp::GtEqFloat => function.instruction(encoder::Instruction::F64Ge),
                    BinOp::GtFloat => function.instruction(encoder::Instruction::F64Gt),
                    _ => unreachable!(),
                };

                function.instruction(encoder::Instruction::StructNew(gleam_bool_type_index));
            }
            BinOp::AddInt
            | BinOp::SubInt
            | BinOp::MultInt
            | BinOp::DivInt
            | BinOp::RemainderInt => {
                let gleam_int_constructor_index =
                    program.resolve_function_index_by_name("gleam".into(), "Int".into());
                let gleam_int_type_index = program.resolve_type_index(encoder, &prelude::int());

                encode_expression(program, encoder, module, function, left)?;
                function.instruction(encoder::Instruction::StructGet(gleam_int_type_index, 0));

                encode_expression(program, encoder, module, function, right)?;
                function.instruction(encoder::Instruction::StructGet(gleam_int_type_index, 0));

                match name {
                    BinOp::AddInt => function.instruction(encoder::Instruction::I64Add),
                    BinOp::SubInt => function.instruction(encoder::Instruction::I64Sub),
                    BinOp::MultInt => function.instruction(encoder::Instruction::I64Mul),
                    BinOp::DivInt => function.instruction(encoder::Instruction::I64DivS),
                    BinOp::RemainderInt => function.instruction(encoder::Instruction::I64RemS),
                    _ => unreachable!(),
                };

                function.instruction(encoder::Instruction::Call(gleam_int_constructor_index));
            }
            BinOp::AddFloat | BinOp::SubFloat | BinOp::MultFloat | BinOp::DivFloat => {
                let gleam_float_constructor_index =
                    program.resolve_function_index_by_name("gleam".into(), "Float".into());
                let gleam_float_type_index = program.resolve_type_index(encoder, &prelude::float());

                encode_expression(program, encoder, module, function, left)?;
                function.instruction(encoder::Instruction::StructGet(gleam_float_type_index, 0));

                encode_expression(program, encoder, module, function, right)?;
                function.instruction(encoder::Instruction::StructGet(gleam_float_type_index, 0));

                match name {
                    BinOp::AddFloat => function.instruction(encoder::Instruction::F64Add),
                    BinOp::SubFloat => function.instruction(encoder::Instruction::F64Sub),
                    BinOp::MultFloat => function.instruction(encoder::Instruction::F64Mul),
                    BinOp::DivFloat => function.instruction(encoder::Instruction::F64Div),
                    _ => unreachable!(),
                };

                function.instruction(encoder::Instruction::Call(gleam_float_constructor_index));
            }
            BinOp::Concatenate => {
                let gleam_string_type = program.resolve_type_index(encoder, &prelude::string());

                let local_type = encoder::ValType::Ref(encoder::RefType {
                    nullable: true,
                    heap_type: encoder::HeapType::Concrete(gleam_string_type),
                });

                let string = function.declare_local("_string".into(), local_type.clone());
                let lhs = function.declare_local("_lhs".into(), local_type.clone());
                let rhs = function.declare_local("_rhs".into(), local_type);

                encode_expression(program, encoder, module, function, left)?;
                function.instruction(encoder::Instruction::LocalSet(lhs));
                encode_expression(program, encoder, module, function, right)?;
                function.instruction(encoder::Instruction::LocalSet(rhs));

                // Calculate the total size of the new string
                function.instruction(encoder::Instruction::LocalGet(lhs));
                function.instruction(encoder::Instruction::ArrayLen);
                function.instruction(encoder::Instruction::LocalGet(rhs));
                function.instruction(encoder::Instruction::ArrayLen);
                function.instruction(encoder::Instruction::I32Add);

                // Create the new string with enough space for both parts
                function.instruction(encoder::Instruction::ArrayNewDefault(gleam_string_type));

                // Copy the lhs into the new string
                function.instruction(encoder::Instruction::LocalTee(string));
                function.instruction(encoder::Instruction::I32Const(0));
                function.instruction(encoder::Instruction::LocalGet(lhs));
                function.instruction(encoder::Instruction::I32Const(0));
                function.instruction(encoder::Instruction::LocalGet(lhs));
                function.instruction(encoder::Instruction::ArrayLen);
                function.instruction(encoder::Instruction::ArrayCopy(
                    gleam_string_type,
                    gleam_string_type,
                ));

                // Copy the rhs into the new string
                function.instruction(encoder::Instruction::LocalGet(string));
                function.instruction(encoder::Instruction::LocalGet(lhs));
                function.instruction(encoder::Instruction::ArrayLen);
                function.instruction(encoder::Instruction::LocalGet(rhs));
                function.instruction(encoder::Instruction::I32Const(0));
                function.instruction(encoder::Instruction::LocalGet(rhs));
                function.instruction(encoder::Instruction::ArrayLen);
                function.instruction(encoder::Instruction::ArrayCopy(
                    gleam_string_type,
                    gleam_string_type,
                ));

                function.instruction(encoder::Instruction::LocalGet(string));
            }
        },
        TypedExpr::Case {
            typ,
            subjects,
            clauses,
            ..
        } => {
            // Create the subject locals
            let subject_locals = (subjects.iter().enumerate())
                .map(|(idx, subject)| {
                    let subject_type = program.resolve_type(encoder, &subject.type_());
                    function.declare_local(format!("_subject${idx}").into(), subject_type)
                })
                .collect_vec();

            // Assign the locals
            for (subject, local) in subjects.iter().zip(subject_locals.iter()) {
                encode_expression(program, encoder, module, function, subject)?;
                function.instruction(encoder::Instruction::LocalSet(*local));
            }

            let block_type = encoder::BlockType::Result(program.resolve_type(encoder, typ));

            function.instruction(encoder::Instruction::Block(block_type.clone()));

            for clause in clauses {
                // Generate block instruction
                function.instruction(encoder::Instruction::Block(encoder::BlockType::Result(
                    encoder::ValType::I32,
                )));

                match &clause.pattern[0] {
                    Pattern::Int { value, .. } => {
                        let gleam_int_type = program.resolve_type_index(encoder, &prelude::int());

                        let value = value.parse::<i64>().unwrap();

                        function.instruction(encoder::Instruction::LocalGet(subject_locals[0]));
                        function.instruction(encoder::Instruction::StructGet(gleam_int_type, 0));
                        function.instruction(encoder::Instruction::I64Const(value));
                        function.instruction(encoder::Instruction::I64Eq);
                    }
                    Pattern::Float { .. } => todo!(),
                    Pattern::String { .. } => todo!(),
                    Pattern::Variable { .. } => todo!(),
                    Pattern::VarUsage { .. } => todo!(),
                    Pattern::Assign { .. } => todo!(),
                    Pattern::Discard { .. } => {
                        function.instruction(encoder::Instruction::I32Const(1))
                    }
                    Pattern::List { .. } => todo!(),
                    Pattern::Constructor { .. } => todo!(),
                    Pattern::Tuple { .. } => todo!(),
                    Pattern::BitArray { .. } => todo!(),
                    Pattern::StringPrefix { .. } => todo!(),
                }

                function.instruction(encoder::Instruction::End);

                function.instruction(encoder::Instruction::If(block_type.clone()));
                encode_expression(program, encoder, module, function, &clause.then)?;
                function.instruction(encoder::Instruction::Else);
            }

            function.instruction(encoder::Instruction::Unreachable);

            for _clause in clauses {
                function.instruction(encoder::Instruction::End);
            }

            function.instruction(encoder::Instruction::End);
        }
        TypedExpr::RecordAccess { index, record, .. } => {
            let heap_type = program.resolve_type_index(encoder, &record.type_());

            // We need to offset by 1 for the index as the variant tag
            // is stored in the 0th index. TODO: Handle this better?
            let index = u32::try_from(*index + 1).expect("Record is too large");

            encode_expression(program, encoder, module, function, record)?;
            function.instruction(encoder::Instruction::StructGet(heap_type, index));
        }
        TypedExpr::ModuleSelect {
            module_name: module,
            constructor,
            ..
        } => match constructor {
            ModuleValueConstructor::Record { name, .. }
            | ModuleValueConstructor::Fn { name, .. } => {
                let function_index =
                    program.resolve_function_index_by_name(module.clone(), name.clone());

                function.instruction(encoder::Instruction::RefFunc(function_index));
            }
            ModuleValueConstructor::Constant { .. } => todo!(),
        },
        TypedExpr::Tuple { typ, elems, .. } => {
            let type_index = program.resolve_type_index(encoder, &typ);

            for elem in elems {
                encode_expression(program, encoder, module, function, elem)?;
            }

            function.instruction(encoder::Instruction::StructNew(type_index));
        }
        TypedExpr::TupleIndex { tuple, index, .. } => {
            let type_index = program.resolve_type_index(encoder, &tuple.type_());

            encode_expression(program, encoder, module, function, tuple)?;

            let index = u32::try_from(*index).unwrap();

            function.instruction(encoder::Instruction::StructGet(type_index, index));
        }
        TypedExpr::Todo { .. } => todo!(),
        TypedExpr::Panic { .. } => todo!(),
        TypedExpr::BitArray { .. } => unimplemented!(),
        TypedExpr::RecordUpdate { .. } => todo!(),
        TypedExpr::NegateBool { value, .. } => {
            let gleam_bool_type_index = program.resolve_type_index(encoder, &prelude::bool());

            encode_expression(program, encoder, module, function, value)?;
            function.instruction(encoder::Instruction::StructGet(gleam_bool_type_index, 0));
            function.instruction(encoder::Instruction::I32Const(1));
            function.instruction(encoder::Instruction::I32Xor);
            function.instruction(encoder::Instruction::StructNew(gleam_bool_type_index));
        }
        TypedExpr::NegateInt { value, .. } => {
            let gleam_int_type_index = program.resolve_type_index(encoder, &prelude::int());

            function.instruction(encoder::Instruction::I64Const(0));
            encode_expression(program, encoder, module, function, value)?;
            function.instruction(encoder::Instruction::StructGet(gleam_int_type_index, 0));
            function.instruction(encoder::Instruction::I64Sub);
            function.instruction(encoder::Instruction::StructNew(gleam_int_type_index));
        }
    }

    Ok(())
}

fn get_custom_type_definitions(module: &Module) -> impl Iterator<Item = &CustomType<Arc<Type>>> {
    (module.ast.definitions.iter()).filter_map(|definition| match definition {
        ast::Definition::CustomType(custom_type) => Some(custom_type),
        _ => None,
    })
}

fn get_function_definitions(module: &Module) -> impl Iterator<Item = &TypedFunction> {
    (module.ast.definitions.iter()).filter_map(|definition| match definition {
        ast::Definition::Function(function) => Some(function),
        _ => None,
    })
}
