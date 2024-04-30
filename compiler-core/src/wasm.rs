use std::sync::Arc;

use ecow::EcoString;
use im::HashSet;
use itertools::Itertools;
use vec1::Vec1;

use crate::{
    ast::{
        Arg, ArgNames, AssignName, BinOp, Clause, ClauseGuard, Constant, CustomType, Definition,
        Pattern, Statement, TypedAssignment, TypedExpr, TypedFunction, TypedPattern,
        TypedStatement,
    },
    build::Module as ModuleAst,
    error::Result,
    type_::{prelude::PreludeType, ModuleValueConstructor, Type, ValueConstructorVariant},
    wasm::program::{
        runtime::{self, string_compare},
        Program,
    },
};

use self::encoder::{
    BlockType, CompositeType, Data, FieldType, Function, FunctionLinkage, HeapType, Index,
    Instruction, Local, Module, RefType, StorageType, StructType, SubType, ValType,
};

mod encoder;
mod program;

pub fn program(modules: &[ModuleAst]) -> Result<Vec<u8>> {
    let mut program = Program::default();
    let mut encoder = Module::default();

    runtime::register(&mut program, &mut encoder);

    for module in modules {
        encode_module_declarations(&mut program, &mut encoder, module)?;
    }

    for module in modules {
        encode_module_definitions(&mut program, &mut encoder, module)?;
    }

    Ok(encoder.finish())
}

fn encode_module_declarations(
    program: &mut Program,
    encoder: &mut Module,
    module: &ModuleAst,
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
    encoder: &mut Module,
    module: &ModuleAst,
    custom_type: &CustomType<Arc<Type>>,
) -> Result<()> {
    let parent_type_index = encoder.declare_type();

    program.register_type_index(
        module.ast.name.clone(),
        custom_type.name.clone(),
        parent_type_index,
    );

    let equality_function_index = encoder.declare_function();

    program.register_type_equality_index(parent_type_index, equality_function_index);

    for (constructor_tag, constructor) in custom_type.constructors.iter().enumerate() {
        let constructor_type_index = encoder.declare_type();
        let constructor_tag = i32::try_from(constructor_tag).expect("Too many constructors");

        program.register_type_variant_tag(
            parent_type_index,
            constructor.name.clone(),
            constructor_tag,
        );

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
    encoder: &mut Module,
    module: &ModuleAst,
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
    encoder: &mut Module,
    module: &ModuleAst,
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
    encoder: &mut Module,
    module: &ModuleAst,
    custom_type: &CustomType<Arc<Type>>,
) -> Result<()> {
    let tag_field = FieldType {
        element_type: StorageType::Val(ValType::I32),
        mutable: false,
    };

    let common_fields = {
        let mut common_fields = vec![tag_field.clone()];

        let min_field_count =
            custom_type
                .constructors
                .iter()
                .fold(usize::MAX, |min_field_count, constructor| {
                    std::cmp::min(min_field_count, constructor.arguments.len())
                });

        for field_index in 0..min_field_count {
            let first_constructor = &custom_type.constructors[0];
            let first_constructor_field = &first_constructor.arguments[field_index];

            let all_fields_match = custom_type.constructors.iter().all(|constructor| {
                let field = &constructor.arguments[field_index];

                field.label == first_constructor_field.label
                    && field.type_ == first_constructor_field.type_
            });

            let field_type = if all_fields_match {
                FieldType {
                    element_type: StorageType::Val(
                        program.resolve_type(encoder, &first_constructor_field.type_),
                    ),
                    mutable: false,
                }
            } else {
                // If the field doesn't match, we set it to any. This is required
                // as the shared field may come *after* a non-shared field. Wasm
                // requires the shapes to match when we cast, so this satisfies
                // the runtime.
                FieldType {
                    element_type: StorageType::Val(ValType::Ref(RefType {
                        nullable: true,
                        heap_type: HeapType::Any,
                    })),
                    mutable: false,
                }
            };

            common_fields.push(field_type);
        }

        common_fields
    };

    let parent_type_index =
        program.resolve_type_index_by_name(module.ast.name.clone(), custom_type.name.clone());

    let parent_type = encoder::Type::SubType(SubType {
        is_final: false,
        supertype_idx: None,
        structural_type: CompositeType::Struct(StructType {
            fields: common_fields,
        }),
    });

    encoder.define_type(parent_type_index, parent_type);

    for constructor in custom_type.constructors.iter() {
        let mut fields = vec![tag_field.clone()];

        fields.extend(constructor.arguments.iter().map(|argument| {
            let type_ = program.resolve_type(encoder, &argument.type_);

            FieldType {
                element_type: StorageType::Val(type_),
                mutable: false,
            }
        }));

        let constructor_type_index = program.resolve_type_index_by_name(
            module.ast.name.clone(),
            EcoString::from(format!("{}.{}", custom_type.name, constructor.name)),
        );

        let constructor_type = encoder::Type::SubType(SubType {
            is_final: true,
            supertype_idx: Some(parent_type_index),
            structural_type: CompositeType::Struct(StructType { fields }),
        });

        encoder.define_type(constructor_type_index, constructor_type);

        let constructor_function_type_index = encoder.declare_type();
        let constructor_function_type = encoder::Type::Function {
            params: {
                let mut params = constructor
                    .arguments
                    .iter()
                    .map(|argument| program.resolve_type(encoder, &argument.type_))
                    .collect_vec();
                params.push(ValType::Ref(RefType {
                    nullable: true,
                    heap_type: HeapType::Struct,
                }));
                params
            },
            results: vec![ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(parent_type_index),
            })],
        };

        encoder.define_type(constructor_function_type_index, constructor_function_type);

        let constructor_function_index = encoder.declare_function();
        let constructor_function = {
            let argument_names = (0..constructor.arguments.len())
                .map(|index| EcoString::from(format!("{index}")))
                .collect_vec();

            let arguments = argument_names
                .iter()
                .map(|name| Some(name.clone()))
                .collect_vec();

            let mut function = Function::new(
                constructor_function_index,
                constructor_function_type_index,
                EcoString::from(format!("{}/{}", module.ast.name, constructor.name)),
                FunctionLinkage::Export,
                {
                    let mut arguments = arguments;
                    arguments.push(None);
                    arguments
                },
            );

            let constructor_tag =
                program.resolve_type_variant_tag(parent_type_index, constructor.name.clone());

            let mut arguments = vec![Instruction::I32Const(constructor_tag)];
            arguments.extend(&mut {
                argument_names
                    .into_iter()
                    .map(|argument| Instruction::LocalGet(function.get_local_index(argument)))
            });

            function.instruction(Instruction::StructNew {
                type_: constructor_type_index,
                args: arguments,
            });
            function.instruction(Instruction::End);
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
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(parent_type_index),
            }),
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(parent_type_index),
            }),
        ],
        results: vec![ValType::I32],
    };

    encoder.define_type(equality_function_type_index, equality_function_type);

    let equality_function_index = program.resolve_equality_index(parent_type_index);
    let equality_function = {
        let lhs_parameter = EcoString::from("lhs");
        let rhs_parameter = EcoString::from("rhs");

        let mut function = Function::new(
            equality_function_index,
            equality_function_type_index,
            EcoString::from(format!("{}/{}$eq", module.ast.name, custom_type.name)),
            FunctionLinkage::Export,
            vec![Some(lhs_parameter.clone()), Some(rhs_parameter.clone())],
        );

        let lhs_index = function.get_local_index(lhs_parameter);
        let rhs_index = function.get_local_index(rhs_parameter);

        // If the given arguments are not the same variant,
        // then they are not equal.
        function.instruction(Instruction::If {
            type_: BlockType::Empty,
            cond: Box::new(Instruction::I32Ne {
                lhs: Box::new(Instruction::StructGet {
                    from: Box::new(Instruction::LocalGet(lhs_index)),
                    type_: parent_type_index,
                    index: 0,
                }),
                rhs: Box::new(Instruction::StructGet {
                    from: Box::new(Instruction::LocalGet(rhs_index)),
                    type_: parent_type_index,
                    index: 0,
                }),
            }),
            then: vec![Instruction::Return(Some(Box::new(Instruction::I32Const(
                0,
            ))))],
            else_: vec![],
        });

        let branch_block = Instruction::Block {
            type_: BlockType::Empty,
            code: vec![
                Instruction::StructGet {
                    from: Box::new(Instruction::LocalGet(lhs_index)),
                    type_: parent_type_index,
                    index: 0,
                },
                Instruction::BrTable(
                    (0..custom_type.constructors.len() as u32).collect(),
                    custom_type.constructors.len() as u32,
                ),
            ],
        };

        let block = custom_type
            .constructors
            .iter()
            .rfold(branch_block, |acc, constructor| {
                let mut code = vec![acc];

                let constructor_type_index = program.resolve_type_index_by_name(
                    module.ast.name.clone(),
                    EcoString::from(format!("{}.{}", custom_type.name, constructor.name)),
                );

                let constructor_ref = ValType::Ref(RefType {
                    nullable: true,
                    heap_type: HeapType::Concrete(constructor_type_index),
                });

                let lhs_casted_index = function.declare_anonymous_local(constructor_ref.clone());
                let rhs_casted_index = function.declare_anonymous_local(constructor_ref);

                code.push(Instruction::LocalSet {
                    local: lhs_casted_index,
                    value: Box::new(Instruction::RefCastNullable {
                        type_: HeapType::Concrete(constructor_type_index),
                        value: Box::new(Instruction::LocalGet(lhs_index)),
                    }),
                });

                code.push(Instruction::LocalSet {
                    local: rhs_casted_index,
                    value: Box::new(Instruction::RefCastNullable {
                        type_: HeapType::Concrete(constructor_type_index),
                        value: Box::new(Instruction::LocalGet(rhs_index)),
                    }),
                });

                for (argument_index, argument) in constructor.arguments.iter().enumerate() {
                    let argument_type_index = program.resolve_type_index(encoder, &argument.type_);
                    let field_index = (argument_index + 1) as u32;

                    code.push(Instruction::If {
                        type_: BlockType::Empty,
                        cond: Box::new(Instruction::I32Eqz(Box::new(Instruction::Call {
                            func: program.resolve_equality_index(argument_type_index),
                            args: vec![
                                Instruction::StructGet {
                                    from: Box::new(Instruction::LocalGet(lhs_casted_index)),
                                    type_: constructor_type_index,
                                    index: field_index,
                                },
                                Instruction::StructGet {
                                    from: Box::new(Instruction::LocalGet(rhs_casted_index)),
                                    type_: constructor_type_index,
                                    index: field_index,
                                },
                            ],
                        }))),
                        then: vec![Instruction::Return(Some(Box::new(Instruction::I32Const(
                            0,
                        ))))],
                        else_: vec![],
                    });
                }

                Instruction::Block {
                    type_: BlockType::Empty,
                    code,
                }
            });

        function.instruction(block);
        function.instruction(Instruction::I32Const(1));
        function.instruction(Instruction::End);
        function
    };

    encoder.define_function(equality_function_index, equality_function);

    Ok(())
}

fn encode_function_definition(
    program: &mut Program,
    encoder: &mut Module,
    module: &ModuleAst,
    the_function: &TypedFunction,
) -> Result<()> {
    let function_type = encoder::Type::Function {
        params: {
            let mut params = the_function
                .arguments
                .iter()
                .map(|param| program.resolve_type(encoder, &param.type_))
                .collect_vec();
            if the_function.name != "main" {
                params.push(ValType::Ref(RefType {
                    nullable: true,
                    heap_type: HeapType::Struct,
                }));
            }
            params
        },
        results: vec![program.resolve_type(encoder, &the_function.return_type)],
    };
    let function_type_index = encoder.declare_type();

    encoder.define_type(function_type_index, function_type);

    let function_index =
        program.resolve_function_index_by_name(module.ast.name.clone(), the_function.name.clone());

    let mut function = Function::new(
        function_index,
        function_type_index,
        EcoString::from(format!("{}/{}", module.ast.name, the_function.name)),
        FunctionLinkage::Export,
        {
            let mut arguments = the_function
                .arguments
                .iter()
                .map(|argument| match &argument.names {
                    ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => None,
                    ArgNames::Named { name } | ArgNames::NamedLabelled { name, .. } => {
                        Some(name.clone())
                    }
                })
                .collect_vec();
            if !(the_function.name == "main" && the_function.publicity.is_public()) {
                arguments.push(None);
            }
            arguments
        },
    );

    for instruction in encode_block(program, encoder, module, &mut function, &the_function.body)? {
        function.instruction(instruction);
    }

    function.instruction(Instruction::End);

    encoder.define_function(function_index, function);

    Ok(())
}

enum KeepOrDrop {
    Keep,
    Drop,
}

fn encode_block(
    program: &mut Program,
    encoder: &mut Module,
    module: &ModuleAst,
    function: &mut Function,
    body: &Vec1<TypedStatement>,
) -> Result<Vec<Instruction>> {
    body.iter()
        .enumerate()
        .map(|(statement_index, statement)| {
            encode_statement(
                program,
                encoder,
                module,
                function,
                statement,
                if statement_index == body.len() - 1 {
                    KeepOrDrop::Keep
                } else {
                    KeepOrDrop::Drop
                },
            )
        })
        .collect()
}

fn encode_statement(
    program: &mut Program,
    encoder: &mut Module,
    module: &ModuleAst,
    function: &mut Function,
    statement: &TypedStatement,
    keep_or_drop: KeepOrDrop,
) -> Result<Instruction> {
    let instruction = match statement {
        Statement::Expression(expression) => {
            encode_expression(program, encoder, module, function, expression)
        }
        Statement::Assignment(assignment) => {
            encode_assignment(program, encoder, module, function, assignment)
        }
        Statement::Use(..) => todo!(),
    }?;

    Ok(match keep_or_drop {
        KeepOrDrop::Keep => instruction,
        KeepOrDrop::Drop => Instruction::Drop(Box::new(instruction)),
    })
}

fn encode_assignment(
    program: &mut Program,
    encoder: &mut Module,
    module: &ModuleAst,
    function: &mut Function,
    assignment: &TypedAssignment,
) -> Result<Instruction> {
    let value_type = program.resolve_type(encoder, &assignment.value.type_());
    let value_local = function.declare_anonymous_local(value_type.clone());

    Ok(Instruction::Block {
        type_: BlockType::Result(value_type),
        code: vec![
            Instruction::LocalSet {
                local: value_local,
                value: Box::new(encode_expression(
                    program,
                    encoder,
                    module,
                    function,
                    &assignment.value,
                )?),
            },
            encode_assignment_pattern(
                program,
                encoder,
                module,
                function,
                value_local,
                &assignment.pattern,
                &assignment.value.type_(),
            )?,
            Instruction::LocalGet(value_local),
        ],
    })
}

fn encode_assignment_pattern(
    program: &mut Program,
    encoder: &mut Module,
    module: &ModuleAst,
    function: &mut Function,
    value_local: Index<Local>,
    pattern: &Pattern<Arc<Type>>,
    type_: &Type,
) -> Result<Instruction> {
    match &pattern {
        Pattern::Int { .. } => todo!(),
        Pattern::Float { .. } => todo!(),
        Pattern::String { .. } => todo!(),
        Pattern::Variable { name, .. } => {
            let local_type = program.resolve_type(encoder, type_);
            let local_index = function.declare_local(name.clone(), local_type);

            Ok(Instruction::LocalSet {
                local: local_index,
                value: Box::new(Instruction::LocalGet(value_local)),
            })
        }
        Pattern::VarUsage { .. } => todo!(),
        Pattern::Assign { .. } => todo!(),
        Pattern::Discard { .. } => Ok(Instruction::Nop),
        Pattern::List { .. } => todo!(),
        Pattern::Constructor { .. } => todo!(),
        Pattern::Tuple { elems, .. } => Ok(Instruction::Block {
            type_: BlockType::Empty,
            code: elems
                .iter()
                .enumerate()
                .map(|(index, element)| {
                    let element_type = program.resolve_type(encoder, &element.type_());
                    let element_local = function.declare_anonymous_local(element_type);

                    let tuple_type_index = program.resolve_type_index(encoder, &pattern.type_());

                    Instruction::Block {
                        type_: BlockType::Empty,
                        code: vec![
                            Instruction::LocalSet {
                                local: element_local,
                                value: Box::new(runtime::tuple_get(
                                    tuple_type_index,
                                    Instruction::LocalGet(value_local),
                                    index,
                                )),
                            },
                            encode_assignment_pattern(
                                program,
                                encoder,
                                module,
                                function,
                                element_local,
                                element,
                                &element.type_(),
                            )
                            .unwrap(),
                        ],
                    }
                })
                .collect(),
        }),
        Pattern::BitArray { .. } => todo!(),
        Pattern::StringPrefix { .. } => todo!(),
    }
}

fn encode_int(program: &mut Program, value: &str) -> Result<Instruction> {
    let value = value
        .replace("_", "")
        .parse::<i64>()
        .expect("Value unsupported");

    Ok(runtime::int(program, Instruction::I64Const(value)))
}

fn encode_float(program: &mut Program, value: &str) -> Result<Instruction> {
    let value = value.parse::<f64>().expect("Value unsupported");

    Ok(runtime::float(program, Instruction::F64Const(value)))
}

fn encode_string(
    program: &mut Program,
    encoder: &mut Module,
    value: &EcoString,
) -> Result<Instruction> {
    let string_type_index = program.resolve_prelude_type_index(PreludeType::String);

    let string_data_index = encoder.declare_data();
    let string_data = Data::new(value.bytes().collect_vec());

    encoder.define_data(string_data_index, string_data);

    let string_length = i32::try_from(value.as_bytes().len()).expect("String too long");

    Ok(Instruction::Block {
        type_: BlockType::Result(ValType::Ref(RefType {
            nullable: true,
            heap_type: HeapType::Concrete(string_type_index),
        })),
        code: vec![
            Instruction::I32Const(0),
            Instruction::I32Const(string_length),
            Instruction::ArrayNewData(string_type_index, string_data_index),
        ],
    })
}

fn encode_guard_clause(
    program: &mut Program,
    encoder: &mut Module,
    function: &mut Function,
    guard: &ClauseGuard<Arc<Type>, EcoString>,
) -> Instruction {
    match guard {
        ClauseGuard::Equals { left, right, .. } => {
            let lhs = encode_guard_clause(program, encoder, function, left);
            let rhs = encode_guard_clause(program, encoder, function, right);

            runtime::eq(program, encoder, &left.type_(), lhs, rhs)
        }
        ClauseGuard::NotEquals { left, right, .. } => {
            let type_index = program.resolve_type_index(encoder, &left.type_());

            let is_not_equal = Instruction::I32Eqz(Box::new(Instruction::Call {
                func: program.resolve_equality_index(type_index),
                args: vec![
                    encode_guard_clause(program, encoder, function, left),
                    encode_guard_clause(program, encoder, function, right),
                ],
            }));

            runtime::bool(program, is_not_equal)
        }
        ClauseGuard::GtInt { left, right, .. } => {
            let lhs = encode_guard_clause(program, encoder, function, left);
            let rhs = encode_guard_clause(program, encoder, function, right);

            let is_greater_than = Instruction::I64GtS {
                lhs: Box::new(runtime::int_to_i64(program, lhs)),
                rhs: Box::new(runtime::int_to_i64(program, rhs)),
            };

            runtime::bool(program, is_greater_than)
        }
        ClauseGuard::GtEqInt { left, right, .. } => {
            let lhs = encode_guard_clause(program, encoder, function, left);
            let rhs = encode_guard_clause(program, encoder, function, right);

            let is_greater_than_or_equal_to = Instruction::I64GeS {
                lhs: Box::new(runtime::int_to_i64(program, lhs)),
                rhs: Box::new(runtime::int_to_i64(program, rhs)),
            };

            runtime::bool(program, is_greater_than_or_equal_to)
        }
        ClauseGuard::LtInt { left, right, .. } => {
            let lhs = encode_guard_clause(program, encoder, function, left);
            let rhs = encode_guard_clause(program, encoder, function, right);

            let is_less_than = Instruction::I64LtS {
                lhs: Box::new(runtime::int_to_i64(program, lhs)),
                rhs: Box::new(runtime::int_to_i64(program, rhs)),
            };

            runtime::bool(program, is_less_than)
        }
        ClauseGuard::LtEqInt { left, right, .. } => {
            let lhs = encode_guard_clause(program, encoder, function, left);
            let rhs = encode_guard_clause(program, encoder, function, right);

            let is_less_than_or_equal_to = Instruction::I64LeS {
                lhs: Box::new(runtime::int_to_i64(program, lhs)),
                rhs: Box::new(runtime::int_to_i64(program, rhs)),
            };

            runtime::bool(program, is_less_than_or_equal_to)
        }
        ClauseGuard::GtFloat { left, right, .. } => {
            let lhs = encode_guard_clause(program, encoder, function, left);
            let rhs = encode_guard_clause(program, encoder, function, right);

            let is_greater_than = Instruction::F64Gt {
                lhs: Box::new(runtime::float_to_f64(program, lhs)),
                rhs: Box::new(runtime::float_to_f64(program, rhs)),
            };

            runtime::bool(program, is_greater_than)
        }
        ClauseGuard::GtEqFloat { left, right, .. } => {
            let lhs = encode_guard_clause(program, encoder, function, left);
            let rhs = encode_guard_clause(program, encoder, function, right);

            let is_greater_than_or_equal_to = Instruction::F64Ge {
                lhs: Box::new(runtime::float_to_f64(program, lhs)),
                rhs: Box::new(runtime::float_to_f64(program, rhs)),
            };

            runtime::bool(program, is_greater_than_or_equal_to)
        }
        ClauseGuard::LtFloat { left, right, .. } => {
            let lhs = encode_guard_clause(program, encoder, function, left);
            let rhs = encode_guard_clause(program, encoder, function, right);

            let is_less_than = Instruction::F64Lt {
                lhs: Box::new(runtime::float_to_f64(program, lhs)),
                rhs: Box::new(runtime::float_to_f64(program, rhs)),
            };

            runtime::bool(program, is_less_than)
        }
        ClauseGuard::LtEqFloat { left, right, .. } => {
            let lhs = encode_guard_clause(program, encoder, function, left);
            let rhs = encode_guard_clause(program, encoder, function, right);

            let is_less_than_or_equal_to = Instruction::F64Le {
                lhs: Box::new(runtime::float_to_f64(program, lhs)),
                rhs: Box::new(runtime::float_to_f64(program, rhs)),
            };

            runtime::bool(program, is_less_than_or_equal_to)
        }
        ClauseGuard::Or { left, right, .. } => {
            let bool_type = program.resolve_prelude_type(PreludeType::Bool);

            let lhs = encode_guard_clause(program, encoder, function, left);
            let rhs = encode_guard_clause(program, encoder, function, right);

            Instruction::If {
                type_: BlockType::Result(bool_type),
                cond: Box::new(Instruction::I32Eqz(Box::new(runtime::bool_to_i32(
                    program, lhs,
                )))),
                then: vec![rhs],
                else_: vec![runtime::bool_true(program)],
            }
        }
        ClauseGuard::And { left, right, .. } => {
            let bool_type = program.resolve_prelude_type(PreludeType::Bool);

            let lhs = encode_guard_clause(program, encoder, function, left);
            let rhs = encode_guard_clause(program, encoder, function, right);

            Instruction::If {
                type_: BlockType::Result(bool_type),
                cond: Box::new(Instruction::I32Eq {
                    lhs: Box::new(runtime::bool_to_i32(program, lhs)),
                    rhs: Box::new(Instruction::I32Const(1)),
                }),
                then: vec![rhs],
                else_: vec![runtime::bool_false(program)],
            }
        }
        ClauseGuard::Not { expression, .. } => {
            let value = encode_guard_clause(program, encoder, function, expression);

            runtime::bool_negate(program, value)
        }
        ClauseGuard::Var { name, .. } => {
            Instruction::LocalGet(function.get_local_index(name.clone()))
        }
        ClauseGuard::TupleIndex { index, tuple, .. } => {
            let tuple_type_index = program.resolve_type_index(encoder, &tuple.type_());
            let value = encode_guard_clause(program, encoder, function, tuple);

            runtime::tuple_get(tuple_type_index, value, (*index).try_into().unwrap())
        }
        ClauseGuard::FieldAccess {
            container,
            index: Some(index),
            ..
        } => {
            let container_type_index = program.resolve_type_index(encoder, &container.type_());
            let record = encode_guard_clause(program, encoder, function, container);

            runtime::get_field_from_type(container_type_index, (*index).try_into().unwrap(), record)
        }
        // TODO: When can index be none for a field access?
        ClauseGuard::FieldAccess { index: None, .. } => todo!(),
        ClauseGuard::ModuleSelect { .. } => todo!(),
        ClauseGuard::Constant(constant) => match constant {
            Constant::Int { value, .. } => encode_int(program, value).unwrap(),
            Constant::Float { value, .. } => encode_float(program, value).unwrap(),
            Constant::String { .. } => todo!(),
            Constant::Tuple { .. } => todo!(),
            Constant::List { .. } => todo!(),
            Constant::Record { .. } => todo!(),
            Constant::BitArray { .. } => todo!(),
            Constant::Var { .. } => todo!(),
        },
    }
}

fn encode_case_clause(
    program: &mut Program,
    encoder: &mut Module,
    function: &mut Function,
    clause: &Clause<TypedExpr, Arc<Type>, EcoString>,
    subjects: &[Index<Local>],
) -> Instruction {
    std::iter::once(&clause.pattern)
        .chain(clause.alternative_patterns.iter())
        .rfold(Instruction::I32Const(0), |else_, patterns| {
            Instruction::If {
                type_: BlockType::Result(ValType::I32),
                cond: Box::new(patterns.iter().zip(subjects.iter()).rfold(
                    Instruction::I32Const(1),
                    |then, (pattern, subject)| {
                        let condition =
                            encode_case_pattern(program, encoder, function, pattern, *subject);

                        Instruction::If {
                            type_: BlockType::Result(ValType::I32),
                            cond: Box::new(condition),
                            then: vec![then],
                            else_: vec![Instruction::I32Const(0)],
                        }
                    },
                )),
                then: vec![Instruction::I32Const(1)],
                else_: vec![else_],
            }
        })
}

fn encode_case_pattern(
    program: &mut Program,
    encoder: &mut Module,
    function: &mut Function,
    pattern: &Pattern<Arc<Type>>,
    subject: Index<Local>,
) -> Instruction {
    match pattern {
        Pattern::Int { value, .. } => {
            let value = value.parse::<i64>().unwrap();

            Instruction::I64Eq {
                lhs: Box::new(runtime::int_to_i64(program, Instruction::LocalGet(subject))),
                rhs: Box::new(Instruction::I64Const(value)),
            }
        }
        Pattern::Float { value, .. } => {
            let value = value.parse::<f64>().unwrap();

            Instruction::F64Eq {
                lhs: Box::new(runtime::float_to_f64(
                    program,
                    Instruction::LocalGet(subject),
                )),
                rhs: Box::new(Instruction::F64Const(value)),
            }
        }
        Pattern::String { value, .. } => Instruction::If {
            type_: BlockType::Result(ValType::I32),
            // If the string lengths aren't equal, they cannot be equal
            // so we early return.
            cond: Box::new(Instruction::I32Eq {
                lhs: Box::new(Instruction::ArrayLen(Box::new(Instruction::LocalGet(
                    subject,
                )))),
                rhs: Box::new(Instruction::I32Const(value.len().try_into().unwrap())),
            }),
            then: vec![string_compare(
                program,
                encoder,
                function,
                value,
                Instruction::LocalGet(subject),
            )],
            else_: vec![Instruction::I32Const(0)],
        },
        Pattern::Variable { name, type_, .. } => {
            let type_ = program.resolve_type(encoder, type_);
            let local = function.declare_local(name.clone(), type_);

            Instruction::Block {
                type_: BlockType::Result(ValType::I32),
                code: vec![
                    Instruction::LocalSet {
                        local,
                        value: Box::new(Instruction::LocalGet(subject)),
                    },
                    Instruction::I32Const(1),
                ],
            }
        }
        Pattern::VarUsage { .. } => todo!(),
        Pattern::Assign { name, pattern, .. } => {
            let local_type = program.resolve_type(encoder, &pattern.type_());
            let local = function.declare_local(name.clone(), local_type);

            Instruction::If {
                type_: BlockType::Result(ValType::I32),
                cond: Box::new(encode_case_pattern(
                    program, encoder, function, pattern, subject,
                )),
                then: vec![
                    Instruction::LocalSet {
                        local,
                        value: Box::new(Instruction::LocalGet(subject)),
                    },
                    Instruction::I32Const(1),
                ],
                else_: vec![Instruction::I32Const(0)],
            }
        }
        Pattern::Discard { .. } => Instruction::I32Const(1),
        Pattern::List {
            // location,
            elements,
            tail,
            // type_,
            ..
        } => {
            elements.iter().rfold(
                if let Some(tail) = tail {
                    encode_case_pattern(program, encoder, function, tail, subject)
                } else {
                    Instruction::RefIsNull(Box::new(Instruction::LocalGet(subject)))
                },
                |else_, element| {
                    let is_null = Instruction::RefIsNull(Box::new(Instruction::LocalGet(subject)));

                    let head = runtime::list_head(program, Instruction::LocalGet(subject));
                    let tail = runtime::list_tail(program, Instruction::LocalGet(subject));

                    let head_type_index = program.resolve_type_index(encoder, &element.type_());
                    let head_type = HeapType::Concrete(head_type_index);

                    let head_local = function
                        .declare_anonymous_local(program.resolve_type(encoder, &element.type_()));

                    // If the
                    Instruction::If {
                        type_: BlockType::Result(ValType::I32),
                        cond: Box::new(is_null),
                        then: vec![Instruction::I32Const(0)],
                        else_: vec![
                            Instruction::LocalSet {
                                local: head_local,
                                value: Box::new(runtime::cast_to(head, head_type)),
                            },
                            Instruction::If {
                                type_: BlockType::Result(ValType::I32),
                                cond: Box::new(encode_case_pattern(
                                    program, encoder, function, element, head_local,
                                )),
                                then: vec![
                                    Instruction::LocalSet {
                                        local: subject,
                                        value: Box::new(tail),
                                    },
                                    else_,
                                ],
                                else_: vec![Instruction::I32Const(0)],
                            },
                        ],
                    }
                },
            )
        }
        Pattern::Constructor {
            name: variant,
            arguments,
            type_,
            ..
        } => {
            let Some((module, type_name)) = type_.named_type_name() else {
                panic!("This should always be a named type")
            };

            let type_index = program.resolve_type_index(encoder, type_);
            let target_variant_tag = program.resolve_type_variant_tag(type_index, variant.clone());

            let variant_type_index =
                program.resolve_variant_type_index_by_name(module, type_name, variant.clone());
            let variant_heap_type = HeapType::Concrete(variant_type_index);

            let subject_local = Instruction::LocalGet(subject);

            let do_tags_match = Instruction::I32Eq {
                lhs: Box::new(runtime::get_type_tag(type_index, subject_local.clone())),
                rhs: Box::new(Instruction::I32Const(target_variant_tag)),
            };

            let do_arguments_match =
                arguments
                    .iter()
                    .enumerate()
                    .rfold(Instruction::I32Const(1), |then, (idx, arg)| {
                        let field = runtime::get_field_from_type(
                            variant_type_index,
                            idx,
                            runtime::cast_to(subject_local.clone(), variant_heap_type.clone()),
                        );

                        let field_local = function.declare_anonymous_local(
                            program.resolve_type(encoder, &arg.value.type_()),
                        );

                        let cond = encode_case_pattern(
                            program,
                            encoder,
                            function,
                            &arg.value,
                            field_local,
                        );

                        Instruction::Block {
                            type_: BlockType::Result(ValType::I32),
                            code: vec![
                                Instruction::LocalSet {
                                    local: field_local,
                                    value: Box::new(field),
                                },
                                Instruction::If {
                                    type_: BlockType::Result(ValType::I32),
                                    cond: Box::new(cond),
                                    then: vec![then],
                                    else_: vec![Instruction::I32Const(0)],
                                },
                            ],
                        }
                    });

            Instruction::If {
                type_: BlockType::Result(ValType::I32),
                cond: Box::new(do_tags_match),
                then: vec![do_arguments_match],
                else_: vec![Instruction::I32Const(0)],
            }
        }
        Pattern::Tuple { elems, .. } => {
            elems
                .iter()
                .enumerate()
                .rfold(Instruction::I32Const(1), |then, (index, element)| {
                    let local_type = program.resolve_type(encoder, &element.type_());
                    let local = function.declare_anonymous_local(local_type);

                    let tuple_type_index = program.resolve_type_index(encoder, &pattern.type_());

                    Instruction::Block {
                        type_: BlockType::Result(ValType::I32),
                        code: vec![
                            Instruction::LocalSet {
                                local,
                                value: Box::new(runtime::tuple_get(
                                    tuple_type_index,
                                    Instruction::LocalGet(subject),
                                    index,
                                )),
                            },
                            Instruction::If {
                                type_: BlockType::Result(ValType::I32),
                                cond: Box::new(encode_case_pattern(
                                    program, encoder, function, element, local,
                                )),
                                then: vec![then],
                                else_: vec![Instruction::I32Const(0)],
                            },
                        ],
                    }
                })
        }
        Pattern::BitArray { .. } => todo!(),
        Pattern::StringPrefix {
            left_side_string,
            left_side_assignment,
            right_side_assignment,
            ..
        } => {
            let when_prefix_matched = {
                let mut instructions = match left_side_assignment {
                    Some((name, _)) => {
                        let string_type = program.resolve_prelude_type(PreludeType::String);

                        let local = if function.does_local_exist(name.clone())
                            && function.get_local_type(name.clone()) == &string_type
                        {
                            function.get_local_index(name.clone())
                        } else {
                            function.declare_local(name.clone(), string_type)
                        };

                        vec![Instruction::LocalSet {
                            local: local,
                            value: Box::new(
                                encode_string(program, encoder, left_side_string).unwrap(),
                            ),
                        }]
                    }
                    None => vec![],
                };

                match right_side_assignment {
                    AssignName::Variable(name) => {
                        let string_type = program.resolve_prelude_type(PreludeType::String);
                        let string_type_index =
                            program.resolve_prelude_type_index(PreludeType::String);

                        let local = if function.does_local_exist(name.clone())
                            && function.get_local_type(name.clone()) == &string_type
                        {
                            function.get_local_index(name.clone())
                        } else {
                            function.declare_local(name.clone(), string_type)
                        };

                        let string_offset = left_side_string.len().try_into().unwrap();
                        let string_length = Instruction::I32Sub {
                            lhs: Box::new(Instruction::ArrayLen(Box::new(Instruction::LocalGet(
                                subject,
                            )))),
                            rhs: Box::new(Instruction::I32Const(string_offset)),
                        };

                        instructions.push(Instruction::LocalSet {
                            local,
                            value: Box::new(Instruction::ArrayNewDefault {
                                type_: string_type_index,
                                size: Box::new(string_length.clone()),
                            }),
                        });
                        instructions.push(Instruction::ArrayCopy {
                            dst_type: string_type_index,
                            dst: Box::new(Instruction::LocalGet(local)),
                            dst_offset: Box::new(Instruction::I32Const(0)),
                            src_type: string_type_index,
                            src: Box::new(Instruction::LocalGet(subject)),
                            src_offset: Box::new(Instruction::I32Const(string_offset)),
                            size: Box::new(string_length),
                        });
                        instructions.push(Instruction::I32Const(1));
                    }
                    AssignName::Discard(_) => {
                        instructions.push(Instruction::I32Const(1));
                    }
                };

                instructions
            };

            Instruction::If {
                type_: BlockType::Result(ValType::I32),
                cond: Box::new(Instruction::I32GeS {
                    lhs: Box::new(Instruction::ArrayLen(Box::new(Instruction::LocalGet(
                        subject,
                    )))),
                    rhs: Box::new(Instruction::I32Const(
                        left_side_string.len().try_into().unwrap(),
                    )),
                }),
                then: vec![Instruction::If {
                    type_: BlockType::Result(ValType::I32),
                    cond: Box::new(string_compare(
                        program,
                        encoder,
                        function,
                        left_side_string.as_str(),
                        Instruction::LocalGet(subject),
                    )),
                    then: when_prefix_matched,
                    else_: vec![Instruction::I32Const(0)],
                }],
                else_: vec![Instruction::I32Const(0)],
            }
        }
    }
}

fn encode_expression(
    program: &mut Program,
    encoder: &mut Module,
    module: &ModuleAst,
    function: &mut Function,
    expression: &TypedExpr,
) -> Result<Instruction> {
    match expression {
        TypedExpr::Int { value, .. } => encode_int(program, value),
        TypedExpr::Float { value, .. } => encode_float(program, value),
        TypedExpr::String { value, .. } => encode_string(program, encoder, value),
        TypedExpr::Block { statements, .. } => {
            let block_type = program.resolve_type(encoder, &expression.type_());
            let block = encode_block(program, encoder, module, function, statements)?;

            Ok(Instruction::Block {
                type_: BlockType::Result(block_type),
                code: block,
            })
        }
        TypedExpr::Pipeline {
            assignments,
            finally,
            ..
        } => {
            let block_type = program.resolve_type(encoder, &expression.type_());

            let mut statements = assignments
                .iter()
                .map(|assignment| {
                    let assignment =
                        encode_assignment(program, encoder, module, function, assignment)?;

                    Ok(Instruction::Drop(Box::new(assignment)))
                })
                .collect::<Result<Vec<_>>>()?;

            statements.push(encode_expression(
                program, encoder, module, function, finally,
            )?);

            Ok(Instruction::Block {
                type_: BlockType::Result(block_type),
                code: statements,
            })
        }
        TypedExpr::Var {
            constructor, name, ..
        } => match &constructor.variant {
            ValueConstructorVariant::LocalVariable { .. } => {
                let local_index = function.get_local_index(name.clone());

                Ok(Instruction::LocalGet(local_index))
            }
            ValueConstructorVariant::ModuleConstant { literal, .. } => match literal {
                Constant::Int { value, .. } => encode_int(program, value),
                Constant::Float { value, .. } => encode_float(program, value),
                Constant::String { value, .. } => encode_string(program, encoder, value),
                Constant::Tuple { .. } => todo!(),
                Constant::List { .. } => todo!(),
                Constant::Record { .. } => todo!(),
                Constant::BitArray { .. } => todo!(),
                Constant::Var { .. } => todo!(),
            },
            ValueConstructorVariant::LocalConstant { .. } => todo!(),
            ValueConstructorVariant::ModuleFn { module, name, .. } => {
                let function_index =
                    program.resolve_function_index_by_name(module.clone(), name.clone());

                let closure_type_index = program::runtime::closure_type_index(program);
                let unit_type_index = program::runtime::unit_type_index(program);

                Ok(Instruction::StructNew {
                    type_: closure_type_index,
                    args: vec![
                        Instruction::RefFunc(function_index),
                        Instruction::StructNew {
                            type_: unit_type_index,
                            args: vec![],
                        },
                    ],
                })
            }
            ValueConstructorVariant::Record {
                arity,
                module,
                name,
                ..
            } => {
                let function_index =
                    program.resolve_function_index_by_name(module.clone(), name.clone());
                let unit_type_index = program::runtime::unit_type_index(program);

                match arity {
                    0 => Ok(Instruction::Call {
                        func: function_index,
                        args: vec![Instruction::StructNew {
                            type_: unit_type_index,
                            args: vec![],
                        }],
                    }),
                    _ => {
                        let closure_type_index = program::runtime::closure_type_index(program);

                        Ok(Instruction::StructNew {
                            type_: closure_type_index,
                            args: vec![
                                Instruction::RefFunc(function_index),
                                Instruction::StructNew {
                                    type_: unit_type_index,
                                    args: vec![],
                                },
                            ],
                        })
                    }
                }
            }
        },
        TypedExpr::Fn {
            is_capture: true, ..
        } => todo!(),
        TypedExpr::Fn {
            is_capture: false,
            location,
            args,
            body,
            typ,
            ..
        } => {
            let Type::Fn {
                args: parameters,
                retrn: return_type,
            } = typ.as_ref()
            else {
                panic!("invalid")
            };

            // Collect all variables captured by this closure
            let captured_variables = collect_captured_variables(args, body);
            let captured = captured_variables
                .iter()
                .map(|variable| function.get_local_type(variable.clone()).clone())
                .collect_vec();

            // Create a struct containing all the captured variables.
            let captured_struct_type_index = encoder.declare_type();
            let captured_struct_type = encoder::Type::Struct {
                fields: captured
                    .iter()
                    .map(|captured_type| FieldType {
                        element_type: StorageType::Val(captured_type.clone()),
                        mutable: true,
                    })
                    .collect_vec(),
            };

            encoder.define_type(captured_struct_type_index, captured_struct_type);

            let closure_struct_type_index = program::runtime::closure_type_index(program);

            // Create the type of this closure.
            let closure_type_index = encoder.declare_type();

            let mut lambda_parameters = parameters
                .iter()
                .map(|parameter| program.resolve_type(encoder, parameter))
                .collect_vec();

            lambda_parameters.push(ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Struct,
            }));

            let lambda_type = encoder::Type::Function {
                params: lambda_parameters,
                results: vec![program.resolve_type(encoder, return_type)],
            };

            encoder.define_type(closure_type_index, lambda_type);

            // Create the closure
            let closure_index = encoder.declare_function();
            let closure = {
                let mut arguments = args
                    .iter()
                    .map(|arg| match &arg.names {
                        ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => None,
                        ArgNames::Named { name } | ArgNames::NamedLabelled { name, .. } => {
                            Some(name.clone())
                        }
                    })
                    .collect_vec();

                arguments.push(Some("_ctx".into()));

                let mut lambda = Function::new(
                    closure_index,
                    closure_type_index,
                    format!("{:?}", location).into(),
                    FunctionLinkage::Export,
                    arguments,
                );

                let context = lambda.get_local_index("_ctx".into());

                // Pull out all the captured variables into their own locals
                for (index, (name, type_)) in
                    captured_variables.iter().zip(captured.iter()).enumerate()
                {
                    let local = lambda.declare_local(name.clone(), type_.clone());

                    lambda.instruction(Instruction::LocalSet {
                        local,
                        value: Box::new(Instruction::StructGet {
                            from: Box::new(Instruction::RefCastNullable {
                                value: Box::new(Instruction::LocalGet(context)),
                                type_: HeapType::Concrete(captured_struct_type_index),
                            }),
                            type_: captured_struct_type_index,
                            index: index.try_into().unwrap(),
                        }),
                    });
                }

                // Generate the body of the closure
                for instruction in encode_block(program, encoder, module, &mut lambda, body)? {
                    lambda.instruction(instruction);
                }

                lambda.instruction(Instruction::End);
                lambda
            };

            encoder.define_function(closure_index, closure);

            Ok(Instruction::StructNew {
                type_: closure_struct_type_index,
                args: vec![
                    Instruction::RefFunc(closure_index),
                    Instruction::StructNew {
                        type_: captured_struct_type_index,
                        args: captured_variables
                            .iter()
                            .map(|variable| {
                                Instruction::LocalGet(function.get_local_index(variable.clone()))
                            })
                            .collect_vec(),
                    },
                ],
            })
        }
        TypedExpr::List { elements, tail, .. } => {
            let list_type_index = runtime::list_type_index(program);

            Ok(elements.iter().rfold(
                if let Some(tail) = tail {
                    encode_expression(program, encoder, module, function, tail).unwrap()
                } else {
                    Instruction::RefNull(HeapType::Concrete(list_type_index))
                },
                |next, element| {
                    let value =
                        encode_expression(program, encoder, module, function, element).unwrap();

                    Instruction::StructNew {
                        type_: list_type_index,
                        args: vec![next, value],
                    }
                },
            ))
        }
        TypedExpr::Call { fun, args, .. } => {
            let type_ = program.resolve_type(encoder, &expression.type_());

            let closure_type_index = encoder.declare_type();
            let closure_type = encoder::Type::Function {
                params: {
                    let mut params = args
                        .iter()
                        .map(|arg| program.resolve_type(encoder, &arg.value.type_()))
                        .collect_vec();
                    params.push(ValType::Ref(RefType {
                        nullable: true,
                        heap_type: HeapType::Struct,
                    }));
                    params
                },
                results: vec![type_.clone()],
            };

            encoder.define_type(closure_type_index, closure_type);

            let arguments = args
                .into_iter()
                .map(|arg| encode_expression(program, encoder, module, function, &arg.value))
                .collect::<Result<Vec<_>, _>>()?;

            let closure_struct_type_index = program::runtime::closure_type_index(program);

            let func_ref = encode_expression(program, encoder, module, function, &fun)?;
            let func_ref_local = function.declare_anonymous_local(ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(closure_struct_type_index),
            }));

            Ok(Instruction::Block {
                type_: BlockType::Result(type_),
                code: vec![
                    Instruction::LocalSet {
                        local: func_ref_local,
                        value: Box::new(func_ref),
                    },
                    Instruction::CallRef {
                        type_: closure_type_index,
                        ref_: Box::new(Instruction::RefCastNullable {
                            value: Box::new(Instruction::StructGet {
                                from: Box::new(Instruction::LocalGet(func_ref_local)),
                                type_: closure_struct_type_index,
                                index: 0,
                            }),
                            type_: HeapType::Concrete(closure_type_index),
                        }),
                        args: {
                            let mut args = arguments;
                            args.push(Instruction::StructGet {
                                from: Box::new(Instruction::LocalGet(func_ref_local)),
                                type_: closure_struct_type_index,
                                index: 1,
                            });
                            args
                        },
                    },
                ],
            })
        }
        TypedExpr::BinOp {
            name, left, right, ..
        } => match name {
            BinOp::And => {
                let bool_type = program.resolve_prelude_type(PreludeType::Bool);

                let lhs = encode_expression(program, encoder, module, function, left)?;
                let rhs = encode_expression(program, encoder, module, function, right)?;

                Ok(Instruction::If {
                    type_: BlockType::Result(bool_type),
                    cond: Box::new(Instruction::I32Eq {
                        lhs: Box::new(runtime::bool_to_i32(program, lhs)),
                        rhs: Box::new(Instruction::I32Const(1)),
                    }),
                    then: vec![rhs],
                    else_: vec![runtime::bool_false(program)],
                })
            }
            BinOp::Or => {
                let bool_type = program.resolve_prelude_type(PreludeType::Bool);

                let lhs = encode_expression(program, encoder, module, function, left)?;
                let rhs = encode_expression(program, encoder, module, function, right)?;

                Ok(Instruction::If {
                    type_: BlockType::Result(bool_type),
                    cond: Box::new(Instruction::I32Eqz(Box::new(runtime::bool_to_i32(
                        program, lhs,
                    )))),
                    then: vec![rhs],
                    else_: vec![runtime::bool_true(program)],
                })
            }
            BinOp::Eq => {
                let lhs = encode_expression(program, encoder, module, function, left)?;
                let rhs = encode_expression(program, encoder, module, function, right)?;

                Ok(runtime::eq(program, encoder, &left.type_(), lhs, rhs))
            }
            BinOp::NotEq => {
                let lhs = encode_expression(program, encoder, module, function, left)?;
                let rhs = encode_expression(program, encoder, module, function, right)?;

                let are_equal = runtime::eq(program, encoder, &left.type_(), lhs, rhs);

                Ok(runtime::bool_negate(program, are_equal))
            }
            BinOp::LtInt | BinOp::LtEqInt | BinOp::GtEqInt | BinOp::GtInt => {
                let lhs = encode_expression(program, encoder, module, function, left)?;
                let lhs = runtime::int_to_i64(program, lhs);

                let rhs = encode_expression(program, encoder, module, function, right)?;
                let rhs = runtime::int_to_i64(program, rhs);

                Ok(runtime::bool(
                    program,
                    match name {
                        BinOp::LtInt => Instruction::I64LtS {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        BinOp::LtEqInt => Instruction::I64LeS {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        BinOp::GtEqInt => Instruction::I64GeS {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        BinOp::GtInt => Instruction::I64GtS {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        _ => unreachable!(),
                    },
                ))
            }
            BinOp::LtFloat | BinOp::LtEqFloat | BinOp::GtEqFloat | BinOp::GtFloat => {
                let lhs = encode_expression(program, encoder, module, function, left)?;
                let lhs = runtime::float_to_f64(program, lhs);

                let rhs = encode_expression(program, encoder, module, function, right)?;
                let rhs = runtime::float_to_f64(program, rhs);

                Ok(runtime::bool(
                    program,
                    match name {
                        BinOp::LtFloat => Instruction::F64Lt {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        BinOp::LtEqFloat => Instruction::F64Le {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        BinOp::GtEqFloat => Instruction::F64Ge {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        BinOp::GtFloat => Instruction::F64Gt {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        _ => unreachable!(),
                    },
                ))
            }
            BinOp::AddInt
            | BinOp::SubInt
            | BinOp::MultInt
            | BinOp::DivInt
            | BinOp::RemainderInt => {
                let lhs = encode_expression(program, encoder, module, function, left)?;
                let lhs = runtime::int_to_i64(program, lhs);

                let rhs = encode_expression(program, encoder, module, function, right)?;
                let rhs = runtime::int_to_i64(program, rhs);

                Ok(runtime::int(
                    program,
                    match name {
                        BinOp::AddInt => Instruction::I64Add {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        BinOp::SubInt => Instruction::I64Sub {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        BinOp::MultInt => Instruction::I64Mul {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        BinOp::DivInt => Instruction::I64DivS {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        BinOp::RemainderInt => Instruction::I64RemS {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        _ => unreachable!(),
                    },
                ))
            }
            BinOp::AddFloat | BinOp::SubFloat | BinOp::MultFloat | BinOp::DivFloat => {
                let lhs = encode_expression(program, encoder, module, function, left)?;
                let lhs = runtime::float_to_f64(program, lhs);

                let rhs = encode_expression(program, encoder, module, function, right)?;
                let rhs = runtime::float_to_f64(program, rhs);

                Ok(runtime::float(
                    program,
                    match name {
                        BinOp::AddFloat => Instruction::F64Add {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        BinOp::SubFloat => Instruction::F64Sub {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        BinOp::MultFloat => Instruction::F64Mul {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        BinOp::DivFloat => Instruction::F64Div {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        _ => unreachable!(),
                    },
                ))
            }
            BinOp::Concatenate => {
                let string_type_index = program.resolve_prelude_type_index(PreludeType::String);

                let local_type = ValType::Ref(RefType {
                    nullable: true,
                    heap_type: HeapType::Concrete(string_type_index),
                });

                let string = function.declare_anonymous_local(local_type.clone());
                let lhs = function.declare_anonymous_local(local_type.clone());
                let rhs = function.declare_anonymous_local(local_type.clone());

                Ok(Instruction::Block {
                    type_: BlockType::Result(local_type),
                    code: vec![
                        Instruction::LocalSet {
                            local: lhs,
                            value: Box::new(encode_expression(
                                program, encoder, module, function, left,
                            )?),
                        },
                        Instruction::LocalSet {
                            local: rhs,
                            value: Box::new(encode_expression(
                                program, encoder, module, function, right,
                            )?),
                        },
                        // Create new string
                        Instruction::LocalSet {
                            local: string,
                            value: Box::new(Instruction::ArrayNewDefault {
                                type_: string_type_index,
                                size: Box::new(Instruction::I32Add {
                                    lhs: Box::new(Instruction::ArrayLen(Box::new(
                                        Instruction::LocalGet(lhs),
                                    ))),
                                    rhs: Box::new(Instruction::ArrayLen(Box::new(
                                        Instruction::LocalGet(rhs),
                                    ))),
                                }),
                            }),
                        },
                        // Copy lhs into the new string
                        Instruction::ArrayCopy {
                            dst_type: string_type_index,
                            dst: Box::new(Instruction::LocalGet(string)),
                            dst_offset: Box::new(Instruction::I32Const(0)),
                            src_type: string_type_index,
                            src: Box::new(Instruction::LocalGet(lhs)),
                            src_offset: Box::new(Instruction::I32Const(0)),
                            size: Box::new(Instruction::ArrayLen(Box::new(Instruction::LocalGet(
                                lhs,
                            )))),
                        },
                        // Copy rhs into the new string
                        Instruction::ArrayCopy {
                            dst_type: string_type_index,
                            dst: Box::new(Instruction::LocalGet(string)),
                            dst_offset: Box::new(Instruction::ArrayLen(Box::new(
                                Instruction::LocalGet(lhs),
                            ))),
                            src_type: string_type_index,
                            src: Box::new(Instruction::LocalGet(rhs)),
                            src_offset: Box::new(Instruction::I32Const(0)),
                            size: Box::new(Instruction::ArrayLen(Box::new(Instruction::LocalGet(
                                rhs,
                            )))),
                        },
                        // Get the string
                        Instruction::LocalGet(string),
                    ],
                })
            }
        },
        TypedExpr::Case {
            typ,
            subjects,
            clauses,
            ..
        } => {
            // Declare the subject locals
            let subject_locals = subjects
                .iter()
                .map(|subject| {
                    function
                        .declare_anonymous_local(program.resolve_type(encoder, &subject.type_()))
                })
                .collect_vec();

            // Assign the locals
            let mut code = subjects
                .iter()
                .zip(subject_locals.iter())
                .map(|(subject, local)| {
                    let value =
                        encode_expression(program, encoder, module, function, subject).unwrap();

                    Instruction::LocalSet {
                        local: *local,
                        value: Box::new(value),
                    }
                })
                .collect_vec();

            let block_type = BlockType::Result(program.resolve_type(encoder, typ));

            code.push(
                clauses
                    .iter()
                    .rfold(Instruction::Unreachable, |else_, clause| {
                        let condition =
                            encode_case_clause(program, encoder, function, clause, &subject_locals);

                        let guard = match &clause.guard {
                            Some(guard) => {
                                let guard_clause =
                                    encode_guard_clause(program, encoder, function, guard);

                                runtime::bool_to_i32(program, guard_clause)
                            }
                            None => Instruction::I32Const(1),
                        };

                        let condition = Instruction::If {
                            type_: BlockType::Result(ValType::I32),
                            cond: Box::new(condition),
                            then: vec![guard],
                            else_: vec![Instruction::I32Const(0)],
                        };

                        let then =
                            encode_expression(program, encoder, module, function, &clause.then)
                                .unwrap();

                        Instruction::If {
                            type_: block_type.clone(),
                            cond: Box::new(condition),
                            then: vec![then],
                            else_: vec![else_],
                        }
                    }),
            );

            Ok(Instruction::Block {
                type_: block_type.clone(),
                code,
            })
        }
        TypedExpr::RecordAccess { index, record, .. } => {
            let heap_type_index = program.resolve_type_index(encoder, &record.type_());

            let index = u32::try_from(*index + 1).expect("Record is too large");
            let record = encode_expression(program, encoder, module, function, record)?;

            Ok(Instruction::StructGet {
                from: Box::new(record),
                type_: heap_type_index,
                index,
            })
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

                let closure_type_index = program::runtime::closure_type_index(program);
                let unit_type_index = program::runtime::unit_type_index(program);

                Ok(Instruction::StructNew {
                    type_: closure_type_index,
                    args: vec![
                        Instruction::RefFunc(function_index),
                        Instruction::StructNew {
                            type_: unit_type_index,
                            args: vec![],
                        },
                    ],
                })
            }
            ModuleValueConstructor::Constant { literal, .. } => match literal {
                Constant::Int { value, .. } => encode_int(program, value),
                Constant::Float { value, .. } => encode_float(program, value),
                Constant::String { value, .. } => encode_string(program, encoder, value),
                Constant::Tuple { .. } => todo!(),
                Constant::List { .. } => todo!(),
                Constant::Record { .. } => todo!(),
                Constant::BitArray { .. } => todo!(),
                Constant::Var { .. } => todo!(),
            },
        },
        TypedExpr::Tuple { typ, elems, .. } => {
            let type_index = program.resolve_type_index(encoder, &typ);

            let arguments = elems
                .iter()
                .map(|elem| encode_expression(program, encoder, module, function, elem))
                .collect::<Result<Vec<_>, _>>()?;

            Ok(Instruction::StructNew {
                type_: type_index,
                args: arguments,
            })
        }
        TypedExpr::TupleIndex { tuple, index, .. } => {
            let type_index = program.resolve_type_index(encoder, &tuple.type_());
            let index = u32::try_from(*index).unwrap();

            let tuple = encode_expression(program, encoder, module, function, tuple)?;

            Ok(Instruction::StructGet {
                from: Box::new(tuple),
                type_: type_index,
                index,
            })
        }
        TypedExpr::Todo { .. } => todo!(),
        TypedExpr::Panic { .. } => todo!(),
        TypedExpr::BitArray { segments, .. } => {
            for segment in segments {
                dbg!(segment);
            }

            todo!()
        }
        TypedExpr::RecordUpdate { .. } => todo!(),
        TypedExpr::NegateBool { value, .. } => {
            let value = encode_expression(program, encoder, module, function, value)?;

            let negated = Instruction::I32Xor {
                lhs: Box::new(runtime::bool_to_i32(program, value)),
                rhs: Box::new(Instruction::I32Const(1)),
            };

            Ok(runtime::bool(program, negated))
        }
        TypedExpr::NegateInt { value, .. } => {
            let value = encode_expression(program, encoder, module, function, value)?;

            let negated = Instruction::I64Sub {
                lhs: Box::new(Instruction::I64Const(0)),
                rhs: Box::new(runtime::int_to_i64(program, value)),
            };

            Ok(runtime::int(program, negated))
        }
    }
}

fn get_custom_type_definitions(module: &ModuleAst) -> impl Iterator<Item = &CustomType<Arc<Type>>> {
    module
        .ast
        .definitions
        .iter()
        .filter_map(|definition| match definition {
            Definition::CustomType(custom_type) => Some(custom_type),
            _ => None,
        })
}

fn get_function_definitions(module: &ModuleAst) -> impl Iterator<Item = &TypedFunction> {
    module
        .ast
        .definitions
        .iter()
        .filter_map(|definition| match definition {
            Definition::Function(function) => Some(function),
            _ => None,
        })
}

fn collect_captured_variables(
    arguments: &[Arg<Arc<Type>>],
    body: &[TypedStatement],
) -> Vec<EcoString> {
    let mut captured = vec![];
    let mut variables = HashSet::new();

    for argument in arguments {
        match &argument.names {
            ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => {}
            ArgNames::Named { name } | ArgNames::NamedLabelled { name, .. } => {
                _ = variables.insert(name.clone());
            }
        }
    }

    for statement in body {
        collect_captured_variables_from_statement(statement, &mut variables, &mut captured);
    }

    captured
}

fn collect_captured_variables_from_statement(
    statement: &TypedStatement,
    variables: &mut HashSet<EcoString>,
    captured: &mut Vec<EcoString>,
) {
    match statement {
        Statement::Expression(expression) => {
            collect_captured_variables_from_expression(expression, variables, captured)
        }
        Statement::Assignment(assignment) => {
            collect_captured_variables_from_assignment(assignment, variables, captured)
        }
        Statement::Use(_) => todo!(),
    }
}

fn collect_captured_variables_from_assignment(
    assignment: &TypedAssignment,
    variables: &mut HashSet<EcoString>,
    captured: &mut Vec<EcoString>,
) {
    collect_captured_variables_from_expression(&assignment.value, variables, captured);
    collect_captured_variables_from_pattern(&assignment.pattern, variables, captured);
}

fn collect_captured_variables_from_pattern(
    pattern: &TypedPattern,
    variables: &mut HashSet<EcoString>,
    captured: &mut Vec<EcoString>,
) {
    match pattern {
        Pattern::Int { .. } => {}
        Pattern::Float { .. } => {}
        Pattern::String { .. } => {}
        Pattern::Variable { name, .. } => {
            _ = variables.insert(name.clone());
        }
        Pattern::VarUsage { .. } => todo!(),
        Pattern::Assign { name, pattern, .. } => {
            _ = variables.insert(name.clone());

            collect_captured_variables_from_pattern(pattern, variables, captured);
        }
        Pattern::Discard { .. } => {}
        Pattern::List { elements, tail, .. } => {
            for element in elements {
                collect_captured_variables_from_pattern(element, variables, captured);
            }

            if let Some(tail) = tail {
                collect_captured_variables_from_pattern(tail, variables, captured);
            }
        }
        Pattern::Constructor { arguments, .. } => {
            for arg in arguments {
                collect_captured_variables_from_pattern(&arg.value, variables, captured);
            }
        }
        Pattern::Tuple { elems, .. } => {
            for elem in elems {
                collect_captured_variables_from_pattern(elem, variables, captured);
            }
        }
        Pattern::BitArray { .. } => todo!(),
        Pattern::StringPrefix {
            left_side_assignment,
            right_side_assignment,
            ..
        } => {
            if let Some((name, _)) = left_side_assignment {
                _ = variables.insert(name.clone());
            }

            match right_side_assignment {
                AssignName::Variable(name) => {
                    _ = variables.insert(name.clone());
                }
                AssignName::Discard(_) => {}
            }
        }
    }
}

fn collect_captured_variables_from_expression(
    expression: &TypedExpr,
    variables: &mut HashSet<EcoString>,
    captured: &mut Vec<EcoString>,
) {
    match expression {
        TypedExpr::Int { .. } => {}
        TypedExpr::Float { .. } => {}
        TypedExpr::String { .. } => {}
        TypedExpr::Block { statements, .. } => {
            for statement in statements {
                collect_captured_variables_from_statement(statement, variables, captured);
            }
        }
        TypedExpr::Pipeline {
            assignments,
            finally,
            ..
        } => {
            for assignment in assignments {
                collect_captured_variables_from_assignment(assignment, variables, captured);
            }

            collect_captured_variables_from_expression(finally, variables, captured);
        }
        TypedExpr::Var {
            constructor, name, ..
        } => match &constructor.variant {
            ValueConstructorVariant::LocalVariable { .. } => {
                if !variables.contains(name) {
                    captured.push(name.clone());
                }
            }
            ValueConstructorVariant::ModuleConstant { .. } => {}
            ValueConstructorVariant::LocalConstant { .. } => {}
            ValueConstructorVariant::ModuleFn { .. } => {}
            ValueConstructorVariant::Record { .. } => {}
        },
        TypedExpr::Fn { body, .. } => {
            for statement in body {
                collect_captured_variables_from_statement(statement, variables, captured);
            }
        }
        TypedExpr::List { elements, tail, .. } => {
            for element in elements {
                collect_captured_variables_from_expression(element, variables, captured);
            }

            if let Some(tail) = tail {
                collect_captured_variables_from_expression(tail, variables, captured);
            }
        }
        TypedExpr::Call { args, .. } => {
            for arg in args {
                collect_captured_variables_from_expression(&arg.value, variables, captured);
            }
        }
        TypedExpr::BinOp { left, right, .. } => {
            collect_captured_variables_from_expression(left, variables, captured);
            collect_captured_variables_from_expression(right, variables, captured);
        }
        TypedExpr::Case {
            subjects, clauses, ..
        } => {
            for subject in subjects {
                collect_captured_variables_from_expression(subject, variables, captured);
            }

            for clause in clauses {
                collect_captured_variables_from_expression(&clause.then, variables, captured);

                for pattern in &clause.pattern {
                    collect_captured_variables_from_pattern(&pattern, variables, captured);
                }

                for alternative_pattern in &clause.alternative_patterns {
                    for pattern in alternative_pattern {
                        collect_captured_variables_from_pattern(&pattern, variables, captured);
                    }
                }
            }
        }
        TypedExpr::RecordAccess { record, .. } => {
            collect_captured_variables_from_expression(record, variables, captured);
        }
        TypedExpr::ModuleSelect { .. } => {}
        TypedExpr::Tuple { elems, .. } => {
            for elem in elems {
                collect_captured_variables_from_expression(elem, variables, captured);
            }
        }
        TypedExpr::TupleIndex { tuple, .. } => {
            collect_captured_variables_from_expression(tuple, variables, captured);
        }
        TypedExpr::Todo { .. } => todo!(),
        TypedExpr::Panic { .. } => todo!(),
        // TODO: Do this in the future
        TypedExpr::BitArray { .. } => {}
        TypedExpr::RecordUpdate { .. } => todo!(),
        TypedExpr::NegateBool { value, .. } => {
            collect_captured_variables_from_expression(value, variables, captured);
        }
        TypedExpr::NegateInt { .. } => todo!(),
    }
}
