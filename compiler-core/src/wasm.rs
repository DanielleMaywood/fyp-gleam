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
    build::Module,
    error::Result,
    type_::{prelude::PreludeType, ModuleValueConstructor, Type, ValueConstructorVariant},
    wasm::{
        encoder::{BlockType, Index, Local},
        program::{
            runtime::{self, string_compare},
            Program,
        },
    },
};

mod encoder;
mod program;

pub fn program(modules: &[Module]) -> Result<Vec<u8>> {
    let mut program = Program::default();
    let mut encoder = encoder::Module::default();

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

    for constructor in custom_type.constructors.iter() {
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
            params: {
                let mut params = constructor
                    .arguments
                    .iter()
                    .map(|argument| program.resolve_type(encoder, &argument.type_))
                    .collect_vec();
                params.push(encoder::ValType::Ref(encoder::RefType {
                    nullable: true,
                    heap_type: encoder::HeapType::Struct,
                }));
                params
            },
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

            let arguments = argument_names
                .iter()
                .map(|name| Some(name.clone()))
                .collect_vec();

            let mut function = encoder::Function::new(
                constructor_function_index,
                constructor_function_type_index,
                EcoString::from(format!("{}/{}", module.ast.name, constructor.name)),
                encoder::FunctionLinkage::Export,
                {
                    let mut arguments = arguments;
                    arguments.push(None);
                    arguments
                },
            );

            let constructor_tag =
                program.resolve_type_variant_tag(parent_type_index, constructor.name.clone());

            let mut arguments = vec![encoder::Instruction::I32Const(constructor_tag)];
            arguments.extend(&mut {
                argument_names.into_iter().map(|argument| {
                    encoder::Instruction::LocalGet(function.get_local_index(argument))
                })
            });

            function.instruction(encoder::Instruction::StructNew {
                type_: constructor_type_index,
                args: arguments,
            });
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

        // If the given arguments are not the same variant,
        // then they are not equal.
        function.instruction(encoder::Instruction::If {
            type_: encoder::BlockType::Empty,
            cond: Box::new(encoder::Instruction::I32Ne {
                lhs: Box::new(encoder::Instruction::StructGet {
                    from: Box::new(encoder::Instruction::LocalGet(lhs_index)),
                    type_: parent_type_index,
                    index: 0,
                }),
                rhs: Box::new(encoder::Instruction::StructGet {
                    from: Box::new(encoder::Instruction::LocalGet(rhs_index)),
                    type_: parent_type_index,
                    index: 0,
                }),
            }),
            then: vec![encoder::Instruction::Return(Some(Box::new(
                encoder::Instruction::I32Const(0),
            )))],
            else_: vec![],
        });

        let branch_block = encoder::Instruction::Block {
            type_: encoder::BlockType::Empty,
            code: vec![
                encoder::Instruction::StructGet {
                    from: Box::new(encoder::Instruction::LocalGet(lhs_index)),
                    type_: parent_type_index,
                    index: 0,
                },
                encoder::Instruction::BrTable(
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

                let constructor_ref = encoder::ValType::Ref(encoder::RefType {
                    nullable: true,
                    heap_type: encoder::HeapType::Concrete(constructor_type_index),
                });

                let lhs_casted_index = function.declare_anonymous_local(constructor_ref.clone());
                let rhs_casted_index = function.declare_anonymous_local(constructor_ref);

                code.push(encoder::Instruction::LocalSet {
                    local: lhs_casted_index,
                    value: Box::new(encoder::Instruction::RefCast {
                        type_: encoder::RefType {
                            nullable: true,
                            heap_type: encoder::HeapType::Concrete(constructor_type_index),
                        },
                        value: Box::new(encoder::Instruction::LocalGet(lhs_index)),
                    }),
                });

                code.push(encoder::Instruction::LocalSet {
                    local: rhs_casted_index,
                    value: Box::new(encoder::Instruction::RefCast {
                        type_: encoder::RefType {
                            nullable: true,
                            heap_type: encoder::HeapType::Concrete(constructor_type_index),
                        },
                        value: Box::new(encoder::Instruction::LocalGet(rhs_index)),
                    }),
                });

                for (argument_index, argument) in constructor.arguments.iter().enumerate() {
                    let argument_type_index = program.resolve_type_index(encoder, &argument.type_);
                    let field_index = (argument_index + 1) as u32;

                    code.push(encoder::Instruction::If {
                        type_: encoder::BlockType::Empty,
                        cond: Box::new(encoder::Instruction::I32Eqz(Box::new(
                            encoder::Instruction::Call {
                                func: program.resolve_equality_index(argument_type_index),
                                args: vec![
                                    encoder::Instruction::StructGet {
                                        from: Box::new(encoder::Instruction::LocalGet(
                                            lhs_casted_index,
                                        )),
                                        type_: constructor_type_index,
                                        index: field_index,
                                    },
                                    encoder::Instruction::StructGet {
                                        from: Box::new(encoder::Instruction::LocalGet(
                                            rhs_casted_index,
                                        )),
                                        type_: constructor_type_index,
                                        index: field_index,
                                    },
                                ],
                            },
                        ))),
                        then: vec![encoder::Instruction::Return(Some(Box::new(
                            encoder::Instruction::I32Const(0),
                        )))],
                        else_: vec![],
                    });
                }

                encoder::Instruction::Block {
                    type_: encoder::BlockType::Empty,
                    code,
                }
            });

        function.instruction(block);
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
        params: {
            let mut params = the_function
                .arguments
                .iter()
                .map(|param| program.resolve_type(encoder, &param.type_))
                .collect_vec();
            if the_function.name != "main" {
                params.push(encoder::ValType::Ref(encoder::RefType {
                    nullable: true,
                    heap_type: encoder::HeapType::Struct,
                }));
            }
            params
        },
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
            if the_function.name != "main" {
                arguments.push(None);
            }
            arguments
        },
    );

    for instruction in encode_block(program, encoder, module, &mut function, &the_function.body)? {
        function.instruction(instruction);
    }

    function.instruction(encoder::Instruction::End);

    encoder.define_function(function_index, function);

    Ok(())
}

enum KeepOrDrop {
    Keep,
    Drop,
}

fn encode_block(
    program: &mut Program,
    encoder: &mut encoder::Module,
    module: &Module,
    function: &mut encoder::Function,
    body: &Vec1<TypedStatement>,
) -> Result<Vec<encoder::Instruction>> {
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
    encoder: &mut encoder::Module,
    module: &Module,
    function: &mut encoder::Function,
    statement: &TypedStatement,
    keep_or_drop: KeepOrDrop,
) -> Result<encoder::Instruction> {
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
        KeepOrDrop::Drop => encoder::Instruction::Drop(Box::new(instruction)),
    })
}

fn encode_assignment(
    program: &mut Program,
    encoder: &mut encoder::Module,
    module: &Module,
    function: &mut encoder::Function,
    assignment: &TypedAssignment,
) -> Result<encoder::Instruction> {
    match &assignment.pattern {
        Pattern::Int { .. } => todo!(),
        Pattern::Float { .. } => todo!(),
        Pattern::String { .. } => todo!(),
        Pattern::Variable { name, .. } => {
            let value = encode_expression(program, encoder, module, function, &assignment.value)?;

            let local_type = program.resolve_type(encoder, &assignment.value.type_());
            let local_index = function.declare_local(name.clone(), local_type);

            Ok(encoder::Instruction::LocalTee {
                local: local_index,
                value: Box::new(value),
            })
        }
        Pattern::VarUsage { .. } => todo!(),
        Pattern::Assign { .. } => todo!(),
        Pattern::Discard { .. } => {
            encode_expression(program, encoder, module, function, &assignment.value)
        }
        Pattern::List { .. } => todo!(),
        Pattern::Constructor { .. } => todo!(),
        Pattern::Tuple { .. } => todo!(),
        Pattern::BitArray { .. } => todo!(),
        Pattern::StringPrefix { .. } => todo!(),
    }
}

fn encode_int(program: &mut Program, value: i64) -> Result<encoder::Instruction> {
    Ok(encoder::Instruction::Call {
        func: runtime::int_constructor(program),
        args: vec![encoder::Instruction::I64Const(value)],
    })
}

fn encode_float(program: &mut Program, value: f64) -> Result<encoder::Instruction> {
    Ok(encoder::Instruction::Call {
        func: runtime::float_constructor(program),
        args: vec![encoder::Instruction::F64Const(value)],
    })
}

fn encode_string(
    program: &mut Program,
    encoder: &mut encoder::Module,
    value: &EcoString,
) -> Result<encoder::Instruction> {
    let string_type_index = program.resolve_prelude_type_index(PreludeType::String);

    let string_data_index = encoder.declare_data();
    let string_data = encoder::Data::new(value.bytes().collect_vec());

    encoder.define_data(string_data_index, string_data);

    let string_length = i32::try_from(value.as_bytes().len()).expect("String too long");

    Ok(encoder::Instruction::Block {
        type_: encoder::BlockType::Result(encoder::ValType::Ref(encoder::RefType {
            nullable: true,
            heap_type: encoder::HeapType::Concrete(string_type_index),
        })),
        code: vec![
            encoder::Instruction::I32Const(0),
            encoder::Instruction::I32Const(string_length),
            encoder::Instruction::ArrayNewData(string_type_index, string_data_index),
        ],
    })
}

fn encode_guard_clause(
    program: &mut Program,
    encoder: &mut encoder::Module,
    function: &mut encoder::Function,
    guard: &ClauseGuard<Arc<Type>, EcoString>,
) -> encoder::Instruction {
    match guard {
        ClauseGuard::Equals { left, right, .. } => {
            let bool_type_index = program.resolve_prelude_type_index(PreludeType::Bool);
            let type_index = program.resolve_type_index(encoder, &left.type_());

            encoder::Instruction::StructNew {
                type_: bool_type_index,
                args: vec![encoder::Instruction::Call {
                    func: program.resolve_equality_index(type_index),
                    args: vec![
                        encode_guard_clause(program, encoder, function, left),
                        encode_guard_clause(program, encoder, function, right),
                    ],
                }],
            }
        }
        ClauseGuard::NotEquals { left, right, .. } => {
            let bool_type_index = program.resolve_prelude_type_index(PreludeType::Bool);
            let type_index = program.resolve_type_index(encoder, &left.type_());
            let equality_index = program.resolve_equality_index(type_index);

            encoder::Instruction::StructNew {
                type_: bool_type_index,
                args: vec![encoder::Instruction::I32Eqz(Box::new(
                    encoder::Instruction::Call {
                        func: equality_index,
                        args: vec![
                            encode_guard_clause(program, encoder, function, left),
                            encode_guard_clause(program, encoder, function, right),
                        ],
                    },
                ))],
            }
        }
        ClauseGuard::GtInt { .. } => todo!(),
        ClauseGuard::GtEqInt { .. } => todo!(),
        ClauseGuard::LtInt { .. } => todo!(),
        ClauseGuard::LtEqInt { .. } => todo!(),
        ClauseGuard::GtFloat { .. } => todo!(),
        ClauseGuard::GtEqFloat { .. } => todo!(),
        ClauseGuard::LtFloat { .. } => todo!(),
        ClauseGuard::LtEqFloat { .. } => todo!(),
        ClauseGuard::Or { .. } => todo!(),
        ClauseGuard::And { .. } => todo!(),
        ClauseGuard::Not { .. } => todo!(),
        ClauseGuard::Var { name, .. } => {
            encoder::Instruction::LocalGet(function.get_local_index(name.clone()))
        }
        ClauseGuard::TupleIndex { .. } => todo!(),
        ClauseGuard::FieldAccess { .. } => todo!(),
        ClauseGuard::ModuleSelect { .. } => todo!(),
        ClauseGuard::Constant(constant) => match constant {
            Constant::Int { value, .. } => encode_int(program, value.parse().unwrap()).unwrap(),
            Constant::Float { .. } => todo!(),
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
    encoder: &mut encoder::Module,
    function: &mut encoder::Function,
    clause: &Clause<TypedExpr, Arc<Type>, EcoString>,
    subjects: &[Index<Local>],
) -> encoder::Instruction {
    std::iter::once(&clause.pattern)
        .chain(clause.alternative_patterns.iter())
        .rfold(encoder::Instruction::I32Const(0), |else_, patterns| {
            encoder::Instruction::If {
                type_: encoder::BlockType::Result(encoder::ValType::I32),
                cond: Box::new(patterns.iter().zip(subjects.iter()).rfold(
                    encoder::Instruction::I32Const(1),
                    |then, (pattern, subject)| {
                        let condition =
                            encode_case_pattern(program, encoder, function, pattern, *subject);

                        encoder::Instruction::If {
                            type_: encoder::BlockType::Result(encoder::ValType::I32),
                            cond: Box::new(condition),
                            then: vec![then],
                            else_: vec![encoder::Instruction::I32Const(0)],
                        }
                    },
                )),
                then: vec![encoder::Instruction::I32Const(1)],
                else_: vec![else_],
            }
        })
}

fn encode_case_pattern(
    program: &mut Program,
    encoder: &mut encoder::Module,
    function: &mut encoder::Function,
    pattern: &Pattern<Arc<Type>>,
    subject: Index<Local>,
) -> encoder::Instruction {
    match pattern {
        Pattern::Int { value, .. } => {
            let int_type_index = program.resolve_prelude_type_index(PreludeType::Int);

            let value = value.parse::<i64>().unwrap();

            encoder::Instruction::I64Eq {
                lhs: Box::new(encoder::Instruction::StructGet {
                    from: Box::new(encoder::Instruction::LocalGet(subject)),
                    type_: int_type_index,
                    index: 0,
                }),
                rhs: Box::new(encoder::Instruction::I64Const(value)),
            }
        }
        Pattern::Float { value, .. } => {
            let float_type_index = program.resolve_prelude_type_index(PreludeType::Float);

            let value = value.parse::<f64>().unwrap();

            encoder::Instruction::F64Eq {
                lhs: Box::new(encoder::Instruction::StructGet {
                    from: Box::new(encoder::Instruction::LocalGet(subject)),
                    type_: float_type_index,
                    index: 0,
                }),
                rhs: Box::new(encoder::Instruction::F64Const(value)),
            }
        }
        Pattern::String { value, .. } => encoder::Instruction::If {
            type_: encoder::BlockType::Result(encoder::ValType::I32),
            // If the string lengths aren't equal, they cannot be equal
            // so we early return.
            cond: Box::new(encoder::Instruction::I32Eq {
                lhs: Box::new(encoder::Instruction::ArrayLen(Box::new(
                    encoder::Instruction::LocalGet(subject),
                ))),
                rhs: Box::new(encoder::Instruction::I32Const(
                    value.len().try_into().unwrap(),
                )),
            }),
            then: vec![string_compare(
                program,
                encoder,
                function,
                value,
                encoder::Instruction::LocalGet(subject),
            )],
            else_: vec![encoder::Instruction::I32Const(0)],
        },
        Pattern::Variable { name, type_, .. } => {
            let type_ = program.resolve_type(encoder, type_);
            let local = function.declare_local(name.clone(), type_);

            encoder::Instruction::Block {
                type_: encoder::BlockType::Result(encoder::ValType::I32),
                code: vec![
                    encoder::Instruction::LocalSet {
                        local,
                        value: Box::new(encoder::Instruction::LocalGet(subject)),
                    },
                    encoder::Instruction::I32Const(1),
                ],
            }
        }
        Pattern::VarUsage { .. } => todo!(),
        Pattern::Assign { name, pattern, .. } => {
            let local_type = program.resolve_type(encoder, &pattern.type_());
            let local = function.declare_local(name.clone(), local_type);

            encoder::Instruction::If {
                type_: encoder::BlockType::Result(encoder::ValType::I32),
                cond: Box::new(encode_case_pattern(
                    program, encoder, function, pattern, subject,
                )),
                then: vec![
                    encoder::Instruction::LocalSet {
                        local,
                        value: Box::new(encoder::Instruction::LocalGet(subject)),
                    },
                    encoder::Instruction::I32Const(1),
                ],
                else_: vec![encoder::Instruction::I32Const(0)],
            }
        }
        Pattern::Discard { .. } => encoder::Instruction::I32Const(1),
        Pattern::List {
            location,
            elements,
            tail,
            type_,
        } => {
            let list_type_index = runtime::list_type_index(program);

            match elements.as_slice() {
                [] => match tail {
                    Some(tail) => encode_case_pattern(program, encoder, function, tail, subject),
                    None => encoder::Instruction::I32Const(1),
                },
                [element, rest @ ..] => {
                    let value_type_index = program.resolve_type_index(encoder, &element.type_());
                    let value_type = program.resolve_type(encoder, &element.type_());
                    let value_local = function.declare_anonymous_local(value_type);

                    let next_local =
                        function.declare_anonymous_local(encoder::ValType::Ref(encoder::RefType {
                            nullable: true,
                            heap_type: encoder::HeapType::Concrete(list_type_index),
                        }));

                    encoder::Instruction::Block {
                        type_: encoder::BlockType::Result(encoder::ValType::I32),
                        code: vec![
                            encoder::Instruction::LocalSet {
                                local: value_local,
                                value: Box::new(encoder::Instruction::RefCast {
                                    type_: encoder::RefType {
                                        nullable: true,
                                        heap_type: encoder::HeapType::Concrete(value_type_index),
                                    },
                                    value: Box::new(encoder::Instruction::StructGet {
                                        from: Box::new(encoder::Instruction::LocalGet(subject)),
                                        type_: list_type_index,
                                        index: 1,
                                    }),
                                }),
                            },
                            encoder::Instruction::If {
                                type_: encoder::BlockType::Result(encoder::ValType::I32),
                                cond: Box::new(encode_case_pattern(
                                    program,
                                    encoder,
                                    function,
                                    element,
                                    value_local,
                                )),
                                then: vec![
                                    encoder::Instruction::LocalSet {
                                        local: next_local,
                                        value: Box::new(encoder::Instruction::StructGet {
                                            from: Box::new(encoder::Instruction::LocalGet(subject)),
                                            type_: list_type_index,
                                            index: 0,
                                        }),
                                    },
                                    encode_case_pattern(
                                        program,
                                        encoder,
                                        function,
                                        &Pattern::List {
                                            location: location.clone(),
                                            elements: rest.to_vec(),
                                            tail: tail.clone(),
                                            type_: type_.clone(),
                                        },
                                        next_local,
                                    ),
                                ],
                                else_: vec![encoder::Instruction::I32Const(0)],
                            },
                        ],
                    }
                }
            }
        }
        Pattern::Constructor {
            name,
            arguments,
            type_,
            ..
        } => {
            let type_index = program.resolve_type_index(encoder, type_);
            let tag = program.resolve_type_variant_tag(type_index, name.clone());

            let Some((module, type_name)) = type_.named_type_name() else {
                panic!("This should always be a named type")
            };

            let variant_type_index = program.resolve_type_index_by_name(
                module,
                EcoString::from(format!("{}.{}", type_name, name)),
            );

            let tags_match = encoder::Instruction::I32Eq {
                lhs: Box::new(encoder::Instruction::StructGet {
                    from: Box::new(encoder::Instruction::LocalGet(subject)),
                    type_: type_index,
                    index: 0,
                }),
                rhs: Box::new(encoder::Instruction::I32Const(tag)),
            };

            let arguments_match = arguments.iter().enumerate().rfold(
                encoder::Instruction::I32Const(1),
                |then, (idx, arg)| {
                    let field = encoder::Instruction::StructGet {
                        from: Box::new(encoder::Instruction::RefCast {
                            value: Box::new(encoder::Instruction::LocalGet(subject)),
                            type_: encoder::RefType {
                                nullable: true,
                                heap_type: encoder::HeapType::Concrete(variant_type_index),
                            },
                        }),
                        type_: variant_type_index,
                        index: (1 + idx).try_into().unwrap(),
                    };

                    let field_type = program.resolve_type(encoder, &arg.value.type_());
                    let field_index = function.declare_anonymous_local(field_type);

                    let cond =
                        encode_case_pattern(program, encoder, function, &arg.value, field_index);

                    encoder::Instruction::Block {
                        type_: encoder::BlockType::Result(encoder::ValType::I32),
                        code: vec![
                            encoder::Instruction::LocalSet {
                                local: field_index,
                                value: Box::new(field),
                            },
                            encoder::Instruction::If {
                                type_: encoder::BlockType::Result(encoder::ValType::I32),
                                cond: Box::new(cond),
                                then: vec![then],
                                else_: vec![encoder::Instruction::I32Const(0)],
                            },
                        ],
                    }
                },
            );

            encoder::Instruction::If {
                type_: encoder::BlockType::Result(encoder::ValType::I32),
                cond: Box::new(tags_match),
                then: vec![arguments_match],
                else_: vec![encoder::Instruction::I32Const(0)],
            }
        }
        Pattern::Tuple { elems, .. } => elems.iter().enumerate().rfold(
            encoder::Instruction::I32Const(1),
            |then, (index, element)| {
                let local_type = program.resolve_type(encoder, &element.type_());
                let local = function.declare_anonymous_local(local_type);

                encoder::Instruction::Block {
                    type_: encoder::BlockType::Result(encoder::ValType::I32),
                    code: vec![
                        encoder::Instruction::LocalSet {
                            local,
                            value: Box::new(encoder::Instruction::StructGet {
                                from: Box::new(encoder::Instruction::LocalGet(subject)),
                                type_: program.resolve_type_index(encoder, &pattern.type_()),
                                index: index.try_into().unwrap(),
                            }),
                        },
                        encoder::Instruction::If {
                            type_: encoder::BlockType::Result(encoder::ValType::I32),
                            cond: Box::new(encode_case_pattern(
                                program, encoder, function, element, local,
                            )),
                            then: vec![then],
                            else_: vec![encoder::Instruction::I32Const(0)],
                        },
                    ],
                }
            },
        ),
        Pattern::BitArray { .. } => todo!(),
        Pattern::StringPrefix {
            left_side_string,
            right_side_assignment,
            ..
        } => encoder::Instruction::If {
            type_: encoder::BlockType::Result(encoder::ValType::I32),
            cond: Box::new(encoder::Instruction::I32GeS {
                lhs: Box::new(encoder::Instruction::ArrayLen(Box::new(
                    encoder::Instruction::LocalGet(subject),
                ))),
                rhs: Box::new(encoder::Instruction::I32Const(
                    left_side_string.len().try_into().unwrap(),
                )),
            }),
            then: vec![encoder::Instruction::If {
                type_: encoder::BlockType::Result(encoder::ValType::I32),
                cond: Box::new(string_compare(
                    program,
                    encoder,
                    function,
                    left_side_string.as_str(),
                    encoder::Instruction::LocalGet(subject),
                )),
                then: match right_side_assignment {
                    AssignName::Variable(name) => {
                        let string_type_index =
                            program.resolve_prelude_type_index(PreludeType::String);

                        let local = function.declare_local(
                            name.clone(),
                            program.resolve_prelude_type(PreludeType::String),
                        );

                        let string_offset = left_side_string.len().try_into().unwrap();
                        let string_length = encoder::Instruction::I32Sub {
                            lhs: Box::new(encoder::Instruction::ArrayLen(Box::new(
                                encoder::Instruction::LocalGet(subject),
                            ))),
                            rhs: Box::new(encoder::Instruction::I32Const(string_offset)),
                        };

                        vec![
                            encoder::Instruction::LocalSet {
                                local,
                                value: Box::new(encoder::Instruction::ArrayNewDefault {
                                    type_: string_type_index,
                                    size: Box::new(string_length.clone()),
                                }),
                            },
                            encoder::Instruction::ArrayCopy {
                                dst_type: string_type_index,
                                dst: Box::new(encoder::Instruction::LocalGet(local)),
                                dst_offset: Box::new(encoder::Instruction::I32Const(0)),
                                src_type: string_type_index,
                                src: Box::new(encoder::Instruction::LocalGet(subject)),
                                src_offset: Box::new(encoder::Instruction::I32Const(string_offset)),
                                size: Box::new(string_length),
                            },
                            encoder::Instruction::I32Const(1),
                        ]
                    }
                    AssignName::Discard(_) => {
                        vec![encoder::Instruction::I32Const(1)]
                    }
                },
                else_: vec![encoder::Instruction::I32Const(0)],
            }],
            else_: vec![encoder::Instruction::I32Const(0)],
        },
    }
}

fn encode_expression(
    program: &mut Program,
    encoder: &mut encoder::Module,
    module: &Module,
    function: &mut encoder::Function,
    expression: &TypedExpr,
) -> Result<encoder::Instruction> {
    match expression {
        TypedExpr::Int { value, .. } => {
            let value = value.parse::<i64>().expect("Value unsupported");

            encode_int(program, value)
        }
        TypedExpr::Float { value, .. } => {
            let value = value.parse::<f64>().expect("Value unsupported");

            encode_float(program, value)
        }
        TypedExpr::String { value, .. } => encode_string(program, encoder, value),
        TypedExpr::Block { statements, .. } => {
            let block_type = program.resolve_type(encoder, &expression.type_());
            let block = encode_block(program, encoder, module, function, statements)?;

            Ok(encoder::Instruction::Block {
                type_: encoder::BlockType::Result(block_type),
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

                    Ok(encoder::Instruction::Drop(Box::new(assignment)))
                })
                .collect::<Result<Vec<_>>>()?;

            statements.push(encode_expression(
                program, encoder, module, function, finally,
            )?);

            Ok(encoder::Instruction::Block {
                type_: encoder::BlockType::Result(block_type),
                code: statements,
            })
        }
        TypedExpr::Var {
            constructor, name, ..
        } => match &constructor.variant {
            ValueConstructorVariant::LocalVariable { .. } => {
                let local_index = function.get_local_index(name.clone());

                Ok(encoder::Instruction::LocalGet(local_index))
            }
            ValueConstructorVariant::ModuleConstant { literal, .. } => match literal {
                Constant::Int { value, .. } => {
                    let value = value.parse().unwrap();

                    encode_int(program, value)
                }
                Constant::Float { value, .. } => {
                    let value = value.parse().unwrap();

                    encode_float(program, value)
                }
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

                Ok(encoder::Instruction::StructNew {
                    type_: closure_type_index,
                    args: vec![
                        encoder::Instruction::RefFunc(function_index),
                        encoder::Instruction::StructNew {
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
                    0 => Ok(encoder::Instruction::Call {
                        func: function_index,
                        args: vec![encoder::Instruction::StructNew {
                            type_: unit_type_index,
                            args: vec![],
                        }],
                    }),
                    _ => {
                        let closure_type_index = program::runtime::closure_type_index(program);

                        Ok(encoder::Instruction::StructNew {
                            type_: closure_type_index,
                            args: vec![
                                encoder::Instruction::RefFunc(function_index),
                                encoder::Instruction::StructNew {
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
                    .map(|captured_type| encoder::FieldType {
                        element_type: encoder::StorageType::Val(captured_type.clone()),
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

            lambda_parameters.push(encoder::ValType::Ref(encoder::RefType {
                nullable: true,
                heap_type: encoder::HeapType::Struct,
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

                let mut lambda = encoder::Function::new(
                    closure_index,
                    closure_type_index,
                    format!("{:?}", location).into(),
                    encoder::FunctionLinkage::Export,
                    arguments,
                );

                let context = lambda.get_local_index("_ctx".into());

                // Pull out all the captured variables into their own locals
                for (index, (name, type_)) in
                    captured_variables.iter().zip(captured.iter()).enumerate()
                {
                    let local = lambda.declare_local(name.clone(), type_.clone());

                    lambda.instruction(encoder::Instruction::LocalSet {
                        local,
                        value: Box::new(encoder::Instruction::StructGet {
                            from: Box::new(encoder::Instruction::RefCast {
                                value: Box::new(encoder::Instruction::LocalGet(context)),
                                type_: encoder::RefType {
                                    nullable: true,
                                    heap_type: encoder::HeapType::Concrete(
                                        captured_struct_type_index,
                                    ),
                                },
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

                lambda.instruction(encoder::Instruction::End);
                lambda
            };

            encoder.define_function(closure_index, closure);

            Ok(encoder::Instruction::StructNew {
                type_: closure_struct_type_index,
                args: vec![
                    encoder::Instruction::RefFunc(closure_index),
                    encoder::Instruction::StructNew {
                        type_: captured_struct_type_index,
                        args: captured_variables
                            .iter()
                            .map(|variable| {
                                encoder::Instruction::LocalGet(
                                    function.get_local_index(variable.clone()),
                                )
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
                    encoder::Instruction::RefNull(encoder::HeapType::Concrete(list_type_index))
                },
                |next, element| {
                    let value =
                        encode_expression(program, encoder, module, function, element).unwrap();

                    encoder::Instruction::StructNew {
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
                    params.push(encoder::ValType::Ref(encoder::RefType {
                        nullable: true,
                        heap_type: encoder::HeapType::Struct,
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
            let func_ref_local =
                function.declare_anonymous_local(encoder::ValType::Ref(encoder::RefType {
                    nullable: true,
                    heap_type: encoder::HeapType::Concrete(closure_struct_type_index),
                }));

            Ok(encoder::Instruction::Block {
                type_: encoder::BlockType::Result(type_),
                code: vec![
                    encoder::Instruction::LocalSet {
                        local: func_ref_local,
                        value: Box::new(func_ref),
                    },
                    encoder::Instruction::CallRef {
                        type_: closure_type_index,
                        ref_: Box::new(encoder::Instruction::RefCast {
                            value: Box::new(encoder::Instruction::StructGet {
                                from: Box::new(encoder::Instruction::LocalGet(func_ref_local)),
                                type_: closure_struct_type_index,
                                index: 0,
                            }),
                            type_: encoder::RefType {
                                nullable: true,
                                heap_type: encoder::HeapType::Concrete(closure_type_index),
                            },
                        }),
                        args: {
                            let mut args = arguments;
                            args.push(encoder::Instruction::StructGet {
                                from: Box::new(encoder::Instruction::LocalGet(func_ref_local)),
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
                let bool_type_index = program.resolve_prelude_type_index(PreludeType::Bool);
                let bool_type = program.resolve_prelude_type(PreludeType::Bool);

                let lhs = encode_expression(program, encoder, module, function, left)?;
                let rhs = encode_expression(program, encoder, module, function, right)?;

                Ok(encoder::Instruction::If {
                    type_: BlockType::Result(bool_type),
                    cond: Box::new(encoder::Instruction::I32Eq {
                        lhs: Box::new(encoder::Instruction::StructGet {
                            from: Box::new(lhs),
                            type_: bool_type_index,
                            index: 0,
                        }),
                        rhs: Box::new(encoder::Instruction::I32Const(1)),
                    }),
                    then: vec![rhs],
                    else_: vec![encoder::Instruction::StructNew {
                        type_: bool_type_index,
                        args: vec![encoder::Instruction::I32Const(0)],
                    }],
                })
            }
            BinOp::Or => {
                let bool_type_index = program.resolve_prelude_type_index(PreludeType::Bool);
                let bool_type = program.resolve_prelude_type(PreludeType::Bool);

                let lhs = encode_expression(program, encoder, module, function, left)?;
                let rhs = encode_expression(program, encoder, module, function, right)?;

                Ok(encoder::Instruction::If {
                    type_: BlockType::Result(bool_type),
                    cond: Box::new(encoder::Instruction::I32Eqz(Box::new(
                        encoder::Instruction::StructGet {
                            from: Box::new(lhs),
                            type_: bool_type_index,
                            index: 0,
                        },
                    ))),
                    then: vec![rhs],
                    else_: vec![encoder::Instruction::StructNew {
                        type_: bool_type_index,
                        args: vec![encoder::Instruction::I32Const(1)],
                    }],
                })
            }
            BinOp::Eq => {
                let bool_type_index = program.resolve_prelude_type_index(PreludeType::Bool);
                let type_index = program.resolve_type_index(encoder, &left.type_());

                Ok(encoder::Instruction::StructNew {
                    type_: bool_type_index,
                    args: vec![encoder::Instruction::Call {
                        func: program.resolve_equality_index(type_index),
                        args: vec![
                            encode_expression(program, encoder, module, function, left)?,
                            encode_expression(program, encoder, module, function, right)?,
                        ],
                    }],
                })
            }
            BinOp::NotEq => {
                let bool_type_index = program.resolve_prelude_type_index(PreludeType::Bool);
                let type_index = program.resolve_type_index(encoder, &left.type_());
                let equality_index = program.resolve_equality_index(type_index);

                Ok(encoder::Instruction::StructNew {
                    type_: bool_type_index,
                    args: vec![encoder::Instruction::I32Eqz(Box::new(
                        encoder::Instruction::Call {
                            func: equality_index,
                            args: vec![
                                encode_expression(program, encoder, module, function, left)?,
                                encode_expression(program, encoder, module, function, right)?,
                            ],
                        },
                    ))],
                })
            }
            BinOp::LtInt | BinOp::LtEqInt | BinOp::GtEqInt | BinOp::GtInt => {
                let bool_type_index = program.resolve_prelude_type_index(PreludeType::Bool);
                let int_type_index = program.resolve_prelude_type_index(PreludeType::Int);

                let lhs = encode_expression(program, encoder, module, function, left)?;
                let lhs = encoder::Instruction::StructGet {
                    from: Box::new(lhs),
                    type_: int_type_index,
                    index: 0,
                };

                let rhs = encode_expression(program, encoder, module, function, right)?;
                let rhs = encoder::Instruction::StructGet {
                    from: Box::new(rhs),
                    type_: int_type_index,
                    index: 0,
                };

                Ok(encoder::Instruction::StructNew {
                    type_: bool_type_index,
                    args: vec![match name {
                        BinOp::LtInt => encoder::Instruction::I64LtS {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        BinOp::LtEqInt => encoder::Instruction::I64LeS {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        BinOp::GtEqInt => encoder::Instruction::I64GeS {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        BinOp::GtInt => encoder::Instruction::I64GtS {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        _ => unreachable!(),
                    }],
                })
            }
            BinOp::LtFloat | BinOp::LtEqFloat | BinOp::GtEqFloat | BinOp::GtFloat => {
                let bool_type_index = program.resolve_prelude_type_index(PreludeType::Bool);
                let float_type_index = program.resolve_prelude_type_index(PreludeType::Float);

                let lhs = encode_expression(program, encoder, module, function, left)?;
                let lhs = encoder::Instruction::StructGet {
                    from: Box::new(lhs),
                    type_: float_type_index,
                    index: 0,
                };

                let rhs = encode_expression(program, encoder, module, function, right)?;
                let rhs = encoder::Instruction::StructGet {
                    from: Box::new(rhs),
                    type_: float_type_index,
                    index: 0,
                };

                Ok(encoder::Instruction::StructNew {
                    type_: bool_type_index,
                    args: vec![match name {
                        BinOp::LtFloat => encoder::Instruction::F64Lt {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        BinOp::LtEqFloat => encoder::Instruction::F64Le {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        BinOp::GtEqFloat => encoder::Instruction::F64Ge {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        BinOp::GtFloat => encoder::Instruction::F64Gt {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        _ => unreachable!(),
                    }],
                })
            }
            BinOp::AddInt
            | BinOp::SubInt
            | BinOp::MultInt
            | BinOp::DivInt
            | BinOp::RemainderInt => {
                let int_constructor_index = runtime::int_constructor(program);
                let int_type_index = program.resolve_prelude_type_index(PreludeType::Int);

                let lhs = encode_expression(program, encoder, module, function, left)?;
                let lhs = encoder::Instruction::StructGet {
                    from: Box::new(lhs),
                    type_: int_type_index,
                    index: 0,
                };

                let rhs = encode_expression(program, encoder, module, function, right)?;
                let rhs = encoder::Instruction::StructGet {
                    from: Box::new(rhs),
                    type_: int_type_index,
                    index: 0,
                };

                Ok(encoder::Instruction::Call {
                    func: int_constructor_index,
                    args: vec![match name {
                        BinOp::AddInt => encoder::Instruction::I64Add {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        BinOp::SubInt => encoder::Instruction::I64Sub {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        BinOp::MultInt => encoder::Instruction::I64Mul {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        BinOp::DivInt => encoder::Instruction::I64DivS {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        BinOp::RemainderInt => encoder::Instruction::I64RemS {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        _ => unreachable!(),
                    }],
                })
            }
            BinOp::AddFloat | BinOp::SubFloat | BinOp::MultFloat | BinOp::DivFloat => {
                let float_constructor_index = runtime::float_constructor(program);
                let float_type_index = program.resolve_prelude_type_index(PreludeType::Float);

                let lhs = encode_expression(program, encoder, module, function, left)?;
                let lhs = encoder::Instruction::StructGet {
                    from: Box::new(lhs),
                    type_: float_type_index,
                    index: 0,
                };

                let rhs = encode_expression(program, encoder, module, function, right)?;
                let rhs = encoder::Instruction::StructGet {
                    from: Box::new(rhs),
                    type_: float_type_index,
                    index: 0,
                };

                Ok(encoder::Instruction::Call {
                    func: float_constructor_index,
                    args: vec![match name {
                        BinOp::AddFloat => encoder::Instruction::F64Add {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        BinOp::SubFloat => encoder::Instruction::F64Sub {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        BinOp::MultFloat => encoder::Instruction::F64Mul {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        BinOp::DivFloat => encoder::Instruction::F64Div {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        },
                        _ => unreachable!(),
                    }],
                })
            }
            BinOp::Concatenate => {
                let string_type_index = program.resolve_prelude_type_index(PreludeType::String);

                let local_type = encoder::ValType::Ref(encoder::RefType {
                    nullable: true,
                    heap_type: encoder::HeapType::Concrete(string_type_index),
                });

                let string = function.declare_anonymous_local(local_type.clone());
                let lhs = function.declare_anonymous_local(local_type.clone());
                let rhs = function.declare_anonymous_local(local_type.clone());

                Ok(encoder::Instruction::Block {
                    type_: encoder::BlockType::Result(local_type),
                    code: vec![
                        encoder::Instruction::LocalSet {
                            local: lhs,
                            value: Box::new(encode_expression(
                                program, encoder, module, function, left,
                            )?),
                        },
                        encoder::Instruction::LocalSet {
                            local: rhs,
                            value: Box::new(encode_expression(
                                program, encoder, module, function, right,
                            )?),
                        },
                        // Create new string
                        encoder::Instruction::LocalSet {
                            local: string,
                            value: Box::new(encoder::Instruction::ArrayNewDefault {
                                type_: string_type_index,
                                size: Box::new(encoder::Instruction::I32Add {
                                    lhs: Box::new(encoder::Instruction::ArrayLen(Box::new(
                                        encoder::Instruction::LocalGet(lhs),
                                    ))),
                                    rhs: Box::new(encoder::Instruction::ArrayLen(Box::new(
                                        encoder::Instruction::LocalGet(rhs),
                                    ))),
                                }),
                            }),
                        },
                        // Copy lhs into the new string
                        encoder::Instruction::ArrayCopy {
                            dst_type: string_type_index,
                            dst: Box::new(encoder::Instruction::LocalGet(string)),
                            dst_offset: Box::new(encoder::Instruction::I32Const(0)),
                            src_type: string_type_index,
                            src: Box::new(encoder::Instruction::LocalGet(lhs)),
                            src_offset: Box::new(encoder::Instruction::I32Const(0)),
                            size: Box::new(encoder::Instruction::ArrayLen(Box::new(
                                encoder::Instruction::LocalGet(lhs),
                            ))),
                        },
                        // Copy rhs into the new string
                        encoder::Instruction::ArrayCopy {
                            dst_type: string_type_index,
                            dst: Box::new(encoder::Instruction::LocalGet(string)),
                            dst_offset: Box::new(encoder::Instruction::ArrayLen(Box::new(
                                encoder::Instruction::LocalGet(lhs),
                            ))),
                            src_type: string_type_index,
                            src: Box::new(encoder::Instruction::LocalGet(rhs)),
                            src_offset: Box::new(encoder::Instruction::I32Const(0)),
                            size: Box::new(encoder::Instruction::ArrayLen(Box::new(
                                encoder::Instruction::LocalGet(rhs),
                            ))),
                        },
                        // Get the string
                        encoder::Instruction::LocalGet(string),
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

                    encoder::Instruction::LocalSet {
                        local: *local,
                        value: Box::new(value),
                    }
                })
                .collect_vec();

            let block_type = encoder::BlockType::Result(program.resolve_type(encoder, typ));

            code.push(
                clauses
                    .iter()
                    .rfold(encoder::Instruction::Unreachable, |else_, clause| {
                        let condition =
                            encode_case_clause(program, encoder, function, clause, &subject_locals);

                        let guard = match &clause.guard {
                            Some(guard) => {
                                let bool_type_index =
                                    program.resolve_prelude_type_index(PreludeType::Bool);

                                encoder::Instruction::StructGet {
                                    type_: bool_type_index,
                                    from: Box::new(encode_guard_clause(
                                        program, encoder, function, guard,
                                    )),
                                    index: 0,
                                }
                            }
                            None => encoder::Instruction::I32Const(1),
                        };

                        let condition = encoder::Instruction::If {
                            type_: BlockType::Result(encoder::ValType::I32),
                            cond: Box::new(condition),
                            then: vec![guard],
                            else_: vec![encoder::Instruction::I32Const(0)],
                        };

                        let then =
                            encode_expression(program, encoder, module, function, &clause.then)
                                .unwrap();

                        encoder::Instruction::If {
                            type_: block_type.clone(),
                            cond: Box::new(condition),
                            then: vec![then],
                            else_: vec![else_],
                        }
                    }),
            );

            Ok(encoder::Instruction::Block {
                type_: block_type.clone(),
                code,
            })
        }
        TypedExpr::RecordAccess { index, record, .. } => {
            let heap_type_index = program.resolve_type_index(encoder, &record.type_());

            let index = u32::try_from(*index + 1).expect("Record is too large");
            let record = encode_expression(program, encoder, module, function, record)?;

            Ok(encoder::Instruction::StructGet {
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

                Ok(encoder::Instruction::StructNew {
                    type_: closure_type_index,
                    args: vec![
                        encoder::Instruction::RefFunc(function_index),
                        encoder::Instruction::StructNew {
                            type_: unit_type_index,
                            args: vec![],
                        },
                    ],
                })
            }
            ModuleValueConstructor::Constant { literal, .. } => match literal {
                Constant::Int { value, .. } => {
                    let value = value.parse().unwrap();

                    encode_int(program, value)
                }
                Constant::Float { value, .. } => {
                    let value = value.parse().unwrap();

                    encode_float(program, value)
                }
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

            Ok(encoder::Instruction::StructNew {
                type_: type_index,
                args: arguments,
            })
        }
        TypedExpr::TupleIndex { tuple, index, .. } => {
            let type_index = program.resolve_type_index(encoder, &tuple.type_());
            let index = u32::try_from(*index).unwrap();

            let tuple = encode_expression(program, encoder, module, function, tuple)?;

            Ok(encoder::Instruction::StructGet {
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
            let bool_type_index = program.resolve_prelude_type_index(PreludeType::Bool);
            let value = encode_expression(program, encoder, module, function, value)?;

            Ok(encoder::Instruction::StructNew {
                type_: bool_type_index,
                args: vec![encoder::Instruction::I32Xor {
                    lhs: Box::new(encoder::Instruction::StructGet {
                        from: Box::new(value),
                        type_: bool_type_index,
                        index: 0,
                    }),
                    rhs: Box::new(encoder::Instruction::I32Const(1)),
                }],
            })
        }
        TypedExpr::NegateInt { value, .. } => {
            let int_type_index = program.resolve_prelude_type_index(PreludeType::Int);
            let value = encode_expression(program, encoder, module, function, value)?;

            Ok(encoder::Instruction::StructNew {
                type_: int_type_index,
                args: vec![encoder::Instruction::I64Sub {
                    lhs: Box::new(encoder::Instruction::I64Const(0)),
                    rhs: Box::new(encoder::Instruction::StructGet {
                        from: Box::new(value),
                        type_: int_type_index,
                        index: 0,
                    }),
                }],
            })
        }
    }
}

fn get_custom_type_definitions(module: &Module) -> impl Iterator<Item = &CustomType<Arc<Type>>> {
    module
        .ast
        .definitions
        .iter()
        .filter_map(|definition| match definition {
            Definition::CustomType(custom_type) => Some(custom_type),
            _ => None,
        })
}

fn get_function_definitions(module: &Module) -> impl Iterator<Item = &TypedFunction> {
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
