use ecow::EcoString;

use crate::wasm::{encoder, Program};

pub fn register(program: &mut Program, encoder: &mut encoder::Module) {
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

        function.instruction(encoder::Instruction::StructNew {
            type_: gleam_int_index,
            args: vec![encoder::Instruction::LocalGet(param_index)],
        });
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

        function.instruction(encoder::Instruction::I64Eq {
            lhs: Box::new(encoder::Instruction::StructGet {
                from: Box::new(encoder::Instruction::LocalGet(lhs_index)),
                type_: gleam_int_index,
                index: 0,
            }),
            rhs: Box::new(encoder::Instruction::StructGet {
                from: Box::new(encoder::Instruction::LocalGet(rhs_index)),
                type_: gleam_int_index,
                index: 0,
            }),
        });
        function.instruction(encoder::Instruction::End);
        function
    };

    encoder.define_function(is_eq_function_index, is_eq_function);
    program.register_type_equality_index(gleam_int_index, is_eq_function_index);

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

        function.instruction(encoder::Instruction::StructGet {
            from: Box::new(encoder::Instruction::LocalGet(param_index)),
            type_: gleam_int_index,
            index: 0,
        });
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

        function.instruction(encoder::Instruction::StructNew {
            type_: gleam_float_index,
            args: vec![encoder::Instruction::LocalGet(param_index)],
        });
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

        function.instruction(encoder::Instruction::F64Eq {
            lhs: Box::new(encoder::Instruction::StructGet {
                from: Box::new(encoder::Instruction::LocalGet(lhs_index)),
                type_: gleam_float_index,
                index: 0,
            }),
            rhs: Box::new(encoder::Instruction::StructGet {
                from: Box::new(encoder::Instruction::LocalGet(rhs_index)),
                type_: gleam_float_index,
                index: 0,
            }),
        });
        function.instruction(encoder::Instruction::End);
        function
    };

    encoder.define_function(is_eq_function_index, is_eq_function);
    program.register_type_equality_index(gleam_float_index, is_eq_function_index);

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

        function.instruction(encoder::Instruction::StructGet {
            from: Box::new(encoder::Instruction::LocalGet(param_index)),
            type_: gleam_float_index,
            index: 0,
        });
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

        function.instruction(encoder::Instruction::StructNew {
            type_: gleam_bool_index,
            args: vec![encoder::Instruction::I32Const(1)],
        });
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

        function.instruction(encoder::Instruction::StructNew {
            type_: gleam_bool_index,
            args: vec![encoder::Instruction::I32Const(0)],
        });
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

        function.instruction(encoder::Instruction::I32Eq {
            lhs: Box::new(encoder::Instruction::StructGet {
                from: Box::new(encoder::Instruction::LocalGet(lhs_index)),
                type_: gleam_bool_index,
                index: 0,
            }),
            rhs: Box::new(encoder::Instruction::StructGet {
                from: Box::new(encoder::Instruction::LocalGet(rhs_index)),
                type_: gleam_bool_index,
                index: 0,
            }),
        });
        function.instruction(encoder::Instruction::End);
        function
    };

    encoder.define_function(is_eq_function_index, is_eq_function);
    program.register_type_equality_index(gleam_bool_index, is_eq_function_index);

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

        function.instruction(encoder::Instruction::StructGet {
            from: Box::new(encoder::Instruction::LocalGet(param_index)),
            type_: gleam_bool_index,
            index: 0,
        });
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
