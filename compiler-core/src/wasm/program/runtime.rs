use ecow::EcoString;

use crate::type_::prelude::{self, PreludeType};
use crate::wasm::encoder;

use super::Program;

pub const RUNTIME_MODULE: &'static str = "_$runtime";
pub const RUNTIME_CLOSURE_TYPE: &'static str = "Closure";
pub const RUNTIME_UNIT_TYPE: &'static str = "Unit";

pub const PRELUDE_MODULE: &'static str = prelude::PRELUDE_MODULE_NAME;

pub fn register(program: &mut Program, encoder: &mut encoder::Module) {
    register_unit(program, encoder);
    register_closure(program, encoder);
    register_prelude_nil(program, encoder);
    register_prelude_int(program, encoder);
    register_prelude_float(program, encoder);
    register_prelude_bool(program, encoder);
    register_prelude_list(program, encoder);
    register_prelude_string(program, encoder);
}

pub fn unit_type_index(program: &Program) -> encoder::Index<encoder::Type> {
    program.resolve_type_index_by_name(RUNTIME_MODULE.into(), RUNTIME_UNIT_TYPE.into())
}

fn register_unit(program: &mut Program, encoder: &mut encoder::Module) {
    let unit_type_index = encoder.declare_type();
    let unit_type = encoder::Type::Struct { fields: vec![] };

    encoder.define_type(unit_type_index, unit_type);
    program.register_type_index(RUNTIME_MODULE, RUNTIME_UNIT_TYPE, unit_type_index);
}

pub fn closure_type_index(program: &Program) -> encoder::Index<encoder::Type> {
    program.resolve_type_index_by_name(RUNTIME_MODULE.into(), RUNTIME_CLOSURE_TYPE.into())
}

fn register_closure(program: &mut Program, encoder: &mut encoder::Module) {
    let closure_struct_type_index = encoder.declare_type();
    let closure_struct_type = encoder::Type::Struct {
        fields: vec![
            encoder::FieldType {
                element_type: encoder::StorageType::Val(encoder::ValType::Ref(encoder::RefType {
                    nullable: true,
                    heap_type: encoder::HeapType::Func,
                })),
                mutable: true,
            },
            encoder::FieldType {
                element_type: encoder::StorageType::Val(encoder::ValType::Ref(encoder::RefType {
                    nullable: true,
                    heap_type: encoder::HeapType::Struct,
                })),
                mutable: true,
            },
        ],
    };

    encoder.define_type(closure_struct_type_index, closure_struct_type);
    program.register_type_index(
        RUNTIME_MODULE,
        RUNTIME_CLOSURE_TYPE,
        closure_struct_type_index,
    );
}

fn register_prelude_nil(program: &mut Program, encoder: &mut encoder::Module) {
    let gleam_nil_index = encoder.declare_type();
    let gleam_nil = encoder::Type::Struct { fields: vec![] };

    encoder.define_type(gleam_nil_index, gleam_nil);
    program.register_type_index(PRELUDE_MODULE, PreludeType::Nil.name(), gleam_nil_index);

    let gleam_nil_constructor_type_index = encoder.declare_type();
    let gleam_nil_constructor_type = encoder::Type::Function {
        params: vec![encoder::ValType::Ref(encoder::RefType {
            nullable: true,
            heap_type: encoder::HeapType::Struct,
        })],
        results: vec![encoder::ValType::Ref(encoder::RefType {
            nullable: true,
            heap_type: encoder::HeapType::Concrete(gleam_nil_index),
        })],
    };

    encoder.define_type(gleam_nil_constructor_type_index, gleam_nil_constructor_type);

    let gleam_nil_constructor_index = encoder.declare_function();
    let gleam_nil_constructor = {
        let mut function = encoder::Function::new(
            gleam_nil_constructor_index,
            gleam_nil_constructor_type_index,
            format!("{PRELUDE_MODULE}/{}", PreludeType::Nil.name()).into(),
            encoder::FunctionLinkage::Local,
            vec![],
        );

        function.instruction(encoder::Instruction::StructNew {
            type_: gleam_nil_index,
            args: vec![],
        });
        function.instruction(encoder::Instruction::End);
        function
    };

    encoder.define_function(gleam_nil_constructor_index, gleam_nil_constructor);
    program.register_function_index(
        PRELUDE_MODULE,
        PreludeType::Nil.name(),
        gleam_nil_constructor_index,
    );
}

pub fn int_constructor(program: &Program) -> encoder::Index<encoder::Function> {
    program.resolve_function_index_by_name(PRELUDE_MODULE.into(), PreludeType::Int.name().into())
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
    program.register_type_index(PRELUDE_MODULE, PreludeType::Int.name(), gleam_int_index);

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
            format!("{PRELUDE_MODULE}/{}", PreludeType::Int.name()).into(),
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
    program.register_function_index(
        PRELUDE_MODULE,
        PreludeType::Int.name(),
        gleam_int_constructor_index,
    );

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
            format!("{PRELUDE_MODULE}/{}$eq", PreludeType::Int.name()).into(),
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
            format!("{PRELUDE_MODULE}/{}$to_int", PreludeType::Int.name()).into(),
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

pub fn float_constructor(program: &Program) -> encoder::Index<encoder::Function> {
    program.resolve_function_index_by_name("gleam".into(), "Float".into())
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
        params: vec![encoder::ValType::Ref(encoder::RefType {
            nullable: true,
            heap_type: encoder::HeapType::Struct,
        })],
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
            vec![None],
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

pub fn list_type_index(program: &Program) -> encoder::Index<encoder::Type> {
    program.resolve_type_index_by_name("gleam".into(), "List".into())
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

pub fn string_compare(
    program: &mut Program,
    encoder: &mut encoder::Module,
    function: &mut encoder::Function,
    string: &str,
    with: encoder::Instruction,
) -> encoder::Instruction {
    let gleam_string_type_index = program.resolve_type_index(encoder, &prelude::string());
    let gleam_string_type = program.resolve_type(encoder, &prelude::string());

    let local = function.declare_anonymous_local(gleam_string_type);

    let do_strings_match = string.bytes().enumerate().rfold(
        encoder::Instruction::I32Const(1),
        |then, (index, byte)| encoder::Instruction::If {
            type_: encoder::BlockType::Result(encoder::ValType::I32),
            cond: Box::new(encoder::Instruction::I32Eq {
                lhs: Box::new(encoder::Instruction::ArrayGetU {
                    array: Box::new(encoder::Instruction::LocalGet(local)),
                    index: index.try_into().unwrap(),
                    type_: gleam_string_type_index,
                }),
                rhs: Box::new(encoder::Instruction::I32Const(byte.into())),
            }),
            then: vec![then],
            else_: vec![encoder::Instruction::I32Const(0)],
        },
    );

    encoder::Instruction::Block {
        type_: encoder::BlockType::Result(encoder::ValType::I32),
        code: vec![
            encoder::Instruction::LocalSet {
                local,
                value: Box::new(with),
            },
            do_strings_match,
        ],
    }
}
