use crate::type_::prelude;
use crate::wasm::encoder;

use super::Program;

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
