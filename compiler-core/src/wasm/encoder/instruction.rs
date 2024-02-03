use itertools::Itertools;

use super::{BlockType, Data, Encode, Function, HeapType, Index, Local, RefType, Type};

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Instruction {
    // Control Instructions
    End,
    Drop(Box<Self>),
    Unreachable,
    Return(Option<Box<Self>>),
    If {
        type_: BlockType,
        cond: Box<Self>,
        then: Vec<Self>,
        else_: Vec<Self>,
    },
    Block {
        type_: BlockType,
        code: Vec<Self>,
    },
    Call {
        func: Index<Function>,
        args: Vec<Self>,
    },
    CallRef {
        type_: Index<Type>,
        ref_: Box<Self>,
        args: Vec<Self>,
    },
    BrTable(Vec<u32>, u32),

    // Varible Instructions
    LocalGet(Index<Local>),
    LocalSet {
        local: Index<Local>,
        value: Box<Self>,
    },
    LocalTee {
        local: Index<Local>,
        value: Box<Self>,
    },

    // Reference Types Instructions
    RefNull(HeapType),
    RefFunc(Index<Function>),
    RefCast(RefType),

    // Numeric Instructions
    I64Add {
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    I64Sub {
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    I64Mul {
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    I64DivS {
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    I64RemS {
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    I64LtS {
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    I64LeS {
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    I64GtS {
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    I64GeS {
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    I64Eq {
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    I64Const(i64),
    I32Add {
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    I32Xor {
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    I32Eq {
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    I32Eqz(Box<Self>),
    I32Ne {
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    I32Const(i32),
    F64Add {
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    F64Sub {
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    F64Mul {
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    F64Div {
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    F64Lt {
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    F64Le {
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    F64Gt {
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    F64Ge {
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    F64Eq {
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    F64Const(f64),

    // GC Instructions
    StructNew {
        type_: Index<Type>,
        args: Vec<Self>,
    },
    StructGet {
        from: Box<Self>,
        type_: Index<Type>,
        index: u32,
    },
    ArrayNew(Index<Type>),
    ArrayNewDefault(Index<Type>),
    ArrayNewData(Index<Type>, Index<Data>),
    ArrayGetU {
        array: Box<Self>,
        index: i32,
        type_: Index<Type>,
    },
    ArrayLen(Box<Self>),
    ArrayCopy(Index<Type>, Index<Type>),
    ArrayInitData(Index<Type>, Index<Data>),
}

impl Instruction {
    fn encode_binary_op<'a>(
        module: &super::Module,
        encoder: &mut super::Encoder,
        lhs: &'a Self,
        rhs: &'a Self,
        operation: wasm_encoder::Instruction<'a>,
    ) -> Vec<wasm_encoder::Instruction<'a>> {
        let mut encoded = lhs.encode(module, encoder);
        encoded.append(&mut rhs.encode(module, encoder));
        encoded.push(operation);
        encoded
    }

    pub fn encode<'a>(
        &'a self,
        module: &super::Module,
        encoder: &mut super::Encoder,
    ) -> Vec<wasm_encoder::Instruction<'a>> {
        match self {
            // Control Instructions
            Instruction::End => vec![wasm_encoder::Instruction::End],
            Instruction::Drop(value) => {
                let mut encoded = value.encode(module, encoder);
                encoded.push(wasm_encoder::Instruction::Drop);
                encoded
            }
            Instruction::Unreachable => vec![wasm_encoder::Instruction::Unreachable],
            Instruction::Return(value) => {
                let mut encoded = if let Some(value) = value {
                    value.encode(module, encoder)
                } else {
                    vec![]
                };
                encoded.push(wasm_encoder::Instruction::Return);
                encoded
            }
            Instruction::If {
                type_,
                cond,
                then,
                else_,
            } => {
                let mut encoded = cond.encode(module, encoder);
                encoded.push(wasm_encoder::Instruction::If(type_.encode(module, encoder)));
                encoded.extend(then.iter().flat_map(|code| code.encode(module, encoder)));
                if else_.len() > 0 {
                    encoded.push(wasm_encoder::Instruction::Else);
                    encoded.extend(else_.iter().flat_map(|code| code.encode(module, encoder)));
                }
                encoded.push(wasm_encoder::Instruction::End);
                encoded
            }
            Instruction::Block { type_, code } => {
                let mut encoded = vec![wasm_encoder::Instruction::Block(
                    type_.encode(module, encoder),
                )];
                encoded.extend(code.iter().flat_map(|code| code.encode(module, encoder)));
                encoded.push(wasm_encoder::Instruction::End);
                encoded
            }
            Instruction::Call { func, args } => {
                let mut encoded = args
                    .iter()
                    .flat_map(|arg| arg.encode(module, encoder))
                    .collect_vec();
                encoded.push(wasm_encoder::Instruction::Call(
                    module.resolve_function_index(*func),
                ));
                encoded
            }
            Instruction::CallRef { type_, ref_, args } => {
                let mut encoded = args
                    .iter()
                    .flat_map(|arg| arg.encode(module, encoder))
                    .collect_vec();
                encoded.append(&mut ref_.encode(module, encoder));
                encoded.push(wasm_encoder::Instruction::CallRef(
                    module.resolve_type_index(*type_),
                ));
                encoded
            }
            Instruction::BrTable(targets, default) => {
                vec![wasm_encoder::Instruction::BrTable(targets.into(), *default)]
            }

            // Varible Instructions
            Instruction::LocalGet(index) => vec![wasm_encoder::Instruction::LocalGet(index.get())],
            Instruction::LocalSet { local, value } => {
                let mut encoded = value.encode(module, encoder);
                encoded.push(wasm_encoder::Instruction::LocalSet(local.get()));
                encoded
            }
            Instruction::LocalTee { local, value } => {
                let mut encoded = value.encode(module, encoder);
                encoded.push(wasm_encoder::Instruction::LocalTee(local.get()));
                encoded
            }

            // Reference Types Instructions
            Instruction::RefNull(heap_type) => {
                vec![wasm_encoder::Instruction::RefNull(
                    heap_type.encode(module, encoder),
                )]
            }
            Instruction::RefFunc(index) => {
                vec![wasm_encoder::Instruction::RefFunc(
                    module.resolve_function_index(*index),
                )]
            }
            Instruction::RefCast(ty) => {
                vec![wasm_encoder::Instruction::RefCast(
                    ty.encode(module, encoder),
                )]
            }

            // Numeric Instructions
            Instruction::I64Add { lhs, rhs } => {
                Self::encode_binary_op(module, encoder, lhs, rhs, wasm_encoder::Instruction::I64Add)
            }
            Instruction::I64Sub { lhs, rhs } => {
                Self::encode_binary_op(module, encoder, lhs, rhs, wasm_encoder::Instruction::I64Sub)
            }
            Instruction::I64Mul { lhs, rhs } => {
                Self::encode_binary_op(module, encoder, lhs, rhs, wasm_encoder::Instruction::I64Mul)
            }
            Instruction::I64DivS { lhs, rhs } => Self::encode_binary_op(
                module,
                encoder,
                lhs,
                rhs,
                wasm_encoder::Instruction::I64DivS,
            ),
            Instruction::I64RemS { lhs, rhs } => Self::encode_binary_op(
                module,
                encoder,
                lhs,
                rhs,
                wasm_encoder::Instruction::I64RemS,
            ),
            Instruction::I64LtS { lhs, rhs } => {
                Self::encode_binary_op(module, encoder, lhs, rhs, wasm_encoder::Instruction::I64LtS)
            }
            Instruction::I64LeS { lhs, rhs } => {
                Self::encode_binary_op(module, encoder, lhs, rhs, wasm_encoder::Instruction::I64LeS)
            }
            Instruction::I64GtS { lhs, rhs } => {
                Self::encode_binary_op(module, encoder, lhs, rhs, wasm_encoder::Instruction::I64GtS)
            }
            Instruction::I64GeS { lhs, rhs } => {
                Self::encode_binary_op(module, encoder, lhs, rhs, wasm_encoder::Instruction::I64GeS)
            }
            Instruction::I64Eq { lhs, rhs } => {
                Self::encode_binary_op(module, encoder, lhs, rhs, wasm_encoder::Instruction::I64Eq)
            }
            Instruction::I64Const(value) => vec![wasm_encoder::Instruction::I64Const(*value)],
            Instruction::I32Add { lhs, rhs } => {
                Self::encode_binary_op(module, encoder, lhs, rhs, wasm_encoder::Instruction::I32Add)
            }
            Instruction::I32Xor { lhs, rhs } => {
                Self::encode_binary_op(module, encoder, lhs, rhs, wasm_encoder::Instruction::I32Xor)
            }
            Instruction::I32Eq { lhs, rhs } => {
                Self::encode_binary_op(module, encoder, lhs, rhs, wasm_encoder::Instruction::I32Eq)
            }
            Instruction::I32Eqz(value) => {
                let mut encoded = value.encode(module, encoder);
                encoded.push(wasm_encoder::Instruction::I32Eqz);
                encoded
            }
            Instruction::I32Ne { lhs, rhs } => {
                Self::encode_binary_op(module, encoder, lhs, rhs, wasm_encoder::Instruction::I32Ne)
            }
            Instruction::I32Const(value) => vec![wasm_encoder::Instruction::I32Const(*value)],
            Instruction::F64Add { lhs, rhs } => {
                Self::encode_binary_op(module, encoder, lhs, rhs, wasm_encoder::Instruction::F64Add)
            }
            Instruction::F64Sub { lhs, rhs } => {
                Self::encode_binary_op(module, encoder, lhs, rhs, wasm_encoder::Instruction::F64Sub)
            }
            Instruction::F64Mul { lhs, rhs } => {
                Self::encode_binary_op(module, encoder, lhs, rhs, wasm_encoder::Instruction::F64Mul)
            }
            Instruction::F64Div { lhs, rhs } => {
                Self::encode_binary_op(module, encoder, lhs, rhs, wasm_encoder::Instruction::F64Div)
            }
            Instruction::F64Lt { lhs, rhs } => {
                Self::encode_binary_op(module, encoder, lhs, rhs, wasm_encoder::Instruction::F64Lt)
            }
            Instruction::F64Le { lhs, rhs } => {
                Self::encode_binary_op(module, encoder, lhs, rhs, wasm_encoder::Instruction::F64Le)
            }
            Instruction::F64Gt { lhs, rhs } => {
                Self::encode_binary_op(module, encoder, lhs, rhs, wasm_encoder::Instruction::F64Gt)
            }
            Instruction::F64Ge { lhs, rhs } => {
                Self::encode_binary_op(module, encoder, lhs, rhs, wasm_encoder::Instruction::F64Ge)
            }
            Instruction::F64Eq { lhs, rhs } => {
                Self::encode_binary_op(module, encoder, lhs, rhs, wasm_encoder::Instruction::F64Eq)
            }
            Instruction::F64Const(value) => vec![wasm_encoder::Instruction::F64Const(*value)],

            // GC Instructions
            Instruction::StructNew { type_, args } => {
                let mut encoded = args
                    .iter()
                    .flat_map(|arg| arg.encode(module, encoder))
                    .collect_vec();
                encoded.push(wasm_encoder::Instruction::StructNew(
                    module.resolve_type_index(*type_),
                ));
                encoded
            }
            Instruction::StructGet { from, type_, index } => {
                let mut encoded = from.encode(module, encoder);
                encoded.push(wasm_encoder::Instruction::StructGet(
                    module.resolve_type_index(*type_),
                    *index,
                ));
                encoded
            }
            Instruction::ArrayNew(heap_type) => {
                vec![wasm_encoder::Instruction::ArrayNew(
                    module.resolve_type_index(*heap_type),
                )]
            }
            Instruction::ArrayNewDefault(heap_type) => {
                vec![wasm_encoder::Instruction::ArrayNewDefault(
                    module.resolve_type_index(*heap_type),
                )]
            }
            Instruction::ArrayNewData(heap_type, data) => {
                vec![wasm_encoder::Instruction::ArrayNewData(
                    module.resolve_type_index(*heap_type),
                    data.get(),
                )]
            }
            Instruction::ArrayGetU {
                array,
                index,
                type_,
            } => {
                let mut encoded = array.encode(module, encoder);
                encoded.push(wasm_encoder::Instruction::I32Const(*index));
                encoded.push(wasm_encoder::Instruction::ArrayGetU(type_.get()));
                encoded
            }
            Instruction::ArrayLen(array) => {
                let mut encoded = array.encode(module, encoder);
                encoded.push(wasm_encoder::Instruction::ArrayLen);
                encoded
            }
            Instruction::ArrayCopy(src_type, dst_type) => {
                vec![wasm_encoder::Instruction::ArrayCopy(
                    module.resolve_type_index(*src_type),
                    module.resolve_type_index(*dst_type),
                )]
            }
            Instruction::ArrayInitData(heap_type, data) => {
                vec![wasm_encoder::Instruction::ArrayInitData(
                    module.resolve_type_index(*heap_type),
                    data.get(),
                )]
            }
        }
    }
}
