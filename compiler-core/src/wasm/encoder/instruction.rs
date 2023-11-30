use super::{BlockType, Data, Encode, Function, HeapType, Index, Local, RefType, Type};

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Instruction {
    // Control Instructions
    End,
    Drop,
    Unreachable,
    Return,
    If(BlockType),
    Else,
    Block(BlockType),
    Call(Index<Function>),
    CallRef(Index<Type>),
    BrTable(Vec<u32>, u32),

    // Varible Instructions
    LocalGet(Index<Local>),
    LocalSet(Index<Local>),
    LocalTee(Index<Local>),

    // Reference Types Instructions
    RefNull(HeapType),
    RefFunc(Index<Function>),
    RefCast(RefType),

    // Numeric Instructions
    I64Add,
    I64Sub,
    I64Mul,
    I64DivS,
    I64RemS,
    I64LtS,
    I64LeS,
    I64GtS,
    I64GeS,
    I64Eq,
    I64Const(i64),
    I32Add,
    I32Xor,
    I32Eq,
    I32Eqz,
    I32Ne,
    I32Const(i32),
    F64Add,
    F64Sub,
    F64Mul,
    F64Div,
    F64Lt,
    F64Le,
    F64Gt,
    F64Ge,
    F64Eq,
    F64Const(f64),

    // GC Instructions
    StructNew(Index<Type>),
    StructGet(Index<Type>, u32),
    ArrayNew(Index<Type>),
    ArrayNewDefault(Index<Type>),
    ArrayNewData(Index<Type>, Index<Data>),
    ArrayLen,
    ArrayCopy(Index<Type>, Index<Type>),
    ArrayInitData(Index<Type>, Index<Data>),
}

impl Instruction {
    pub fn encode<'a>(
        &'a self,
        module: &super::Module,
        encoder: &mut super::Encoder,
    ) -> wasm_encoder::Instruction<'a> {
        match self {
            // Control Instructions
            Instruction::End => wasm_encoder::Instruction::End,
            Instruction::Drop => wasm_encoder::Instruction::Drop,
            Instruction::Unreachable => wasm_encoder::Instruction::Unreachable,
            Instruction::Return => wasm_encoder::Instruction::Return,
            Instruction::If(block_type) => {
                wasm_encoder::Instruction::If(block_type.encode(module, encoder))
            }
            Instruction::Else => wasm_encoder::Instruction::Else,
            Instruction::Block(block_type) => {
                wasm_encoder::Instruction::Block(block_type.encode(module, encoder))
            }
            Instruction::Call(index) => {
                wasm_encoder::Instruction::Call(module.resolve_function_index(*index))
            }
            Instruction::CallRef(index) => {
                wasm_encoder::Instruction::CallRef(module.resolve_type_index(*index))
            }
            Instruction::BrTable(targets, default) => {
                wasm_encoder::Instruction::BrTable(targets.into(), *default)
            }

            // Varible Instructions
            Instruction::LocalGet(index) => wasm_encoder::Instruction::LocalGet(index.get()),
            Instruction::LocalSet(index) => wasm_encoder::Instruction::LocalSet(index.get()),
            Instruction::LocalTee(index) => wasm_encoder::Instruction::LocalTee(index.get()),

            // Reference Types Instructions
            Instruction::RefNull(heap_type) => {
                wasm_encoder::Instruction::RefNull(heap_type.encode(module, encoder))
            }
            Instruction::RefFunc(index) => {
                wasm_encoder::Instruction::RefFunc(module.resolve_function_index(*index))
            }
            Instruction::RefCast(ty) => {
                wasm_encoder::Instruction::RefCast(ty.encode(module, encoder))
            }

            // Numeric Instructions
            Instruction::I64Add => wasm_encoder::Instruction::I64Add,
            Instruction::I64Sub => wasm_encoder::Instruction::I64Sub,
            Instruction::I64Mul => wasm_encoder::Instruction::I64Mul,
            Instruction::I64DivS => wasm_encoder::Instruction::I64DivS,
            Instruction::I64RemS => wasm_encoder::Instruction::I64RemS,
            Instruction::I64LtS => wasm_encoder::Instruction::I64LtS,
            Instruction::I64LeS => wasm_encoder::Instruction::I64LeS,
            Instruction::I64GtS => wasm_encoder::Instruction::I64GtS,
            Instruction::I64GeS => wasm_encoder::Instruction::I64GeS,
            Instruction::I64Eq => wasm_encoder::Instruction::I64Eq,
            Instruction::I64Const(value) => wasm_encoder::Instruction::I64Const(*value),
            Instruction::I32Add => wasm_encoder::Instruction::I32Add,
            Instruction::I32Xor => wasm_encoder::Instruction::I32Xor,
            Instruction::I32Eq => wasm_encoder::Instruction::I32Eq,
            Instruction::I32Eqz => wasm_encoder::Instruction::I32Eqz,
            Instruction::I32Ne => wasm_encoder::Instruction::I32Ne,
            Instruction::I32Const(value) => wasm_encoder::Instruction::I32Const(*value),
            Instruction::F64Add => wasm_encoder::Instruction::F64Add,
            Instruction::F64Sub => wasm_encoder::Instruction::F64Sub,
            Instruction::F64Mul => wasm_encoder::Instruction::F64Mul,
            Instruction::F64Div => wasm_encoder::Instruction::F64Div,
            Instruction::F64Lt => wasm_encoder::Instruction::F64Lt,
            Instruction::F64Le => wasm_encoder::Instruction::F64Le,
            Instruction::F64Gt => wasm_encoder::Instruction::F64Gt,
            Instruction::F64Ge => wasm_encoder::Instruction::F64Ge,
            Instruction::F64Eq => wasm_encoder::Instruction::F64Eq,
            Instruction::F64Const(value) => wasm_encoder::Instruction::F64Const(*value),

            // GC Instructions
            Instruction::StructNew(heap_type) => {
                wasm_encoder::Instruction::StructNew(module.resolve_type_index(*heap_type))
            }
            Instruction::StructGet(heap_type, index) => {
                wasm_encoder::Instruction::StructGet(module.resolve_type_index(*heap_type), *index)
            }
            Instruction::ArrayNew(heap_type) => {
                wasm_encoder::Instruction::ArrayNew(module.resolve_type_index(*heap_type))
            }
            Instruction::ArrayNewDefault(heap_type) => {
                wasm_encoder::Instruction::ArrayNewDefault(module.resolve_type_index(*heap_type))
            }
            Instruction::ArrayNewData(heap_type, data) => wasm_encoder::Instruction::ArrayNewData(
                module.resolve_type_index(*heap_type),
                data.get(),
            ),
            Instruction::ArrayLen => wasm_encoder::Instruction::ArrayLen,
            Instruction::ArrayCopy(src_type, dst_type) => wasm_encoder::Instruction::ArrayCopy(
                module.resolve_type_index(*src_type),
                module.resolve_type_index(*dst_type),
            ),
            Instruction::ArrayInitData(heap_type, data) => {
                wasm_encoder::Instruction::ArrayInitData(
                    module.resolve_type_index(*heap_type),
                    data.get(),
                )
            }
        }
    }
}
