use itertools::Itertools;

use super::{Encode, Encoder, Index, Module};

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum BlockType {
    Empty,
    Result(ValType),
    FunctionType(Index<Type>),
}

impl Encode for BlockType {
    type Output = wasm_encoder::BlockType;

    fn encode(&self, module: &Module, encoder: &mut Encoder) -> Self::Output {
        match self {
            BlockType::Empty => wasm_encoder::BlockType::Empty,
            BlockType::Result(val_type) => {
                wasm_encoder::BlockType::Result(val_type.encode(module, encoder))
            }
            BlockType::FunctionType(index) => wasm_encoder::BlockType::FunctionType(index.get()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub enum ValType {
    I32,
    I64,
    F32,
    F64,
    V128,
    Ref(RefType),
}

impl Encode for ValType {
    type Output = wasm_encoder::ValType;

    fn encode(&self, module: &Module, encoder: &mut Encoder) -> Self::Output {
        match self {
            ValType::I32 => wasm_encoder::ValType::I32,
            ValType::I64 => wasm_encoder::ValType::I64,
            ValType::F32 => wasm_encoder::ValType::F32,
            ValType::F64 => wasm_encoder::ValType::F64,
            ValType::V128 => wasm_encoder::ValType::V128,
            ValType::Ref(ref_type) => wasm_encoder::ValType::Ref(ref_type.encode(module, encoder)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RefType {
    pub nullable: bool,
    pub heap_type: HeapType,
}

impl Encode for RefType {
    type Output = wasm_encoder::RefType;

    fn encode(&self, module: &Module, encoder: &mut Encoder) -> Self::Output {
        wasm_encoder::RefType {
            nullable: self.nullable,
            heap_type: self.heap_type.encode(module, encoder),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub enum HeapType {
    Func,
    Extern,
    Any,
    None,
    NoExtern,
    NoFunc,
    Eq,
    Struct,
    Array,
    I31,
    Concrete(Index<Type>),
}

impl Encode for HeapType {
    type Output = wasm_encoder::HeapType;

    fn encode(&self, module: &Module, _encoder: &mut Encoder) -> Self::Output {
        match self {
            HeapType::Func => wasm_encoder::HeapType::Func,
            HeapType::Extern => wasm_encoder::HeapType::Extern,
            HeapType::Any => wasm_encoder::HeapType::Any,
            HeapType::None => wasm_encoder::HeapType::None,
            HeapType::NoExtern => wasm_encoder::HeapType::NoExtern,
            HeapType::NoFunc => wasm_encoder::HeapType::NoFunc,
            HeapType::Eq => wasm_encoder::HeapType::Eq,
            HeapType::Struct => wasm_encoder::HeapType::Struct,
            HeapType::Array => wasm_encoder::HeapType::Array,
            HeapType::I31 => wasm_encoder::HeapType::I31,
            HeapType::Concrete(index) => {
                wasm_encoder::HeapType::Concrete(module.resolve_type_index(*index))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct SubType {
    pub is_final: bool,
    pub supertype_idx: Option<Index<Type>>,
    pub structural_type: CompositeType,
}

impl Encode for SubType {
    type Output = wasm_encoder::SubType;

    fn encode(&self, module: &Module, encoder: &mut Encoder) -> Self::Output {
        wasm_encoder::SubType {
            is_final: self.is_final,
            supertype_idx: self
                .supertype_idx
                .map(|index| module.resolve_type_index(index)),
            composite_type: self.structural_type.encode(module, encoder),
        }
    }
}

#[derive(Debug, Clone)]
pub enum CompositeType {
    Struct(StructType),
}

impl Encode for CompositeType {
    type Output = wasm_encoder::CompositeType;

    fn encode(&self, module: &Module, encoder: &mut Encoder) -> Self::Output {
        match self {
            CompositeType::Struct(struct_type) => {
                wasm_encoder::CompositeType::Struct(struct_type.encode(module, encoder))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructType {
    pub fields: Vec<FieldType>,
}

impl Encode for StructType {
    type Output = wasm_encoder::StructType;

    fn encode(&self, module: &Module, encoder: &mut Encoder) -> Self::Output {
        let fields = (self.fields.iter())
            .map(|field| field.encode(module, encoder))
            .collect::<Box<[_]>>();

        wasm_encoder::StructType { fields }
    }
}

#[derive(Debug, Clone)]
pub struct FieldType {
    pub element_type: StorageType,
    pub mutable: bool,
}

impl Encode for FieldType {
    type Output = wasm_encoder::FieldType;

    fn encode(&self, module: &Module, encoder: &mut Encoder) -> Self::Output {
        wasm_encoder::FieldType {
            element_type: self.element_type.encode(module, encoder),
            mutable: self.mutable,
        }
    }
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum StorageType {
    I8,
    I16,
    Val(ValType),
}

impl Encode for StorageType {
    type Output = wasm_encoder::StorageType;

    fn encode(&self, module: &Module, encoder: &mut Encoder) -> Self::Output {
        match self {
            StorageType::I8 => wasm_encoder::StorageType::I8,
            StorageType::I16 => wasm_encoder::StorageType::I16,
            StorageType::Val(val_type) => {
                wasm_encoder::StorageType::Val(val_type.encode(module, encoder))
            }
        }
    }
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Type {
    SubType(SubType),
    Struct {
        fields: Vec<FieldType>,
    },
    Function {
        params: Vec<ValType>,
        results: Vec<ValType>,
    },
    Array {
        element_type: StorageType,
        mutable: bool,
    },
    Rec(Vec<SubType>),
    RecElement,
}

impl Encode for Type {
    type Output = ();

    fn encode(&self, module: &Module, encoder: &mut Encoder) -> Self::Output {
        match self {
            Type::Function { params, results } => {
                let params = (params.iter())
                    .map(|param| param.encode(module, encoder))
                    .collect_vec();

                let results = (results.iter())
                    .map(|result| result.encode(module, encoder))
                    .collect_vec();

                _ = encoder.type_section.function(params, results);
            }
            Type::Struct { fields } => {
                let fields = (fields.iter())
                    .map(|field| field.encode(module, encoder))
                    .collect_vec();

                _ = encoder.type_section.struct_(fields);
            }
            Type::SubType(subtype) => {
                let subtype = subtype.encode(module, encoder);

                _ = encoder.type_section.subtype(&subtype);
            }
            Type::Array {
                element_type,
                mutable,
            } => {
                let element_type = element_type.encode(module, encoder);

                _ = encoder.type_section.array(&element_type, *mutable);
            }
            Type::RecElement => (),
            Type::Rec(subtypes) => {
                let subtypes = subtypes
                    .iter()
                    .map(|subtype| subtype.encode(module, encoder))
                    .collect_vec();

                _ = encoder.type_section.rec(subtypes);
            }
        }
    }
}
