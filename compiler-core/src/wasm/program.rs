use crate::wasm::encoder::Index;
use crate::{
    type_::{Type, TypeVar},
    wasm::encoder,
};
use ecow::EcoString;
use im::HashMap;
use itertools::Itertools;
use std::ops::Deref;

pub mod prelude;

type TypeIndexCache<T> = HashMap<T, Index<encoder::Type>>;

#[derive(Debug, Default)]
pub struct Program {
    type_indices: TypeIndexCache<(EcoString, EcoString)>,
    type_equality_indices: HashMap<Index<encoder::Type>, Index<encoder::Function>>,
    type_variant_tags: HashMap<(Index<encoder::Type>, EcoString), i32>,
    function_type_indices_cache: TypeIndexCache<(Vec<Index<encoder::Type>>, Index<encoder::Type>)>,
    tuple_type_indices_cache: TypeIndexCache<Vec<Index<encoder::Type>>>,
    function_indices: HashMap<(EcoString, EcoString), Index<encoder::Function>>,
}

impl Program {
    pub fn register_type_index(
        &mut self,
        module: impl Into<EcoString>,
        name: impl Into<EcoString>,
        index: Index<encoder::Type>,
    ) {
        _ = self
            .type_indices
            .insert((module.into(), name.into()), index);
    }

    pub fn register_type_equality_index(
        &mut self,
        type_index: Index<encoder::Type>,
        function_index: Index<encoder::Function>,
    ) {
        _ = self
            .type_equality_indices
            .insert(type_index, function_index);
    }

    pub fn register_type_variant_tag(
        &mut self,
        type_: Index<encoder::Type>,
        name: impl Into<EcoString>,
        tag: i32,
    ) {
        _ = self.type_variant_tags.insert((type_, name.into()), tag);
    }

    pub fn register_function_index(
        &mut self,
        module: impl Into<EcoString>,
        name: impl Into<EcoString>,
        index: Index<encoder::Function>,
    ) {
        _ = self
            .function_indices
            .insert((module.into(), name.into()), index);
    }

    pub fn resolve_equality_index(
        &self,
        type_index: Index<encoder::Type>,
    ) -> Index<encoder::Function> {
        self.type_equality_indices
            .get(&type_index)
            .copied()
            .expect("Type's equality function should already be declared")
    }

    pub fn resolve_type_variant_tag(
        &self,
        type_index: Index<encoder::Type>,
        name: impl Into<EcoString>,
    ) -> i32 {
        self.type_variant_tags
            .get(&(type_index, name.into()))
            .copied()
            .expect("Type variant tag should already be declared")
    }

    pub fn resolve_type(
        &mut self,
        encoder: &mut encoder::Module,
        type_: &Type,
    ) -> encoder::ValType {
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

    pub fn resolve_type_index(
        &mut self,
        encoder: &mut encoder::Module,
        type_: &Type,
    ) -> Index<encoder::Type> {
        match type_ {
            Type::Named { module, name, .. } => {
                self.resolve_type_index_by_name(module.clone(), name.clone())
            }
            Type::Fn { args, retrn, .. } => {
                let arg_indices = args
                    .iter()
                    .map(|arg| self.resolve_type_index(encoder, arg))
                    .collect_vec();

                let return_index = self.resolve_type_index(encoder, &retrn);

                // TODO: This is only required in `or_insert_with`, however,
                //       due to lifetime rules, we cannot call `resolve_type`.
                //       In the future it might be worth revisiting this
                //       in case of any performance issues.
                let fn_type = encoder::Type::Function {
                    params: args
                        .iter()
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
                let elem_indices = elems
                    .iter()
                    .map(|elem| self.resolve_type_index(encoder, elem))
                    .collect_vec();

                let tuple_type = encoder::Type::Struct {
                    fields: elems
                        .iter()
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

    pub fn resolve_type_index_by_name(
        &self,
        module: EcoString,
        name: EcoString,
    ) -> Index<encoder::Type> {
        self.type_indices
            .get(&(module.clone(), name.clone()))
            .copied()
            .unwrap_or_else(|| panic!("Type index should be defined for '{module}.{name}'",))
    }

    pub fn resolve_function_index_by_name(
        &self,
        module: EcoString,
        name: EcoString,
    ) -> Index<encoder::Function> {
        self.function_indices
            .get(&(module.clone(), name.clone()))
            .copied()
            .unwrap_or_else(|| panic!("Function index should be defined for '{module}.{name}'",))
    }
}
