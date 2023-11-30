mod function;
mod index;
mod instruction;
mod types;

pub use function::*;
pub use index::*;
pub use instruction::*;
pub use types::*;

pub struct Local;

#[derive(Debug, Clone)]
pub struct Data {
    bytes: Vec<u8>,
}

impl Data {
    pub fn new(bytes: impl Into<Vec<u8>>) -> Self {
        Self {
            bytes: bytes.into(),
        }
    }
}

#[derive(Debug, Default)]
pub struct Module {
    type_map: im::OrdMap<Index<Type>, Option<Type>>,
    data_map: im::OrdMap<Index<Data>, Option<Data>>,
    function_map: im::OrdMap<Index<Function>, Option<Function>>,
}

impl Module {
    fn declare<T: Clone>(
        map: &mut im::OrdMap<Index<T>, Option<T>>,
        kind: &'static str,
    ) -> Index<T> {
        let count = u32::try_from(map.len())
            .unwrap_or_else(|_| panic!("Exceeded maximum number of {kind}"));
        let index = Index::new(count);
        _ = map.insert(index, None);
        index
    }

    fn define<T: Clone>(map: &mut im::OrdMap<Index<T>, Option<T>>, index: Index<T>, value: T) {
        _ = map.insert(index, Some(value));
    }

    pub fn declare_type(&mut self) -> Index<Type> {
        Self::declare(&mut self.type_map, "types")
    }

    pub fn define_type(&mut self, index: Index<Type>, type_: Type) {
        Self::define(&mut self.type_map, index, type_);
    }

    pub fn declare_function(&mut self) -> Index<Function> {
        Self::declare(&mut self.function_map, "functions")
    }

    pub fn define_function(&mut self, index: Index<Function>, function: Function) {
        Self::define(&mut self.function_map, index, function);
    }

    pub fn declare_data(&mut self) -> Index<Data> {
        Self::declare(&mut self.data_map, "data")
    }

    pub fn define_data(&mut self, index: Index<Data>, data: Data) {
        Self::define(&mut self.data_map, index, data);
    }

    fn resolve_type_index(&self, index: Index<Type>) -> u32 {
        index.get()
    }

    fn resolve_function_index(&self, index: Index<Function>) -> u32 {
        index.get()
    }

    pub fn finish(self) -> Vec<u8> {
        let mut encoder = Encoder::default();

        for (_type_index, type_) in &self.type_map {
            let type_ = type_.as_ref().expect("Type should be defined");

            type_.encode(&self, &mut encoder);
        }

        for (_function_index, function) in &self.function_map {
            let function = function.as_ref().expect("Function should be defined");

            function.encode(&self, &mut encoder);
        }

        for (_data_index, data) in &self.data_map {
            let data = data.as_ref().expect("Data should be defined");

            _ = encoder.data_section.passive(data.bytes.clone());
        }

        encoder.finish()
    }
}

#[derive(Debug, Default)]
pub struct Encoder {
    type_section: wasm_encoder::TypeSection,
    function_section: wasm_encoder::FunctionSection,
    memory_section: wasm_encoder::MemorySection,
    export_section: wasm_encoder::ExportSection,
    code_section: wasm_encoder::CodeSection,
    data_section: wasm_encoder::DataSection,
}

impl Encoder {
    pub fn finish(mut self) -> Vec<u8> {
        let mut module = wasm_encoder::Module::default();

        // TODO: This shouldn't be hardcoded.
        _ = self.memory_section.memory(wasm_encoder::MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
        });

        _ = module.section(&self.type_section);
        _ = module.section(&self.function_section);
        _ = module.section(&self.memory_section);
        _ = module.section(&self.export_section);
        _ = module.section(&wasm_encoder::DataCountSection {
            count: self.data_section.len(),
        });
        _ = module.section(&self.code_section);
        _ = module.section(&self.data_section);

        module.finish()
    }
}

trait Encode {
    type Output;

    fn encode(&self, module: &Module, encoder: &mut Encoder) -> Self::Output;
}
