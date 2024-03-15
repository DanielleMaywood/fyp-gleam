use ecow::EcoString;

use super::{Encode, Index, Instruction, Local, Type, ValType};

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum FunctionLinkage {
    Local,
    Export,
}

#[derive(Debug, Clone)]
pub struct Function {
    index: Index<Function>,
    type_index: Index<Type>,
    name: EcoString,
    linkage: FunctionLinkage,
    instructions: Vec<Instruction>,
    pub local_count: u32,
    local_map: im::HashMap<EcoString, Index<Local>>,
    local_types: im::OrdMap<Index<Local>, ValType>,
}

impl Function {
    pub fn new(
        index: Index<Function>,
        type_index: Index<Type>,
        name: EcoString,
        linkage: FunctionLinkage,
        params: Vec<Option<EcoString>>,
    ) -> Self {
        let mut local_map = im::HashMap::default();
        let local_count = u32::try_from(params.len()).expect("Unsupported number of parameters");

        for (param_index, param) in params
            .into_iter()
            .filter_map(std::convert::identity)
            .enumerate()
        {
            let local_index = u32::try_from(param_index).expect("Unsupported local index");
            let local_index = Index::new(local_index);

            _ = local_map.insert(param, local_index);
        }

        Self {
            index,
            type_index,
            name,
            linkage,
            instructions: vec![],
            local_count,
            local_map,
            local_types: Default::default(),
        }
    }

    pub fn declare_anonymous_local(&mut self, type_: ValType) -> Index<Local> {
        self.declare_local(format!("_anon${}", self.local_count), type_)
    }

    pub fn declare_local(
        &mut self,
        name: impl Into<EcoString> + std::fmt::Debug,
        type_: ValType,
    ) -> Index<Local> {
        let index = Index::new(self.local_count);
        self.local_count += 1;
        _ = self.local_types.insert(index, type_);
        _ = self.local_map.insert(name.into(), index);
        index
    }

    pub fn get_local_index(&self, name: EcoString) -> Index<Local> {
        self.local_map
            .get(&name.clone())
            .copied()
            .unwrap_or_else(|| panic!("No local index found for {}", name))
    }

    pub fn get_local_type(&self, name: EcoString) -> &ValType {
        let local_index = self.get_local_index(name);

        self.local_types.get(&local_index).unwrap()
    }

    pub fn instruction(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }
}

impl Encode for Function {
    type Output = ();

    fn encode(&self, module: &super::Module, encoder: &mut super::Encoder) -> Self::Output {
        _ = encoder
            .function_section
            .function(module.resolve_type_index(self.type_index));

        if self.linkage == FunctionLinkage::Export {
            _ = encoder.export_section.export(
                self.name.as_str(),
                wasm_encoder::ExportKind::Func,
                module.resolve_function_index(self.index),
            );
        }

        let locals = self
            .local_types
            .iter()
            .map(|(_, local)| local.encode(module, encoder));

        let mut function = wasm_encoder::Function::new_with_locals_types(locals);

        for instruction in &self.instructions {
            for encoded in &instruction.encode(module, encoder) {
                _ = function.instruction(encoded);
            }
        }

        _ = encoder.code_section.function(&function);
    }
}
