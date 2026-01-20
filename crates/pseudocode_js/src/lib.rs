use std::io::Write;

use js_sys::Map;
use pseudocode::{
    instruction::generate_instructions_for_ast,
    interpreter::{InterpreterState, Program},
};
use pseudocode_frontend::write_runtime_error;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct ValueWrapper(pseudocode::instruction::Value);

#[wasm_bindgen]
impl ValueWrapper {
    #[wasm_bindgen]
    pub fn as_js(&self) -> JsValue {
        fn value_to_js(value: &pseudocode::instruction::Value) -> JsValue {
            match value {
                pseudocode::instruction::Value::Number(n) => JsValue::from_f64(*n),
                pseudocode::instruction::Value::Bool(b) => JsValue::from_bool(*b),
                pseudocode::instruction::Value::Array(ref_cell) => {
                    let arr = js_sys::Array::new();
                    for v in ref_cell.borrow().iter() {
                        arr.push(&value_to_js(v));
                    }
                    JsValue::from(arr)
                }
                pseudocode::instruction::Value::None => JsValue::NULL,
            }
        }
        value_to_js(&self.0)
    }

    #[wasm_bindgen]
    pub fn clear(&mut self) {
        self.0 = pseudocode::instruction::Value::None;
    }
}

#[wasm_bindgen]
#[derive(Default)]
pub struct ProgramWrapper {
    source_code: String,
    program: <Program as ToOwned>::Owned,
    output: std::io::Cursor<Vec<u8>>,
    state: InterpreterState,
}

#[wasm_bindgen]
pub enum Mode {
    Jumpy,
    Structured,
    Procedural,
}

#[wasm_bindgen]
impl ProgramWrapper {
    fn clear_output(&mut self) {
        self.output.get_mut().clear();
        self.output.set_position(0);
    }

    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self::default()
    }

    #[wasm_bindgen]
    pub fn load_source(&mut self, src: &str, mode: Mode) -> bool {
        self.clear_output();
        self.state = InterpreterState::new();
        self.source_code = src.to_string();

        let Some(program_ast) = pseudocode_frontend::parse_source_to_ast(
            src,
            "input".to_string(),
            match mode {
                Mode::Jumpy => pseudocode::parser::Mode::JumpyImp,
                Mode::Structured => pseudocode::parser::Mode::StructuredImp,
                Mode::Procedural => pseudocode::parser::Mode::ProceduralImp,
            },
            &mut self.output,
        ) else {
            return false;
        };

        let program = generate_instructions_for_ast(&program_ast);

        self.program = program;

        true
    }

    pub fn reset_state_with_environment(&mut self, env_initializers: Vec<String>) -> bool {
        let Some(initial_environment) =
            pseudocode_frontend::create_initial_environment(&env_initializers, &mut self.output)
        else {
            return false;
        };

        self.state = InterpreterState::new_with_environment(initial_environment);

        true
    }

    #[wasm_bindgen]
    pub fn clear(&mut self) {
        self.source_code.clear();
        self.program.clear();
        self.output.get_mut().clear();
        self.state = InterpreterState::new();
    }

    #[wasm_bindgen]
    pub fn output(&self) -> Option<String> {
        String::from_utf8(self.output.get_ref().clone()).ok()
    }

    #[wasm_bindgen]
    pub fn step(&mut self) -> bool {
        match self
            .state
            .step_with_print_dest(&self.program, &mut self.output)
        {
            Ok(step_result) => match step_result {
                pseudocode::interpreter::StepResult::Continued => true,
                pseudocode::interpreter::StepResult::Halted(_) => false,
            },
            Err(runtime_error) => {
                write_runtime_error(
                    &self.source_code,
                    "input".to_string(),
                    &runtime_error,
                    &mut self.output,
                );
                false
            }
        }
    }

    #[wasm_bindgen]
    pub fn current_environment(&self) -> Map {
        let env = &self.state.frame_stack.last().unwrap().environment.0;

        let obj = js_sys::Map::new();

        for (key, value) in env.iter() {
            let wrapped = ValueWrapper(value.clone());
            obj.set(&JsValue::from_str(key), &wrapped.into());
        }

        obj
    }
}

#[wasm_bindgen(start)]
pub fn main() {
    console_error_panic_hook::set_once();
}
