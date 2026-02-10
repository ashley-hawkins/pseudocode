use js_sys::Map;
use pseudocode::{
    instruction::generate_instructions_for_ast,
    interpreter::{InterpreterState, OwnedProgram, Program},
};
use pseudocode_frontend::write_runtime_error;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct ValueJs(pseudocode::instruction::Value);

#[wasm_bindgen]
impl ValueJs {
    #[wasm_bindgen(js_name = toJs)]
    pub fn to_js(&self) -> JsValue {
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
pub struct ProgramJs {
    source_code: String,
    program: OwnedProgram,
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
impl ProgramJs {
    fn clear_output(&mut self) {
        self.output.get_mut().clear();
        self.output.set_position(0);
    }

    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self::default()
    }

    #[wasm_bindgen(js_name = loadSource)]
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

    #[wasm_bindgen(js_name = resetStateWithEnvironment)]
    pub fn reset_state_with_environment(&mut self, env_initializers: Vec<String>) -> bool {
        let Some(initial_environment) =
            pseudocode_frontend::create_initial_environment(&env_initializers, &mut self.output)
        else {
            return false;
        };

        self.state = InterpreterState::new_with_environment(initial_environment);

        true
    }

    pub fn clear(&mut self) {
        self.source_code.clear();
        self.program.clear();
        self.output.get_mut().clear();
        self.state = InterpreterState::new();
    }

    pub fn output(&self) -> Option<String> {
        String::from_utf8(self.output.get_ref().clone()).ok()
    }

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

    #[wasm_bindgen(js_name = currentEnvironment, unchecked_return_type = "Map<string, ValueJs>")]
    pub fn current_environment(&self) -> Map {
        self.frame_at(0).unwrap()
    }

    #[wasm_bindgen(js_name = frameAt, unchecked_return_type = "Map<string, ValueJs> | undefined")]
    pub fn frame_at(&self, index: usize) -> Option<Map> {
        let frame = self
            .state
            .frame_stack
            .get(self.state.frame_stack.len() - 1 - index)?;

        let env = &frame.environment.0;

        let obj = js_sys::Map::new();

        for (key, value) in env.iter() {
            let wrapped = ValueJs(value.clone());
            obj.set(&JsValue::from_str(key), &wrapped.into());
        }

        Some(obj)
    }

    #[wasm_bindgen(js_name = currentFrames, unchecked_return_type = "Array<Map<string, ValueJs>>")]
    pub fn current_frames(&self) -> Vec<Map> {
        let mut frames = Vec::new();
        for i in 0..self.state.frame_stack.len() {
            frames.push(self.frame_at(i).unwrap());
        }
        frames
    }

    #[wasm_bindgen(js_name = querySourceLines)]
    pub fn query_source_lines(&mut self) -> LineQueryResult {
        let last_line = self
            .state
            .last_instruction_offset
            .map(|offset| self.program[offset].instruction.span.start.line);
        let next_line = self
            .program
            .get(self.state.instruction_offset)
            .map(|annotated_instr| annotated_instr.instruction.span.start.line);

        let next_line_is_start_of_line =
            is_start_of_line(&self.program, self.state.instruction_offset);

        LineQueryResult {
            last_line,
            next_line,
            next_line_is_start_of_line,
        }
    }
}

fn is_start_of_line(program: &Program, instruction_index: usize) -> bool {
    instruction_index < program.len()
        && (instruction_index == 0
            || (!program[instruction_index].annotation.exempt_from_break
                && program[instruction_index - 1].instruction.span.start.line
                    != program[instruction_index].instruction.span.start.line))
}

#[wasm_bindgen]
pub struct LineQueryResult {
    pub last_line: Option<usize>,
    pub next_line: Option<usize>,
    pub next_line_is_start_of_line: bool,
}

#[wasm_bindgen]
impl LineQueryResult {
    /// Returns true if the program is transitioning to the target line
    /// in the next step.
    #[wasm_bindgen]
    pub fn at_line_boundary(&self) -> bool {
        self.next_line_is_start_of_line
        // || match (self.last_line, self.next_line) {
        //     // Transitioning to any different line
        //     (last, Some(next)) => last != Some(next),
        //     // If next is None then we're halted, so no line transitions
        //     _ => false,
        // }
    }
}

#[wasm_bindgen(start)]
pub fn main() {
    console_error_panic_hook::set_once();
}
