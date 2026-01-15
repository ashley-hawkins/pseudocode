use std::collections::HashMap;

use chumsky::span::WrappingSpan;

use crate::{
    instruction::{Instruction, Value},
    util::{SourceSpan, Spanned},
};

pub struct Environment(HashMap<String, Value>);

pub struct Frame {
    pub environment: Environment,
    // If there's no return address then the program ends on return
    pub return_address: Option<usize>,
}

pub struct InterpreterState<'prog> {
    pub program: &'prog Program,
    pub instruction_offset: usize,
    pub frame_stack: Vec<Frame>,
    pub expr_stack: Vec<Spanned<Value>>,
}

pub enum ControlFlow {
    Return,
    Jump(usize),
    Continue,
}

fn binary_operation(
    lhs: Spanned<Value>,
    rhs: Spanned<Value>,
    op: &crate::expr::BinaryOperator,
) -> Value {
    match op {
        crate::expr::BinaryOperator::Add => Value::Number(
            lhs.inner.ensure_number().expect("Expected number")
                + rhs.inner.ensure_number().expect("Expected number"),
        ),
        crate::expr::BinaryOperator::Sub => Value::Number(
            lhs.inner.ensure_number().expect("Expected number")
                - rhs.inner.ensure_number().expect("Expected number"),
        ),
        crate::expr::BinaryOperator::Mul => Value::Number(
            lhs.inner.ensure_number().expect("Expected number")
                * rhs.inner.ensure_number().expect("Expected number"),
        ),
        crate::expr::BinaryOperator::Div => Value::Number(
            lhs.inner.ensure_number().expect("Expected number")
                / rhs.inner.ensure_number().expect("Expected number"),
        ),
        crate::expr::BinaryOperator::Lt => Value::Bool(
            lhs.inner.ensure_number().expect("Expected number")
                < rhs.inner.ensure_number().expect("Expected number"),
        ),
        crate::expr::BinaryOperator::Gt => Value::Bool(
            lhs.inner.ensure_number().expect("Expected number")
                > rhs.inner.ensure_number().expect("Expected number"),
        ),
        crate::expr::BinaryOperator::Lte => Value::Bool(
            lhs.inner.ensure_number().expect("Expected number")
                <= rhs.inner.ensure_number().expect("Expected number"),
        ),
        crate::expr::BinaryOperator::Gte => Value::Bool(
            lhs.inner.ensure_number().expect("Expected number")
                >= rhs.inner.ensure_number().expect("Expected number"),
        ),
        crate::expr::BinaryOperator::Eq => Value::Bool(lhs.inner == rhs.inner),
        crate::expr::BinaryOperator::Neq => Value::Bool(lhs.inner != rhs.inner),
        crate::expr::BinaryOperator::And => Value::Bool(
            lhs.inner.ensure_bool().expect("Expected bool")
                && rhs.inner.ensure_bool().expect("Expected bool"),
        ),
        crate::expr::BinaryOperator::Or => Value::Bool(
            lhs.inner.ensure_bool().expect("Expected bool")
                || rhs.inner.ensure_bool().expect("Expected bool"),
        ),
    }
}

fn unary_operation(operand: Spanned<Value>, op: &crate::expr::UnaryOperator) -> Value {
    match op {
        crate::expr::UnaryOperator::Neg => {
            Value::Number(-operand.inner.ensure_number().expect("Expected number"))
        }
        crate::expr::UnaryOperator::Not => {
            Value::Bool(!operand.inner.ensure_bool().expect("Expected bool"))
        }
    }
}

pub enum StepResult {
    Continued,
    Halted,
}

pub type Program = [Spanned<Instruction>];

pub fn run_program(program: &Program) -> Value {
    let mut state = InterpreterState::new(program);

    while let StepResult::Continued = state.step() {}

    state.pop_value().inner
}

impl<'prog> InterpreterState<'prog> {
    pub fn new(program: &'prog Program) -> Self {
        Self {
            program,
            instruction_offset: 0,
            frame_stack: vec![Frame {
                environment: Environment(HashMap::new()),
                return_address: None,
            }],
            expr_stack: Vec::new(),
        }
    }

    fn push_value(&mut self, value: Spanned<Value>) {
        self.expr_stack.push(value.span.make_wrapped(value.inner));
    }

    fn pop_value(&mut self) -> Spanned<Value> {
        self.expr_stack.pop().unwrap()
    }

    fn var_from_current_frame(&self, name: &str) -> Option<&Value> {
        self.frame_stack.last().unwrap().environment.0.get(name)
    }

    fn set_or_insert_var_in_current_frame(&mut self, name: &str, value: Value) {
        let frame = self.frame_stack.last_mut().unwrap();
        frame.environment.0.insert(name.to_string(), value);
    }

    fn push_frame(&mut self) {
        self.frame_stack.push(Frame {
            environment: Environment(HashMap::new()),
            return_address: Some(self.instruction_offset + 1),
        });
    }

    fn pop_frame(&mut self) -> Frame {
        self.frame_stack.pop().unwrap()
    }

    fn binary_operation(&mut self, op: &crate::expr::BinaryOperator) -> Value {
        let rhs = self.pop_value();
        let lhs = self.pop_value();

        binary_operation(lhs, rhs, op)
    }

    fn unary_operation(&mut self, op: &crate::expr::UnaryOperator) -> Value {
        let operand = self.pop_value();

        unary_operation(operand, op)
    }

    fn prepare_function_call(
        &mut self,
        argument_count: usize,
        function_offset: usize,
    ) -> ControlFlow {
        let Instruction::FunctionHeader { parameter_count } = &self.program[function_offset].inner
        else {
            panic!("Function call target is not function metadata");
        };

        if argument_count != *parameter_count {
            panic!("Function called with incorrect number of parameters");
        }

        self.push_frame();

        ControlFlow::Jump(function_offset + 1)
    }

    fn run_instruction(&mut self) -> ControlFlow {
        let instruction: &chumsky::prelude::Spanned<
            crate::instruction::InstructionGeneric<usize>,
            SourceSpan,
        > = &self.program[self.instruction_offset];

        match &instruction.inner {
            Instruction::Push(crate::instruction::PushSource::Literal(literal)) => {
                self.push_value(instruction.span.make_wrapped(literal.clone()));
            }
            Instruction::Push(crate::instruction::PushSource::Environment(src)) => {
                let val = self.var_from_current_frame(src).unwrap().clone();
                self.push_value(instruction.span.make_wrapped(val));
            }
            Instruction::Pop(crate::instruction::PopDestination::Environment(dest)) => {
                let new_val = self.pop_value().inner.deep_clone();
                self.set_or_insert_var_in_current_frame(dest, new_val);
            }
            Instruction::Pop(crate::instruction::PopDestination::Discard) => {
                self.pop_value();
            }
            Instruction::Binary { op } => {
                let res = self.binary_operation(op);
                self.push_value(instruction.span.make_wrapped(res));
            }
            Instruction::Unary(unary_operator) => {
                // todo: have a span for the operand
                let res = self.unary_operation(unary_operator);
                self.push_value(instruction.span.make_wrapped(res));
            }
            Instruction::ArrayBuild => {
                let array_size = self.pop_value().ensure_int().unwrap() as usize;
                let mut elements = Vec::with_capacity(array_size);

                for _ in 0..array_size {
                    elements.push(Value::Number(0.0));
                }

                self.push_value(instruction.span.make_wrapped(Value::from(elements)));
            }
            Instruction::ArrayLiteral { length } => {
                let array_length = *length;

                let mut elements = Vec::with_capacity(array_length);

                for _ in 0..array_length {
                    elements.push(self.pop_value().inner.move_or_deep_clone());
                }

                self.push_value(instruction.span.make_wrapped(Value::from(elements)));
            }
            Instruction::ArrayIndex => {
                let index = self.pop_value().ensure_int().unwrap() as usize;
                let array = self.pop_value();

                match array.inner {
                    Value::Array(arr) => {
                        let array_ref = arr.borrow();
                        let result = array_ref[index].clone();
                        self.push_value(instruction.span.make_wrapped(result));
                    }
                    _ => panic!("Array index attempted on non-array value"),
                }
            }
            Instruction::ArraySlice { has_start, has_end } => {
                let end = has_end.then(|| self.pop_value().ensure_int().unwrap());
                let start = has_start.then(|| self.pop_value().ensure_int().unwrap());
                let array = self.pop_value();

                match array.inner {
                    Value::Array(arr) => {
                        let array_ref = arr.borrow();
                        let start_idx = start.unwrap_or(0) as usize;
                        let end_idx = end.unwrap_or(array_ref.len() as i64) as usize;

                        let result = Value::from(
                            array_ref[start_idx..end_idx]
                                .iter()
                                .map(|v| v.deep_clone())
                                .collect::<Vec<_>>(),
                        );

                        self.push_value(instruction.span.make_wrapped(result));
                    }
                    _ => panic!("Array slice attempted on non-array value"),
                }
            }
            Instruction::FunctionCall { target, arg_count } => {
                return self.prepare_function_call(*arg_count, *target);
            }
            Instruction::Jump {
                is_conditional,
                target,
            } => {
                if *is_conditional {
                    let condition = self.pop_value();
                    match condition.inner {
                        Value::Bool(true) => {
                            return ControlFlow::Jump(*target);
                        }
                        Value::Bool(false) => {}
                        _ => panic!("Condition for conditional jump is not a boolean"),
                    }
                } else {
                    return ControlFlow::Jump(*target);
                }
            }
            Instruction::Return => return ControlFlow::Return,
            Instruction::Debug {
                with_newline,
                arg_sources,
            } => {
                for source in arg_sources {
                    match source {
                        crate::instruction::DebugArgSource::StringLiteral(s) => print!("{}", s),
                        crate::instruction::DebugArgSource::Stack => {
                            let value = self.pop_value();
                            print!("{}", value.inner);
                        }
                    }
                }

                if *with_newline {
                    println!();
                }
            }
            Instruction::FunctionHeader { .. } => {
                panic!("Function header encountered during normal execution")
            }
        }
        ControlFlow::Continue
    }

    pub fn step(&mut self) -> StepResult {
        let control_flow = self.run_instruction();

        match control_flow {
            ControlFlow::Continue => {
                self.instruction_offset += 1;
            }
            ControlFlow::Jump(target) => {
                self.instruction_offset = target;
            }
            ControlFlow::Return => {
                let frame = self.pop_frame();
                match frame.return_address {
                    Some(return_address) => {
                        self.instruction_offset = return_address;
                    }
                    None => {
                        return StepResult::Halted;
                    }
                }
            }
        }

        StepResult::Continued
    }
}
