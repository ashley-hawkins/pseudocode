use std::collections::HashMap;

use chumsky::span::WrappingSpan;

use crate::{
    instruction::{EnsureType, Instruction, Value},
    type_checker::Type,
    util::{SourceSpan, Spanned},
};

pub struct Environment(HashMap<String, Value>);

#[derive(Debug)]
pub enum RuntimeError {
    TypeError {
        expected: Type,
        found: Type,
        span: SourceSpan,
    },
}

pub struct Frame {
    pub environment: Environment,
    // If there's no return address then the program ends on return
    pub return_address: Option<usize>,
}

pub type Program = [Spanned<Instruction>];

pub struct InterpreterState {
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
) -> Result<Value, RuntimeError> {
    Ok(match op {
        crate::expr::BinaryOperator::Add => {
            Value::Number(lhs.ensure_number()? + rhs.ensure_number()?)
        }
        crate::expr::BinaryOperator::Sub => {
            Value::Number(lhs.ensure_number()? - rhs.ensure_number()?)
        }
        crate::expr::BinaryOperator::Mul => {
            Value::Number(lhs.ensure_number()? * rhs.ensure_number()?)
        }
        crate::expr::BinaryOperator::Div => {
            Value::Number(lhs.ensure_number()? / rhs.ensure_number()?)
        }
        crate::expr::BinaryOperator::Lt => Value::Bool(lhs.ensure_number()? < rhs.ensure_number()?),
        crate::expr::BinaryOperator::Gt => Value::Bool(lhs.ensure_number()? > rhs.ensure_number()?),
        crate::expr::BinaryOperator::Lte => {
            Value::Bool(lhs.ensure_number()? <= rhs.ensure_number()?)
        }
        crate::expr::BinaryOperator::Gte => {
            Value::Bool(lhs.ensure_number()? >= rhs.ensure_number()?)
        }
        crate::expr::BinaryOperator::Eq => Value::Bool(lhs.inner == rhs.inner),
        crate::expr::BinaryOperator::Neq => Value::Bool(lhs.inner != rhs.inner),
        crate::expr::BinaryOperator::And => Value::Bool(lhs.ensure_bool()? && rhs.ensure_bool()?),
        crate::expr::BinaryOperator::Or => Value::Bool(lhs.ensure_bool()? || rhs.ensure_bool()?),

        crate::expr::BinaryOperator::IsIn => {
            let element = lhs.inner;
            let array = rhs.ensure_array()?;
            let array_ref = array.borrow();

            Value::Bool(array_ref.contains(&element))
        }
    })
}

fn unary_operation(
    operand: Spanned<Value>,
    op: &crate::expr::UnaryOperator,
) -> Result<Value, RuntimeError> {
    let array_window_cmp = |cmp: fn(&f64, &f64) -> bool| -> Result<bool, RuntimeError> {
        Ok(operand
            .clone()
            .ensure_array_of(Type::Number)?
            .borrow()
            .windows(2)
            .all(|w| {
                let lhs = operand
                    .span
                    .make_wrapped(w[0].clone())
                    .ensure_number()
                    .unwrap();
                let rhs = operand
                    .span
                    .make_wrapped(w[1].clone())
                    .ensure_number()
                    .unwrap();

                cmp(&lhs, &rhs)
            }))
    };

    Ok(match op {
        crate::expr::UnaryOperator::Neg => Value::Number(-operand.ensure_number()?),
        crate::expr::UnaryOperator::Not => Value::Bool(!operand.ensure_bool()?),

        crate::expr::UnaryOperator::IsAscending => Value::Bool(array_window_cmp(|a, b| a <= b)?),
        crate::expr::UnaryOperator::IsStrictlyAscending => {
            Value::Bool(array_window_cmp(|a, b| a < b)?)
        }
        crate::expr::UnaryOperator::IsDescending => Value::Bool(array_window_cmp(|a, b| a >= b)?),
        crate::expr::UnaryOperator::IsStrictlyDescending => {
            Value::Bool(array_window_cmp(|a, b| a > b)?)
        }
        crate::expr::UnaryOperator::IsEven => {
            Value::Bool(operand.clone().ensure_number()? % 2.0 == 0.0)
        }
        crate::expr::UnaryOperator::IsOdd => {
            Value::Bool(operand.clone().ensure_number()? % 2.0 != 0.0)
        }
    })
}

pub enum StepResult {
    Continued,
    Halted,
}

pub fn run_program(program: &Program) -> Result<Value, RuntimeError> {
    let mut state = InterpreterState::new();

    while let StepResult::Continued = state.step(program)? {}

    Ok(state.pop_value().inner)
}

impl InterpreterState {
    pub fn new() -> Self {
        Self {
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

    fn var_from_current_frame(&self, name: &str) -> Value {
        self.frame_stack
            .last()
            .unwrap()
            .environment
            .0
            .get(name)
            .cloned()
            .unwrap_or_default()
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

    fn binary_operation(
        &mut self,
        op: &crate::expr::BinaryOperator,
    ) -> Result<Value, RuntimeError> {
        let rhs = self.pop_value();
        let lhs = self.pop_value();

        binary_operation(lhs, rhs, op)
    }

    fn unary_operation(&mut self, op: &crate::expr::UnaryOperator) -> Result<Value, RuntimeError> {
        let operand = self.pop_value();

        unary_operation(operand, op)
    }

    fn prepare_function_call(
        &mut self,
        function_header: &Spanned<Instruction>,
        argument_count: usize,
        function_offset: usize,
    ) -> ControlFlow {
        let Instruction::FunctionHeader { parameter_count } = &function_header.inner else {
            panic!("Function call target is not function metadata");
        };

        if argument_count != *parameter_count {
            panic!("Function called with incorrect number of parameters");
        }

        self.push_frame();

        ControlFlow::Jump(function_offset + 1)
    }

    fn run_instruction(&mut self, program: &Program) -> Result<ControlFlow, RuntimeError> {
        let instruction = &program[self.instruction_offset];

        match &instruction.inner {
            Instruction::Push(crate::instruction::PushSource::Literal(literal)) => {
                self.push_value(instruction.span.make_wrapped(literal.clone()));
            }
            Instruction::Push(crate::instruction::PushSource::Environment(src)) => {
                let val = self.var_from_current_frame(src);
                self.push_value(instruction.span.make_wrapped(val));
            }
            Instruction::Pop(crate::instruction::PopDestination::Environment(dest)) => {
                let new_val = self.pop_value().inner.deep_clone();
                self.set_or_insert_var_in_current_frame(dest, new_val);
            }
            Instruction::Pop(crate::instruction::PopDestination::ArrayAccess) => {
                let new_val = self.pop_value().inner.deep_clone();
                let array_index = self.pop_value().ensure_int()? as usize;
                let array = self.pop_value().ensure_array()?;
                let mut array_ref = array.borrow_mut();
                array_ref[array_index] = new_val;
            }
            Instruction::Pop(crate::instruction::PopDestination::Discard) => {
                self.pop_value();
            }
            Instruction::Dup(n) => {
                let dup_count = n;
                for _ in 0..*dup_count {
                    let val = self.expr_stack[self.expr_stack.len() - dup_count].clone();
                    self.expr_stack.push(val);
                }
            }
            Instruction::SwapNth(n, m) => {
                let len = self.expr_stack.len();
                self.expr_stack.swap(len - 1 - n, len - 1 - m);
            }
            Instruction::Binary { op } => {
                let res = self.binary_operation(op)?;
                self.push_value(instruction.span.make_wrapped(res));
            }
            Instruction::Unary(unary_operator) => {
                // todo: have a span for the operand
                let res = self.unary_operation(unary_operator)?;
                self.push_value(instruction.span.make_wrapped(res));
            }
            Instruction::ArrayBuild => {
                let array_size = self.pop_value().ensure_int()? as usize;
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
                let index = self.pop_value().ensure_int()? as usize;
                let arr = self.pop_value().ensure_array()?;

                let array_ref = arr.borrow();
                let result = array_ref[index].clone();
                self.push_value(instruction.span.make_wrapped(result));
            }
            Instruction::ArraySlice { has_start, has_end } => {
                let end = if *has_end {
                    Some(self.pop_value().ensure_int()?)
                } else {
                    None
                };

                let start = if *has_start {
                    Some(self.pop_value().ensure_int()?)
                } else {
                    None
                };

                let arr = self.pop_value().ensure_array()?;

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
            Instruction::FunctionCall { target, arg_count } => {
                return Ok(self.prepare_function_call(&program[*target], *arg_count, *target));
            }
            Instruction::Jump {
                is_conditional,
                target,
            } => {
                if *is_conditional {
                    let condition = self.pop_value().ensure_bool()?;

                    if condition {
                        return Ok(ControlFlow::Jump(*target));
                    }
                } else {
                    return Ok(ControlFlow::Jump(*target));
                }
            }
            Instruction::Return => return Ok(ControlFlow::Return),
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
            Instruction::DebugStack => {
                println!("--- Stack Dump ---");
                for (i, val) in self.expr_stack.iter().enumerate() {
                    println!("{}: {}", i, val.inner);
                }
                println!("------------------");
            }
            crate::instruction::InstructionGeneric::Assert => {
                let condition = self.pop_value().ensure_bool()?;
                if !condition {
                    // todo: return runtime error instead of panicking
                    panic!("Assertion failed");
                }
            }
            Instruction::FunctionHeader { .. } => {
                panic!("Function header encountered during normal execution")
            }
        }
        Ok(ControlFlow::Continue)
    }

    pub fn step(&mut self, program: &Program) -> Result<StepResult, RuntimeError> {
        let control_flow = self.run_instruction(program)?;

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
                        return Ok(StepResult::Halted);
                    }
                }
            }
        }

        Ok(StepResult::Continued)
    }
}

impl Default for InterpreterState {
    fn default() -> Self {
        Self::new()
    }
}
