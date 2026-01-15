use std::{cell::RefCell, rc::Rc};

use chumsky::span::{Span, WrappingSpan};

use crate::{
    expr::{BinaryOperator, Expr, UnaryOperator},
    parser::{
        AssignmentStatement, AstRoot, Block, ForStatement, GotoStatement, IfStatement,
        ProcedureDefinition, ReturnStatement, Statement, SwapStatement, WhileStatement,
    },
    util::Spanned,
};

#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    None,
    Number(f64),
    Bool(bool),
    // String(String),
    Array(Rc<RefCell<Vec<Value>>>),
}

impl From<Vec<Value>> for Value {
    fn from(vec: Vec<Value>) -> Self {
        Value::Array(Rc::new(RefCell::new(vec)))
    }
}

impl Value {
    // This should be used when popping a value into the environment
    pub fn move_or_deep_clone(self) -> Self {
        match self {
            Value::None => Value::None,
            Value::Number(n) => Value::Number(n),
            Value::Bool(b) => Value::Bool(b),
            // Value::String(s) => Value::String(s),
            Value::Array(arr) => match Rc::try_unwrap(arr) {
                Ok(inner) => Value::Array(Rc::new(RefCell::new(inner.into_inner()))),
                Err(rc) => {
                    let cloned_vec: Vec<Value> =
                        rc.borrow().iter().map(|v| v.deep_clone()).collect();
                    Value::from(cloned_vec)
                }
            },
        }
    }

    pub fn deep_clone(&self) -> Self {
        match self {
            Value::None => Value::None,
            Value::Number(n) => Value::Number(*n),
            Value::Bool(b) => Value::Bool(*b),
            // Value::String(s) => Value::String(s.clone()),
            Value::Array(arr) => {
                let cloned_vec: Vec<Value> = arr.borrow().iter().map(|v| v.deep_clone()).collect();
                Value::from(cloned_vec)
            }
        }
    }

    pub fn ensure_int(&self) -> Option<i64> {
        match self {
            Value::Number(n) if n.fract() == 0.0 => Some(*n as i64),
            _ => None,
        }
    }

    pub fn ensure_number(&self) -> Option<f64> {
        match self {
            Value::Number(n) => Some(*n),
            _ => None,
        }
    }

    pub fn ensure_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(b) => Some(*b),
            _ => None,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Label {
    Function(String),
    Line(usize),
}

pub enum RelativeTarget {
    Label(Label),
    Index(usize),
}

#[derive(Clone, PartialEq, Debug)]
pub enum PopDestination {
    Environment(String),
    Discard,
}

#[derive(Clone, PartialEq, Debug)]
pub enum PushSource {
    Environment(String),
    Literal(Value),
}

// Generic instruction form where control-flow targets can vary (labels in pass 1, indices in pass 2).
#[derive(Clone, PartialEq, Debug)]
pub enum InstructionGeneric<Target> {
    // Special "instruction" that marks the beginning of a function. This is used to validate the parameter list during FunctionCall instructions.
    FunctionHeader {
        parameter_count: usize,
    },

    // out: pushed value
    Push(PushSource),

    // in: popped value
    Pop(PopDestination),

    // in: lhs, rhs
    // out: result
    Binary {
        op: BinaryOperator,
    },

    // in: value
    // out: result
    Unary(UnaryOperator),

    // in: len
    // out: array of length len
    ArrayBuild,

    // in: values...
    // out: array literal
    ArrayLiteral {
        length: usize,
    },

    // in: array, index
    // out: value at array[index]
    ArrayIndex,

    // in: array, [start], [end]
    // out: sub-array array[start:end]
    ArraySlice {
        has_start: bool,
        has_end: bool,
    },

    // in: arg_count arguments
    // out: return value of the function
    FunctionCall {
        target: Target,
        arg_count: usize,
    },

    // in: [condition]
    Jump {
        is_conditional: bool,
        target: Target,
    },

    // in: return value
    Return,
}

// Uses labels to reference instructions as the labels are only during the second pass.
pub type InstructionRelative = InstructionGeneric<RelativeTarget>;
// Uses instruction indices instead of labels.
pub type Instruction = InstructionGeneric<usize>;

enum InstructionGenerationUnit {
    Placeholder,
    Instruction(Spanned<InstructionRelative>),
}

struct InstructionGenerationContext {
    labels: std::collections::HashMap<Label, usize>,
    next_label: Option<Label>,

    instructions: Vec<InstructionGenerationUnit>,
}

impl InstructionGenerationContext {
    fn new() -> Self {
        InstructionGenerationContext {
            labels: std::collections::HashMap::new(),
            next_label: None,
            instructions: Vec::new(),
        }
    }

    fn set_next_label(&mut self, label: Label) {
        self.next_label = Some(label);
    }

    fn push_placeholder(&mut self) -> usize {
        let offset = self.next_instruction_index();
        self.instructions
            .push(InstructionGenerationUnit::Placeholder);
        offset
    }

    fn push_instruction(&mut self, instruction: Spanned<InstructionRelative>) -> usize {
        let offset = self.next_instruction_index();
        self.instructions
            .push(InstructionGenerationUnit::Instruction(instruction));
        if let Some(label) = self.next_label.take() {
            self.labels.entry(label).or_insert(offset);
        }
        offset
    }

    fn replace_instruction(&mut self, offset: usize, instruction: Spanned<InstructionRelative>) {
        self.instructions[offset] = InstructionGenerationUnit::Instruction(instruction);
    }

    fn push<G: GenerateInstructions>(&mut self, generator: &G) {
        generator.generate_instructions(self);
    }

    fn next_instruction_index(&self) -> usize {
        self.instructions.len()
    }

    // fn finalize_single_instruction(&self, instr: InstructionRelative) -> InstructionAbsolute {

    // }

    fn finalize(self) -> Vec<Spanned<Instruction>> {
        let Self {
            labels,
            instructions,
            ..
        } = self;

        let finalize_single_instruction = move |instr| match instr {
            InstructionRelative::Jump {
                is_conditional,
                target,
            } => Instruction::Jump {
                is_conditional,
                target: match target {
                    RelativeTarget::Label(label) => *labels
                        .get(&label)
                        .expect("Unresolved label during finalization"),
                    RelativeTarget::Index(index) => index,
                },
            },
            InstructionRelative::FunctionCall { target, arg_count } => Instruction::FunctionCall {
                target: match target {
                    RelativeTarget::Label(label) => *labels
                        .get(&label)
                        .expect("Unresolved label during finalization"),
                    RelativeTarget::Index(index) => index,
                },
                arg_count,
            },
            InstructionRelative::FunctionHeader {
                parameter_count: parameters,
            } => Instruction::FunctionHeader {
                parameter_count: parameters,
            },
            InstructionRelative::Push(push_source) => Instruction::Push(push_source),
            InstructionRelative::Pop(pop_destination) => Instruction::Pop(pop_destination),
            InstructionRelative::Binary { op } => Instruction::Binary { op },
            InstructionRelative::ArrayBuild => Instruction::ArrayBuild,
            InstructionRelative::ArrayLiteral { length } => Instruction::ArrayLiteral { length },
            InstructionRelative::ArrayIndex => Instruction::ArrayIndex,
            InstructionRelative::ArraySlice { has_start, has_end } => {
                Instruction::ArraySlice { has_start, has_end }
            }
            InstructionRelative::Unary(unary_operator) => Instruction::Unary(unary_operator),
            InstructionRelative::Return => Instruction::Return,
        };

        instructions
            .into_iter()
            .map(|unit| match unit {
                InstructionGenerationUnit::Placeholder => {
                    panic!("Unresolved instruction placeholder during finalization")
                }
                InstructionGenerationUnit::Instruction(instr) => instr
                    .span
                    .make_wrapped(finalize_single_instruction(instr.inner)),
            })
            .collect()
    }
}

trait GenerateInstructions {
    fn generate_instructions(&self, context: &mut InstructionGenerationContext);
}

impl GenerateInstructions for Spanned<Expr<'_>> {
    fn generate_instructions(&self, context: &mut InstructionGenerationContext) {
        context.set_next_label(Label::Line(self.span.start.line));
        match &self.inner {
            Expr::NumberLiteral(n) => {
                context.push_instruction(self.span.make_wrapped(InstructionRelative::Push(
                    PushSource::Literal(Value::Number(*n)),
                )));
            }
            Expr::BooleanLiteral(b) => {
                context.push_instruction(self.span.make_wrapped(InstructionRelative::Push(
                    PushSource::Literal(Value::Bool(*b)),
                )));
            }
            Expr::Build(expr) => {
                context.push(&**expr);
                context.push_instruction(self.span.make_wrapped(InstructionRelative::ArrayBuild));
            }
            Expr::ArrayLiteral(elements) => {
                // Pushed in reverse, so that they can just be popped straight into the array in order.
                for element in elements.iter().rev() {
                    context.push(element);
                }
                context.push_instruction(self.span.make_wrapped(
                    InstructionRelative::ArrayLiteral {
                        length: elements.len(),
                    },
                ));
            }
            Expr::VariableAccess(name) => {
                context.push_instruction(self.span.make_wrapped(InstructionRelative::Push(
                    PushSource::Environment((*name).to_owned()),
                )));
            }
            Expr::FunctionCall { left, arguments } => {
                for arg in &arguments.inner {
                    context.push(arg);
                }
                context.push_instruction(self.span.make_wrapped(
                    InstructionRelative::FunctionCall {
                        target: RelativeTarget::Label(Label::Function(left.inner.to_owned())),
                        arg_count: arguments.inner.len(),
                    },
                ));
            }
            Expr::ArrayAccess { left, right } => {
                context.push(&**left);

                match &right.inner {
                    crate::expr::ArrayIndex::SingleIndex(index_expr) => {
                        context.push(&**index_expr);
                        context.push_instruction(
                            self.span.make_wrapped(InstructionRelative::ArrayIndex),
                        );
                    }
                    crate::expr::ArrayIndex::Slice {
                        start,
                        colon: _,
                        end,
                    } => {
                        if let Some(start_expr) = start {
                            context.push(&**start_expr);
                        }
                        if let Some(end_expr) = end {
                            context.push(&**end_expr);
                        }
                        context.push_instruction(self.span.make_wrapped(
                            InstructionRelative::ArraySlice {
                                has_start: start.is_some(),
                                has_end: end.is_some(),
                            },
                        ));
                    }
                }
            }
            Expr::BinaryOp { left, op, right } => {
                context.push(&**left);
                context.push(&**right);
                context.push_instruction(
                    self.span
                        .make_wrapped(InstructionRelative::Binary { op: op.inner }),
                );
            }
            Expr::UnaryOp { op, expr } => {
                context.push(&**expr);
                context
                    .push_instruction(self.span.make_wrapped(InstructionRelative::Unary(op.inner)));
            }
        }
    }
}

impl GenerateInstructions for Block<'_> {
    fn generate_instructions(&self, context: &mut InstructionGenerationContext) {
        for statement in &self.0 {
            context.push(statement);
        }
    }
}

impl GenerateInstructions for Spanned<Statement<'_>> {
    fn generate_instructions(&self, context: &mut InstructionGenerationContext) {
        match &self.inner {
            Statement::Goto(stmt) => context.push(&self.span.make_wrapped(*stmt)),
            Statement::Swap(stmt) => context.push(&self.span.make_wrapped(*stmt)),
            Statement::Assignment(stmt) => context.push(&self.span.make_wrapped(stmt.clone())),
            Statement::If(stmt) => context.push(&self.span.make_wrapped(stmt.clone())),
            Statement::While(stmt) => context.push(&self.span.make_wrapped(stmt.clone())),
            Statement::For(stmt) => context.push(&self.span.make_wrapped(stmt.clone())),
            Statement::Return(stmt) => context.push(&self.span.make_wrapped(stmt.clone())),
            Statement::BareExpr(expr) => {
                context.push(expr);
                // Discard the result of the expression, since the statement does not use it.

                // TODO: investigate an alternative approach: Just clear the "expression stack" unconditionally after each statement,
                // since this stack is only even used when expressions jump to another location and back (function calls).
                context.push_instruction(
                    self.span
                        .make_wrapped(InstructionRelative::Pop(PopDestination::Discard)),
                );
            }
        }
    }
}

impl GenerateInstructions for Spanned<GotoStatement> {
    fn generate_instructions(&self, context: &mut InstructionGenerationContext) {
        context.push_instruction(self.span.make_wrapped(InstructionRelative::Jump {
            is_conditional: false,
            target: RelativeTarget::Label(Label::Line(self.inner.line_number.inner)),
        }));
    }
}

impl GenerateInstructions for Spanned<SwapStatement<'_>> {
    fn generate_instructions(&self, context: &mut InstructionGenerationContext) {
        context.push_instruction(self.span.make_wrapped(InstructionRelative::Push(
            PushSource::Environment(self.inner.ident_1.inner.to_owned()),
        )));
        context.push_instruction(self.span.make_wrapped(InstructionRelative::Push(
            PushSource::Environment(self.inner.ident_2.inner.to_owned()),
        )));
        context.push_instruction(self.span.make_wrapped(InstructionRelative::Pop(
            PopDestination::Environment(self.inner.ident_1.inner.to_owned()),
        )));
        context.push_instruction(self.span.make_wrapped(InstructionRelative::Pop(
            PopDestination::Environment(self.inner.ident_2.inner.to_owned()),
        )));
    }
}

impl GenerateInstructions for Spanned<AssignmentStatement<'_>> {
    fn generate_instructions(&self, context: &mut InstructionGenerationContext) {
        context.push(&self.inner.expression);
        context.push_instruction(self.span.make_wrapped(InstructionRelative::Pop(
            PopDestination::Environment(self.inner.identifier.inner.to_owned()),
        )));
    }
}

impl GenerateInstructions for Spanned<ReturnStatement<'_>> {
    fn generate_instructions(&self, context: &mut InstructionGenerationContext) {
        if let Some(expr) = &self.inner.expr {
            context.push(expr);
        } else {
            context.push_instruction(
                self.span
                    .make_wrapped(InstructionRelative::Push(PushSource::Literal(Value::None))),
            );
        }
        context.push_instruction(self.span.make_wrapped(InstructionRelative::Return));
    }
}

impl GenerateInstructions for Spanned<ForStatement<'_>> {
    fn generate_instructions(&self, context: &mut InstructionGenerationContext) {
        // Initialize the loop variable
        context.push(&self.inner.start_expr);
        context.push_instruction(self.span.make_wrapped(InstructionRelative::Pop(
            PopDestination::Environment(self.inner.loop_variable.to_owned()),
        )));

        // Span over the "x to y" part of the for loop heading e.g. `for x <- 1 to 10` this would be `1 to 10`
        let range_span = self.inner.start_expr.span.union(self.inner.end_expr.span);

        let while_loop = WhileStatement {
            condition: self.span.make_wrapped(Expr::BinaryOp {
                left: Box::new(
                    range_span.make_wrapped(Expr::VariableAccess(self.inner.loop_variable)),
                ),
                op: range_span.make_wrapped(BinaryOperator::Lte),
                right: Box::new(self.inner.end_expr.clone()),
            }),
            body: Block(
                self.inner
                    .body
                    .0
                    .iter()
                    .cloned()
                    .chain(std::iter::once({
                        // Increment the loop variable at the end of each iteration
                        let increment_expr = self.span.make_wrapped(Expr::BinaryOp {
                            left: Box::new(
                                self.span
                                    .make_wrapped(Expr::VariableAccess(self.inner.loop_variable)),
                            ),
                            op: range_span.make_wrapped(BinaryOperator::Add),
                            right: Box::new(range_span.make_wrapped(Expr::NumberLiteral(1.0))),
                        });
                        let assignment_stmt = AssignmentStatement {
                            identifier: range_span.make_wrapped(self.inner.loop_variable),
                            expression: increment_expr,
                        };
                        range_span.make_wrapped(Statement::Assignment(assignment_stmt))
                    }))
                    .collect::<Vec<_>>(),
            ),
        };

        context.push(&self.span.make_wrapped(while_loop));
    }
}

impl GenerateInstructions for Spanned<WhileStatement<'_>> {
    fn generate_instructions(&self, context: &mut InstructionGenerationContext) {
        let start_pos = context.next_instruction_index();

        context.push(&self.inner.condition);
        context.push_instruction(
            self.span
                .make_wrapped(InstructionRelative::Unary(UnaryOperator::Not)),
        );

        let jump_placeholder_pos = context.push_placeholder();

        context.push(&self.inner.body);

        context.push_instruction(self.span.make_wrapped(InstructionRelative::Jump {
            is_conditional: false,
            target: RelativeTarget::Index(start_pos),
        }));

        context.replace_instruction(
            jump_placeholder_pos,
            self.span.make_wrapped(InstructionRelative::Jump {
                is_conditional: true,
                target: RelativeTarget::Index(context.next_instruction_index()),
            }),
        );
    }
}

impl GenerateInstructions for Spanned<IfStatement<'_>> {
    fn generate_instructions(&self, context: &mut InstructionGenerationContext) {
        context.push(&self.inner.condition);
        context.push_instruction(
            self.span
                .make_wrapped(InstructionRelative::Unary(UnaryOperator::Not)),
        );
        // Initial jump, jumps to wherever we need to be when the condition is false.
        // So that's the end if there's no else, or the beginning of the else block if there is one.
        let initial_jump_placeholder_pos = context.push_placeholder();
        context.push(&self.inner.then_branch);

        let initial_jump_dest = if let Some(else_branch) = &self.inner.else_branch {
            // If there's an else then the end of the "then" block needs to unconditionally jump to the end of the else block.
            let jump_to_end_placeholder_pos = context.push_placeholder();

            let else_begin_pos = context.next_instruction_index();

            context.push(else_branch);

            context.replace_instruction(
                jump_to_end_placeholder_pos,
                self.span.make_wrapped(InstructionRelative::Jump {
                    is_conditional: false,
                    target: RelativeTarget::Index(context.next_instruction_index()),
                }),
            );

            else_begin_pos
        } else {
            context.next_instruction_index()
        };

        context.replace_instruction(
            initial_jump_placeholder_pos,
            self.span.make_wrapped(InstructionRelative::Jump {
                is_conditional: true,
                target: RelativeTarget::Index(initial_jump_dest),
            }),
        );
    }
}

impl GenerateInstructions for Spanned<ProcedureDefinition<'_>> {
    fn generate_instructions(&self, context: &mut InstructionGenerationContext) {
        context.set_next_label(Label::Function(self.inner.name.inner.to_owned()));

        context.push_instruction(self.span.make_wrapped(InstructionRelative::FunctionHeader {
            parameter_count: self.inner.parameters.inner.len(),
        }));

        // Pop in reverse of the order that the args are pushed.
        for param in self.inner.parameters.inner.iter().rev() {
            context.push_instruction(param.span.make_wrapped(InstructionRelative::Pop(
                PopDestination::Environment(param.inner.to_owned()),
            )));
        }

        context.push(&self.inner.body);
    }
}

impl GenerateInstructions for AstRoot<'_> {
    fn generate_instructions(&self, context: &mut InstructionGenerationContext) {
        context.push(&self.main_algorithm.inner);
        for procedure in &self.procedures {
            context.push(procedure);
        }
    }
}

pub fn generate_instructions_for_ast(ast: &AstRoot<'_>) -> Vec<Spanned<Instruction>> {
    let mut context = InstructionGenerationContext::new();
    context.push(ast);
    context.finalize()
}
