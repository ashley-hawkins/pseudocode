use std::{cell::RefCell, rc::Rc};

use chumsky::span::{Span, WrappingSpan};

use crate::{
    expr::{BinaryOperator, Expr, UnaryOperator},
    interpreter::RuntimeError,
    parser::{AstRoot, ProcedureDefinition},
    statement::{
        AssignmentLhs, AssignmentStatement, Block, DebugStatement, ForStatement, GotoStatement,
        IfStatement, ReturnStatement, Statement, SwapStatement, WhileStatement,
    },
    type_checker::Type,
    util::{SourceSpan, Spanned},
};

#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    None,
    Number(f64),
    Bool(bool),
    // String(String),
    Array(Rc<RefCell<Vec<Value>>>),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::None => write!(f, "<null>"),
            Value::Number(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            // Value::String(s) => write!(f, "{}", s),
            Value::Array(arr) => {
                write!(f, "[")?;
                let mut first = true;
                for v in arr.borrow().iter() {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                    first = false;
                }
                write!(f, "]")
            }
        }
    }
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

    fn get_type(&self) -> Type {
        match self {
            Value::None => Type::None,
            Value::Number(x) if x.fract() == 0.0 => Type::Integer,
            Value::Number(_) => Type::Number,
            Value::Bool(_) => Type::Boolean,
            Value::Array(_) => Type::Array,
        }
    }
}

pub trait EnsureType {
    fn ensure_number(self) -> Result<f64, RuntimeError>;
    fn ensure_int(self) -> Result<i64, RuntimeError>;
    fn ensure_bool(self) -> Result<bool, RuntimeError>;
    fn ensure_array(self) -> Result<Rc<RefCell<Vec<Value>>>, RuntimeError>;
    fn ensure_array_of(self, element_type: Type) -> Result<Rc<RefCell<Vec<Value>>>, RuntimeError>;
}

impl EnsureType for Spanned<Value> {
    fn ensure_int(self) -> Result<i64, RuntimeError> {
        let Self { inner, span } = self;
        match inner {
            Value::Number(n) if n.fract() == 0.0 => Ok(n as i64),
            _ => Err(RuntimeError::TypeError {
                expected: Type::Integer,
                found: inner.get_type(),
                span,
            }),
        }
    }

    fn ensure_number(self) -> Result<f64, RuntimeError> {
        let Self { inner, span } = self;
        match inner {
            Value::Number(n) => Ok(n),
            _ => Err(RuntimeError::TypeError {
                expected: Type::Number,
                found: inner.get_type(),
                span,
            }),
        }
    }

    fn ensure_bool(self) -> Result<bool, RuntimeError> {
        let Self { inner, span } = self;
        match inner {
            Value::Bool(b) => Ok(b),
            _ => Err(RuntimeError::TypeError {
                expected: Type::Boolean,
                found: inner.get_type(),
                span,
            }),
        }
    }

    fn ensure_array(self) -> Result<Rc<RefCell<Vec<Value>>>, RuntimeError> {
        let Self { inner, span } = self;
        match inner {
            Value::Array(arr) => Ok(arr),
            _ => Err(RuntimeError::TypeError {
                expected: Type::Array,
                found: inner.get_type(),
                span,
            }),
        }
    }

    fn ensure_array_of(self, element_type: Type) -> Result<Rc<RefCell<Vec<Value>>>, RuntimeError> {
        let Self { inner, span } = self;
        match inner {
            Value::Array(arr) => {
                for v in arr.borrow().iter() {
                    let val_type = v.get_type();
                    if val_type != element_type
                        && !(element_type == Type::Number && val_type == Type::Integer)
                    {
                        return Err(RuntimeError::TypeError {
                            expected: element_type,
                            found: v.get_type(),
                            span,
                        });
                    }
                }
                Ok(arr)
            }
            _ => Err(RuntimeError::TypeError {
                expected: Type::Array,
                found: inner.get_type(),
                span,
            }),
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
    ArrayAccess,
    Discard,
}

#[derive(Clone, PartialEq, Debug)]
pub enum PushSource {
    Environment(String),
    Literal(Value),
}

#[derive(Clone, PartialEq, Debug)]
pub enum DebugArgSource {
    Stack,
    StringLiteral(String),
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

    Dup(usize),

    // swaps the nth value from the top with the mth value from the top
    SwapNth(usize, usize),

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

    Debug {
        with_newline: bool,
        arg_sources: Vec<DebugArgSource>,
    },

    DebugStack,

    Assert,
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

    fn set_next_label_from_span(&mut self, span: &SourceSpan) {
        self.set_next_label(Label::Line(span.start.line + 1));
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
            InstructionRelative::Dup(n) => Instruction::Dup(n),
            InstructionRelative::SwapNth(n, m) => Instruction::SwapNth(n, m),
            InstructionRelative::Binary { op } => Instruction::Binary { op },
            InstructionRelative::ArrayBuild => Instruction::ArrayBuild,
            InstructionRelative::ArrayLiteral { length } => Instruction::ArrayLiteral { length },
            InstructionRelative::ArrayIndex => Instruction::ArrayIndex,
            InstructionRelative::ArraySlice { has_start, has_end } => {
                Instruction::ArraySlice { has_start, has_end }
            }
            InstructionRelative::Unary(unary_operator) => Instruction::Unary(unary_operator),
            InstructionRelative::Return => Instruction::Return,
            InstructionRelative::Debug {
                with_newline,
                arg_sources,
            } => Instruction::Debug {
                with_newline,
                arg_sources,
            },
            InstructionRelative::DebugStack => Instruction::DebugStack,
            InstructionRelative::Assert => Instruction::Assert,
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
        context.set_next_label_from_span(&self.span);
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
            Statement::Swap(stmt) => context.push(&self.span.make_wrapped(stmt.clone())),
            Statement::Assignment(stmt) => context.push(&self.span.make_wrapped(stmt.clone())),
            Statement::If(stmt) => context.push(&self.span.make_wrapped(stmt.clone())),
            Statement::While(stmt) => context.push(&self.span.make_wrapped(stmt.clone())),
            Statement::For(stmt) => context.push(&self.span.make_wrapped(stmt.clone())),
            Statement::Return(stmt) => context.push(&self.span.make_wrapped(stmt.clone())),
            Statement::Debug(DebugStatement { with_newline, args }) => {
                let sources = args
                    .iter()
                    .map(|arg| match arg {
                        crate::statement::DebugArgument::String(str) => {
                            DebugArgSource::StringLiteral((*str).to_owned())
                        }
                        crate::statement::DebugArgument::Expr(_) => DebugArgSource::Stack,
                    })
                    .collect::<Vec<_>>();

                for expr in args.iter().rev().filter_map(|arg| match arg {
                    crate::statement::DebugArgument::String(_) => None,
                    crate::statement::DebugArgument::Expr(expr) => Some(expr),
                }) {
                    context.push(expr)
                }

                context.push_instruction(self.span.make_wrapped(InstructionRelative::Debug {
                    with_newline: *with_newline,
                    arg_sources: sources,
                }));
            }
            Statement::DebugStack => {
                context.push_instruction(self.span.make_wrapped(InstructionRelative::DebugStack));
            }
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
            Statement::Assert(assert_statement) => {
                context.push(&assert_statement.condition);
                context.push_instruction(self.span.make_wrapped(InstructionRelative::Assert));
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
        match &self.inner.lhs {
            AssignmentLhs::Variable(spanned) => {
                context.push_instruction(self.span.make_wrapped(InstructionRelative::Push(
                    PushSource::Environment(spanned.inner.to_owned()),
                )));
            }
            AssignmentLhs::ArrayAccess { left, right } => {
                context.push(&**left);
                context.push(&**right);
                context.push_instruction(self.span.make_wrapped(InstructionRelative::Dup(2)));
                context.push_instruction(self.span.make_wrapped(InstructionRelative::ArrayIndex));
            }
        }
        match &self.inner.rhs {
            AssignmentLhs::Variable(spanned) => {
                context.push_instruction(self.span.make_wrapped(InstructionRelative::Push(
                    PushSource::Environment(spanned.inner.to_owned()),
                )));
                context
                    .push_instruction(self.span.make_wrapped(InstructionRelative::SwapNth(0, 1)));
            }
            AssignmentLhs::ArrayAccess { left, right } => {
                context.push(&**left);
                context.push(&**right);
                context.push_instruction(self.span.make_wrapped(InstructionRelative::Dup(2)));
                context.push_instruction(self.span.make_wrapped(InstructionRelative::ArrayIndex));
                context
                    .push_instruction(self.span.make_wrapped(InstructionRelative::SwapNth(0, 3)));
            }
        }
        match &self.inner.rhs {
            AssignmentLhs::Variable(spanned) => {
                context.push_instruction(self.span.make_wrapped(InstructionRelative::Pop(
                    PopDestination::Environment(spanned.inner.to_owned()),
                )));
            }
            AssignmentLhs::ArrayAccess { .. } => {
                context.push_instruction(
                    self.span
                        .make_wrapped(InstructionRelative::Pop(PopDestination::ArrayAccess)),
                );
            }
        }
        match &self.inner.lhs {
            AssignmentLhs::Variable(spanned) => {
                context.push_instruction(self.span.make_wrapped(InstructionRelative::Pop(
                    PopDestination::Environment(spanned.inner.to_owned()),
                )));
            }
            AssignmentLhs::ArrayAccess { .. } => {
                context.push_instruction(
                    self.span
                        .make_wrapped(InstructionRelative::Pop(PopDestination::ArrayAccess)),
                );
            }
        }
    }
}

impl GenerateInstructions for Spanned<AssignmentStatement<'_>> {
    fn generate_instructions(&self, context: &mut InstructionGenerationContext) {
        match &self.inner.lhs {
            crate::statement::AssignmentLhs::Variable(identifier) => {
                context.push(&self.inner.expression);
                context.push_instruction(self.span.make_wrapped(InstructionRelative::Pop(
                    PopDestination::Environment(identifier.inner.to_owned()),
                )));
            }
            crate::statement::AssignmentLhs::ArrayAccess { left, right } => {
                context.push(&**left);
                context.push(&**right);
                context.push(&self.inner.expression);
                context.push_instruction(
                    self.span
                        .make_wrapped(InstructionRelative::Pop(PopDestination::ArrayAccess)),
                );
            }
        }
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
                            lhs: AssignmentLhs::Variable(
                                range_span.make_wrapped(self.inner.loop_variable),
                            ),
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

        // Fallback return of None if no other return was encountered.
        context.push(
            &self
                .span
                .make_wrapped(Statement::Return(ReturnStatement { expr: None })),
        );
    }
}

impl GenerateInstructions for AstRoot<'_> {
    fn generate_instructions(&self, context: &mut InstructionGenerationContext) {
        context.push(&self.main_algorithm.inner);

        // Fallback return of None if no other return was encountered.
        context.push(
            &self
                .main_algorithm
                .span
                .make_wrapped(Statement::Return(ReturnStatement { expr: None })),
        );

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
