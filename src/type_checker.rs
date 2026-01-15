use std::fmt::Display;

use ariadne::{Color, Label, Report, ReportKind, sources};
use chumsky::span::WrappingSpan;

use crate::expr::{ArrayIndex, BinaryOperator, Expr, UnaryOperator};
use crate::util::{SourceSpan, Spanned};

use crate::parser::{
    AssignmentStatement, AstRoot, Block, ForStatement, GotoStatement, IfStatement,
    ProcedureDefinition, ReturnStatement, Statement, SwapStatement, WhileStatement,
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Type {
    Dynamic,
    Number,
    Integer,
    Boolean,
    Array,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Dynamic => write!(f, "dynamic"),
            Type::Number => write!(f, "number"),
            Type::Boolean => write!(f, "boolean"),
            Type::Array => write!(f, "array"),
            Type::Integer => write!(f, "integer"),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct TypeError<'a> {
    // The context in which the type is expected
    pub context: TypeErrorContext<'a>,
    // The expression for which the type is expected to be `expected`
    pub origin_expr: Spanned<Expr<'a>>,
    // The expected type
    pub expected: Type,
    // The actual type
    pub found: Type,
}

#[derive(Clone, PartialEq, Debug)]
pub enum TypeErrorContext<'a> {
    SubExprOf(Spanned<Expr<'a>>), // the checked expression is a sub-expression of another expression
    IfStatementCond(SourceSpan),  // the checked expression is a condition of an if statement
    WhileStatementCond(SourceSpan), // the checked expression is a condition of a while statement
    ForStatementRange(SourceSpan), // the checked expression is part of the range of a for statement
    Other,
}

impl<'a> From<Spanned<Expr<'a>>> for TypeErrorContext<'a> {
    fn from(expr: Spanned<Expr<'a>>) -> Self {
        TypeErrorContext::SubExprOf(expr)
    }
}

fn result_type_inner<'a>(
    expr: &Spanned<Expr<'a>>,
    expectation_and_context: Option<(Type, TypeErrorContext<'a>)>,
    errs: &mut Vec<TypeError<'a>>,
) -> Type {
    // helper which pushes errors into `errs` and returns the computed Type
    let ty = match &expr.inner {
        Expr::NumberLiteral(val) => {
            if val.fract() == 0.0 {
                Type::Integer
            } else {
                Type::Number
            }
        }
        Expr::BooleanLiteral(_) => Type::Boolean,
        Expr::Build(_) => Type::Array,
        Expr::ArrayLiteral(_) => Type::Array,
        Expr::BinaryOp { left, op, right } => match op.inner {
            BinaryOperator::Add | BinaryOperator::Sub | BinaryOperator::Mul => {
                let left_type =
                    result_type_inner(left, Some((Type::Number, expr.clone().into())), errs);
                let right_type =
                    result_type_inner(right, Some((Type::Number, expr.clone().into())), errs);

                match (left_type, right_type) {
                    (Type::Integer | Type::Dynamic, Type::Integer | Type::Dynamic) => Type::Integer,
                    _ => Type::Number,
                }
            }
            BinaryOperator::Div => {
                let _left_type =
                    result_type_inner(left, Some((Type::Number, expr.clone().into())), errs);
                let _right_type =
                    result_type_inner(right, Some((Type::Number, expr.clone().into())), errs);
                Type::Number
            }
            BinaryOperator::And | BinaryOperator::Or => {
                let _left_type =
                    result_type_inner(left, Some((Type::Boolean, expr.clone().into())), errs);
                let _right_type =
                    result_type_inner(right, Some((Type::Boolean, expr.clone().into())), errs);
                Type::Boolean
            }
            BinaryOperator::Lt | BinaryOperator::Lte | BinaryOperator::Gt | BinaryOperator::Gte => {
                let _left_type =
                    result_type_inner(left, Some((Type::Number, expr.clone().into())), errs);
                let _right_type =
                    result_type_inner(right, Some((Type::Number, expr.clone().into())), errs);
                Type::Boolean
            }
            BinaryOperator::Eq | BinaryOperator::Neq => {
                let left_type = result_type_inner(left, None, errs);
                let _right_type =
                    result_type_inner(right, Some((left_type, expr.clone().into())), errs);
                Type::Boolean
            }
        },
        Expr::VariableAccess(_) => Type::Dynamic,
        Expr::ArrayAccess { left, right } => {
            let _left_type =
                result_type_inner(left, Some((Type::Array, expr.clone().into())), errs);

            match &right.inner {
                ArrayIndex::SingleIndex(index) => {
                    let _index_type =
                        result_type_inner(index, Some((Type::Integer, expr.clone().into())), errs);
                }
                ArrayIndex::Slice {
                    start,
                    end,
                    colon: _,
                } => {
                    if let Some(start_expr) = start {
                        let _start_type = result_type_inner(
                            start_expr,
                            Some((Type::Integer, expr.clone().into())),
                            errs,
                        );
                    }
                    if let Some(end_expr) = end {
                        let _end_type = result_type_inner(
                            end_expr,
                            Some((Type::Integer, expr.clone().into())),
                            errs,
                        );
                    }
                }
            }

            Type::Dynamic
        }
        Expr::FunctionCall { left: _, arguments } => {
            for arg in &arguments.inner {
                let _arg_type = result_type_inner(arg, None, errs);
            }

            Type::Dynamic
        }
        Expr::UnaryOp { op, expr } => match op.inner {
            UnaryOperator::Neg => {
                let _expr_type =
                    result_type_inner(expr, Some((Type::Number, (**expr).clone().into())), errs);
                Type::Number
            }
            UnaryOperator::Not => {
                let _expr_type =
                    result_type_inner(expr, Some((Type::Boolean, (**expr).clone().into())), errs);
                Type::Boolean
            }
        },
    };

    if let Some((expected_type, context)) = expectation_and_context
        // Only report a type mismatch if the type is known
        && ty != Type::Dynamic
        // Any type can be used where Dynamic is expected
        && expected_type != Type::Dynamic
        && ty != expected_type
        // Integer can be used where Number is expected, but not vice versa
        && !(expected_type == Type::Number && ty == Type::Integer)
    {
        errs.push(TypeError {
            context,
            origin_expr: expr.clone(),
            expected: expected_type,
            found: ty,
        });
    }

    ty
}

pub fn result_type<'a>(
    expr: &Spanned<Expr<'a>>,
    expected_and_context: Option<(Type, TypeErrorContext<'a>)>,
) -> (Type, Vec<TypeError<'a>>) {
    let mut errs = Vec::new();
    let ty = result_type_inner(expr, expected_and_context, &mut errs);
    (ty, errs)
}

pub trait ValidateTypes<'a> {
    fn validate_types_into(&self, errs: &mut Vec<TypeError<'a>>);

    fn validate_types(&self) -> Vec<TypeError<'a>> {
        let mut errs = Vec::new();
        self.validate_types_into(&mut errs);
        errs
    }
}

impl<'a> ValidateTypes<'a> for AstRoot<'a> {
    fn validate_types_into(&self, errs: &mut Vec<TypeError<'a>>) {
        for procedure in &self.procedures {
            procedure.validate_types_into(errs);
        }
        for statement in &self.main_algorithm.0 {
            statement.validate_types_into(errs);
        }
    }
}

impl<'a> ValidateTypes<'a> for ProcedureDefinition<'a> {
    fn validate_types_into(&self, errs: &mut Vec<TypeError<'a>>) {
        self.body.validate_types_into(errs);
    }
}

impl<'a> ValidateTypes<'a> for Spanned<Statement<'a>> {
    fn validate_types_into(&self, errs: &mut Vec<TypeError<'a>>) {
        match &self.inner {
            Statement::Goto(stmt) => stmt.validate_types_into(errs),
            Statement::Swap(stmt) => stmt.validate_types_into(errs),
            Statement::Assignment(stmt) => stmt.validate_types_into(errs),
            Statement::If(stmt) => self
                .span
                .make_wrapped(stmt.clone())
                .validate_types_into(errs),
            Statement::While(stmt) => self
                .span
                .make_wrapped(stmt.clone())
                .validate_types_into(errs),
            Statement::For(stmt) => self
                .span
                .make_wrapped(stmt.clone())
                .validate_types_into(errs),
            Statement::Return(stmt) => self
                .span
                .make_wrapped(stmt.clone())
                .validate_types_into(errs),
            Statement::BareExpr(stmt) => self
                .span
                .make_wrapped(stmt.clone())
                .validate_types_into(errs),
        }
    }
}

impl<'a> ValidateTypes<'a> for GotoStatement {
    fn validate_types_into(&self, _errs: &mut Vec<TypeError<'a>>) {}
}

impl<'a> ValidateTypes<'a> for SwapStatement<'a> {
    fn validate_types_into(&self, _errs: &mut Vec<TypeError<'a>>) {}
}

impl<'a> ValidateTypes<'a> for AssignmentStatement<'a> {
    fn validate_types_into(&self, errs: &mut Vec<TypeError<'a>>) {
        let _ = result_type_inner(&self.expression, None, errs);
    }
}

impl<'a> ValidateTypes<'a> for Spanned<IfStatement<'a>> {
    fn validate_types_into(&self, errs: &mut Vec<TypeError<'a>>) {
        let _ = result_type_inner(
            &self.condition,
            Some((Type::Boolean, TypeErrorContext::IfStatementCond(self.span))),
            errs,
        );
        self.then_branch.validate_types_into(errs);
        if let Some(else_branch) = &self.else_branch {
            else_branch.validate_types_into(errs);
        }
    }
}

impl<'a> ValidateTypes<'a> for Spanned<WhileStatement<'a>> {
    fn validate_types_into(&self, errs: &mut Vec<TypeError<'a>>) {
        let _ = result_type_inner(
            &self.condition,
            Some((
                Type::Boolean,
                TypeErrorContext::WhileStatementCond(self.span),
            )),
            errs,
        );
        self.body.validate_types_into(errs);
    }
}

impl<'a> ValidateTypes<'a> for Spanned<ForStatement<'a>> {
    fn validate_types_into(&self, errs: &mut Vec<TypeError<'a>>) {
        let _ = result_type_inner(
            &self.start_expr,
            Some((
                Type::Integer,
                TypeErrorContext::ForStatementRange(self.span),
            )),
            errs,
        );
        let _ = result_type_inner(
            &self.end_expr,
            Some((
                Type::Integer,
                TypeErrorContext::ForStatementRange(self.span),
            )),
            errs,
        );
        self.body.validate_types_into(errs);
    }
}

impl<'a> ValidateTypes<'a> for Spanned<ReturnStatement<'a>> {
    fn validate_types_into(&self, errs: &mut Vec<TypeError<'a>>) {
        if let Some(expr) = &self.expr {
            let _ = result_type_inner(expr, None, errs);
        }
    }
}

impl<'a> ValidateTypes<'a> for Block<'a> {
    fn validate_types_into(&self, errs: &mut Vec<TypeError<'a>>) {
        for statement in &self.0 {
            statement.validate_types_into(errs);
        }
    }
}

impl<'a> ValidateTypes<'a> for Spanned<Expr<'a>> {
    fn validate_types_into(&self, errs: &mut Vec<TypeError<'a>>) {
        let _ = result_type_inner(self, None, errs);
    }
}

pub fn process_type_errors<'a>(src: &'a str, file_name: String, type_errors: &[TypeError<'a>]) {
    for e in type_errors {
        let hint_span = (
            file_name.clone(),
            match &e.context {
                TypeErrorContext::SubExprOf(context) => match &context.inner {
                    Expr::BinaryOp { op, .. } => op.span.start.bytes..op.span.end.bytes,
                    Expr::ArrayAccess { left, right } => {
                        right.span.start.bytes..right.span.end.bytes
                    }
                    _ => context.span.start.bytes..context.span.end.bytes,
                },
                TypeErrorContext::IfStatementCond(span) => span.start.bytes..span.end.bytes,
                TypeErrorContext::WhileStatementCond(span) => span.start.bytes..span.end.bytes,
                TypeErrorContext::ForStatementRange(span) => span.start.bytes..span.end.bytes,
                TypeErrorContext::Other => {
                    e.origin_expr.span.start.bytes..e.origin_expr.span.end.bytes
                }
            },
        );

        let hint_message = match &e.context {
            TypeErrorContext::SubExprOf(context) => match &context.inner {
                Expr::BinaryOp { op, .. } => {
                    let op_name = op.inner.to_string();

                    format!(
                        "hint: operator '{}' expects operands of type {}",
                        op_name, e.expected
                    )
                }
                Expr::UnaryOp { op, .. } => {
                    format!(
                        "hint: unary operator '{}' expects operand of type {}",
                        op.inner, e.expected
                    )
                }
                Expr::FunctionCall { .. } => {
                    panic!("function args are dynamic so this code should never run")
                }
                Expr::ArrayAccess { left, right } => match &right.inner {
                    ArrayIndex::SingleIndex(idx) => {
                        if e.origin_expr.span == idx.span {
                            format!(
                                "hint: the right side of this subscript operation must be of type {}",
                                e.expected
                            )
                        } else if e.origin_expr.span == left.span {
                            format!(
                                "hint: the left side of this subscript operation must be of type {}",
                                e.expected
                            )
                        } else {
                            panic!("type error context does not match any sub-expression")
                        }
                    }
                    ArrayIndex::Slice { start, end, .. } => {
                        if Some(e.origin_expr.span) == start.as_ref().map(|s| s.span)
                            || Some(e.origin_expr.span) == end.as_ref().map(|s| s.span)
                        {
                            format!(
                                "hint: indices of this slice operation must be of type {}",
                                e.expected
                            )
                        } else if e.origin_expr.span == left.span {
                            format!(
                                "hint: the left side of this slice operation must be of type {}",
                                e.expected
                            )
                        } else {
                            panic!("type error context does not match any sub-expression")
                        }
                    }
                },
                _ => panic!("type error context does not match any sub-expression"),
            },
            TypeErrorContext::IfStatementCond(_) => format!(
                "hint: the condition for this if statement must be of type {}",
                e.expected
            ),
            TypeErrorContext::WhileStatementCond(_) => format!(
                "hint: the condition for this while loop must be of type {}",
                e.expected
            ),
            TypeErrorContext::ForStatementRange(_) => format!(
                "hint: the bounds for this for loop must be of type {}",
                e.expected
            ),
            TypeErrorContext::Other => todo!(),
        };

        let hint = Label::new(hint_span)
            .with_color(Color::Yellow)
            .with_message(hint_message);

        let report = Report::build(
            ReportKind::Error,
            (
                file_name.clone(),
                e.origin_expr.span.start.bytes..e.origin_expr.span.end.bytes,
            ),
        )
        .with_message(format!(
            "Type mismatch: expected {}, found {}",
            e.expected, e.found
        ))
        .with_labels([
            Label::new((
                file_name.clone(),
                e.origin_expr.span.start.bytes..e.origin_expr.span.end.bytes,
            ))
            .with_message(format!(
                "expected type {}, but this expression results in type {}",
                e.expected, e.found
            ))
            .with_color(Color::Red),
            hint,
        ])
        .finish();

        report.print(sources([(file_name.clone(), src)])).unwrap();
    }
}
