use std::fmt::Display;

use crate::expr::{ArrayIndex, BinaryOperator, Expr, UnaryOperator};
use crate::util::Spanned;

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
    pub context_expr: Spanned<Expr<'a>>,
    // The expression for which the type is expected to be `expected`
    pub origin_expr: Spanned<Expr<'a>>,
    // The expected type
    pub expected: Type,
    // The actual type
    pub found: Type,
}

enum Context<'a> {
    SubExprOf(Spanned<Expr<'a>>), // the checked expression is a sub-expression of another expression
    IfStatementCond,              // the checked expression is a condition of an if statement
    WhileStatementCond,           // the checked expression is a condition of a while statement
    ForStatementRange,            // the checked expression is part of the range of a for statement
    Other,
}

fn result_type_inner<'a>(
    expr: &Spanned<Expr<'a>>,
    expected_and_context: Option<(Type, Spanned<Expr<'a>>)>,
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
        Expr::BinaryOp { left, op, right } => match op.inner {
            BinaryOperator::Add | BinaryOperator::Sub | BinaryOperator::Mul => {
                let left_type = result_type_inner(left, Some((Type::Number, expr.clone())), errs);
                let right_type = result_type_inner(right, Some((Type::Number, expr.clone())), errs);

                match (left_type, right_type) {
                    (Type::Integer | Type::Dynamic, Type::Integer | Type::Dynamic) => Type::Integer,
                    _ => Type::Number,
                }
            }
            BinaryOperator::Div => {
                let _left_type = result_type_inner(left, Some((Type::Number, expr.clone())), errs);
                let _right_type =
                    result_type_inner(right, Some((Type::Number, expr.clone())), errs);
                Type::Number
            }
            BinaryOperator::And | BinaryOperator::Or => {
                let _left_type = result_type_inner(left, Some((Type::Boolean, expr.clone())), errs);
                let _right_type =
                    result_type_inner(right, Some((Type::Boolean, expr.clone())), errs);
                Type::Boolean
            }
            BinaryOperator::Lt | BinaryOperator::Lte | BinaryOperator::Gt | BinaryOperator::Gte => {
                let _left_type = result_type_inner(left, Some((Type::Number, expr.clone())), errs);
                let _right_type =
                    result_type_inner(right, Some((Type::Number, expr.clone())), errs);
                Type::Boolean
            }
            BinaryOperator::Eq | BinaryOperator::Neq => {
                let left_type = result_type_inner(left, None, errs);
                let _right_type = result_type_inner(right, Some((left_type, expr.clone())), errs);
                Type::Boolean
            }
        },
        Expr::VariableAccess(_) => Type::Dynamic,
        Expr::ArrayAccess { left, right } => {
            let _left_type = result_type_inner(left, Some((Type::Array, expr.clone())), errs);

            match &right.inner {
                ArrayIndex::SingleIndex(index) => {
                    let _index_type =
                        result_type_inner(index, Some((Type::Integer, expr.clone())), errs);
                }
                ArrayIndex::Slice {
                    start,
                    end,
                    colon: _,
                } => {
                    if let Some(start_expr) = start {
                        let _start_type = result_type_inner(
                            start_expr,
                            Some((Type::Integer, expr.clone())),
                            errs,
                        );
                    }
                    if let Some(end_expr) = end {
                        let _end_type =
                            result_type_inner(end_expr, Some((Type::Integer, expr.clone())), errs);
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
                let _expr_type = result_type_inner(expr, Some((Type::Number, *expr.clone())), errs);
                Type::Number
            }
            UnaryOperator::Not => {
                let _expr_type =
                    result_type_inner(expr, Some((Type::Boolean, *expr.clone())), errs);
                Type::Boolean
            }
        },
    };

    if let Some((expected_type, context_expr)) = expected_and_context
        // Only report a type mismatch if the type is known
        && ty != Type::Dynamic
        // Any type can be used where Dynamic is expected
        && expected_type != Type::Dynamic
        && ty != expected_type
        // Integer can be used where Number is expected, but not vice versa
        && !(expected_type == Type::Number && ty == Type::Integer)
    {
        errs.push(TypeError {
            context_expr,
            origin_expr: expr.clone(),
            expected: expected_type,
            found: ty,
        });
    }

    ty
}

pub fn result_type<'a>(
    expr: &Spanned<Expr<'a>>,
    expected_and_context: Option<(Type, Spanned<Expr<'a>>)>,
) -> (Type, Vec<TypeError<'a>>) {
    let mut errs = Vec::new();
    let ty = result_type_inner(expr, expected_and_context, &mut errs);
    (ty, errs)
}

pub trait ValidateTypes {
    fn validate_types_into<'b>(&'b self, errs: &mut Vec<TypeError<'b>>);

    fn validate_types(&self) -> Vec<TypeError<'_>> {
        let mut errs = Vec::new();
        self.validate_types_into(&mut errs);
        errs
    }
}

impl<'a> ValidateTypes for AstRoot<'a> {
    fn validate_types_into<'b>(&'b self, errs: &mut Vec<TypeError<'b>>) {
        for procedure in &self.procedures {
            procedure.validate_types_into(errs);
        }
        for statement in &self.statements.0 {
            statement.inner.validate_types_into(errs);
        }
    }
}

impl<'a> ValidateTypes for ProcedureDefinition<'a> {
    fn validate_types_into<'b>(&'b self, errs: &mut Vec<TypeError<'b>>) {
        self.body.validate_types_into(errs);
    }
}

impl<'a> ValidateTypes for Statement<'a> {
    fn validate_types_into<'b>(&'b self, errs: &mut Vec<TypeError<'b>>) {
        match &self {
            Statement::Goto(stmt) => stmt.validate_types_into(errs),
            Statement::Swap(stmt) => stmt.validate_types_into(errs),
            Statement::Assignment(stmt) => stmt.validate_types_into(errs),
            Statement::If(stmt) => stmt.validate_types_into(errs),
            Statement::While(stmt) => stmt.validate_types_into(errs),
            Statement::For(stmt) => stmt.validate_types_into(errs),
            Statement::Return(stmt) => stmt.validate_types_into(errs),
            Statement::BareExpr(stmt) => stmt.validate_types_into(errs),
        }
    }
}

impl<'a> ValidateTypes for GotoStatement {
    fn validate_types_into<'b>(&'b self, _errs: &mut Vec<TypeError<'b>>) {}
}

impl<'a> ValidateTypes for SwapStatement<'a> {
    fn validate_types_into<'b>(&'b self, _errs: &mut Vec<TypeError<'b>>) {}
}

impl<'a> ValidateTypes for AssignmentStatement<'a> {
    fn validate_types_into<'b>(&'b self, errs: &mut Vec<TypeError<'b>>) {
        let _ = result_type_inner(&self.expression, None, errs);
    }
}

impl<'a> ValidateTypes for IfStatement<'a> {
    fn validate_types_into<'b>(&'b self, errs: &mut Vec<TypeError<'b>>) {
        let _ = result_type_inner(&self.condition, None, errs);
        self.then_branch.validate_types_into(errs);
        if let Some(else_branch) = &self.else_branch {
            else_branch.validate_types_into(errs);
        }
    }
}

impl<'a> ValidateTypes for WhileStatement<'a> {
    fn validate_types_into<'b>(&'b self, errs: &mut Vec<TypeError<'b>>) {
        let _ = result_type_inner(&self.condition, None, errs);
        self.body.validate_types_into(errs);
    }
}

impl<'a> ValidateTypes for ForStatement<'a> {
    fn validate_types_into<'b>(&'b self, errs: &mut Vec<TypeError<'b>>) {
        let _ = result_type_inner(&self.start_expr, None, errs);
        let _ = result_type_inner(&self.end_expr, None, errs);
        self.body.validate_types_into(errs);
    }
}

impl<'a> ValidateTypes for ReturnStatement<'a> {
    fn validate_types_into<'b>(&'b self, errs: &mut Vec<TypeError<'b>>) {
        if let Some(expr) = &self.expr {
            let _ = result_type_inner(expr, None, errs);
        }
    }
}

impl<'a> ValidateTypes for Block<'a> {
    fn validate_types_into<'b>(&'b self, errs: &mut Vec<TypeError<'b>>) {
        for statement in &self.0 {
            statement.inner.validate_types_into(errs);
        }
    }
}

impl<'a> ValidateTypes for Spanned<Expr<'a>> {
    fn validate_types_into<'b>(&'b self, errs: &mut Vec<TypeError<'b>>) {
        let _ = result_type_inner(self, None, errs);
    }
}
