use std::fmt::Display;

use chumsky::span::{SimpleSpan, Spanned};

use crate::parser::{ArrayIndex, BinaryOperator, Expr, UnaryOperator};

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

#[derive(Clone, PartialEq, PartialOrd, Debug, thiserror::Error)]
pub enum TypeError {
    #[error("Type mismatch: expected {expected}, but this expression has type {found}")]
    TypeMismatch {
        origin: SimpleSpan,
        expected: Type,
        found: Type,
    },
}

pub fn result_type<'a>(
    expr: &Spanned<Expr<'a>>,
    expected: Option<Type>,
) -> Result<Type, TypeError> {
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
            BinaryOperator::Add
            | BinaryOperator::Sub
            | BinaryOperator::Mul
            | BinaryOperator::Div => {
                let _left_type = result_type(left, Some(Type::Number))?;
                let _right_type = result_type(right, Some(Type::Number))?;
                Type::Number
            }
            BinaryOperator::And | BinaryOperator::Or => {
                let _left_type = result_type(left, Some(Type::Boolean))?;
                let _right_type = result_type(right, Some(Type::Boolean))?;
                Type::Boolean
            }
            BinaryOperator::Lt
            | BinaryOperator::Lte
            | BinaryOperator::Gt
            | BinaryOperator::Gte
            | BinaryOperator::Eq
            | BinaryOperator::Neq => {
                let _left_type = result_type(left, Some(Type::Number))?;
                let _right_type = result_type(right, Some(Type::Number))?;
                Type::Boolean
            }
        },
        Expr::VariableAccess(_) => Type::Dynamic,
        Expr::ArrayAccess { left, right } => {
            let _left_type = result_type(left, Some(Type::Array))?;

            match &right.inner {
                ArrayIndex::SingleIndex(index) => {
                    let _index_type = result_type(index, Some(Type::Number))?;
                }
                ArrayIndex::Slice {
                    start,
                    end,
                    colon: _,
                } => {
                    if let Some(start_expr) = start {
                        let _start_type = result_type(start_expr, Some(Type::Integer))?;
                    }
                    if let Some(end_expr) = end {
                        let _end_type = result_type(end_expr, Some(Type::Integer))?;
                    }
                }
            }

            Type::Dynamic
        }
        Expr::FunctionCall { left: _, arguments } => {
            for arg in &arguments.inner {
                let _arg_type = result_type(arg, None)?;
            }

            Type::Dynamic
        }
        Expr::UnaryOp { op, expr } => match op.inner {
            UnaryOperator::Neg => {
                let _expr_type = result_type(expr, Some(Type::Number))?;
                Type::Number
            }
            UnaryOperator::Not => {
                let _expr_type = result_type(expr, Some(Type::Boolean))?;
                Type::Boolean
            }
        },
    };

    if let Some(expected_type) = expected
        // Only report a type mismatch if the type is known
        && ty != Type::Dynamic
        // Any type can be used where Dynamic is expected
        && expected_type != Type::Dynamic
        && ty != expected_type
        // Integer can be used where Number is expected, but not vice versa
        && !(expected_type == Type::Number && ty == Type::Integer)
    {
        return Err(TypeError::TypeMismatch {
            origin: expr.span,
            expected: expected_type,
            found: ty,
        });
    }

    Ok(ty)
}
