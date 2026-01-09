use std::fmt::Display;

use chumsky::combinator::Validate;

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
    pub context_expr: Spanned<Expr<'a>>,
    // The expression for which the type is expected to be `expected`
    pub origin_expr: Spanned<Expr<'a>>,
    // The expected type
    pub expected: Type,
    // The actual type
    pub found: Type,
}

pub fn result_type<'a>(
    expr: &Spanned<Expr<'a>>,
    expected_and_context: Option<(Type, Spanned<Expr<'a>>)>,
) -> Result<Type, TypeError<'a>> {
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
                let _left_type = result_type(left, Some((Type::Number, expr.clone())))?;
                let _right_type = result_type(right, Some((Type::Number, expr.clone())))?;
                match (_left_type, _right_type) {
                    (Type::Integer | Type::Dynamic, Type::Integer | Type::Dynamic) => Type::Integer,
                    _ => Type::Number,
                }
            }
            BinaryOperator::Div => {
                let _left_type = result_type(left, Some((Type::Number, expr.clone())))?;
                let _right_type = result_type(right, Some((Type::Number, expr.clone())))?;
                Type::Number
            }
            BinaryOperator::And | BinaryOperator::Or => {
                let _left_type = result_type(left, Some((Type::Boolean, expr.clone())))?;
                let _right_type = result_type(right, Some((Type::Boolean, expr.clone())))?;
                Type::Boolean
            }
            BinaryOperator::Lt | BinaryOperator::Lte | BinaryOperator::Gt | BinaryOperator::Gte => {
                let _left_type = result_type(left, Some((Type::Number, expr.clone())))?;
                let _right_type = result_type(right, Some((Type::Number, expr.clone())))?;
                Type::Boolean
            }
            BinaryOperator::Eq | BinaryOperator::Neq => {
                let left_type = result_type(left, None)?;
                let _right_type = result_type(right, Some((left_type, expr.clone())))?;
                Type::Boolean
            }
        },
        Expr::VariableAccess(_) => Type::Dynamic,
        Expr::ArrayAccess { left, right } => {
            let _left_type = result_type(left, Some((Type::Array, expr.clone())))?;

            match &right.inner {
                ArrayIndex::SingleIndex(index) => {
                    let _index_type = result_type(index, Some((Type::Integer, expr.clone())))?;
                }
                ArrayIndex::Slice {
                    start,
                    end,
                    colon: _,
                } => {
                    if let Some(start_expr) = start {
                        let _start_type =
                            result_type(start_expr, Some((Type::Integer, expr.clone())))?;
                    }
                    if let Some(end_expr) = end {
                        let _end_type = result_type(end_expr, Some((Type::Integer, expr.clone())))?;
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
                let _expr_type = result_type(expr, Some((Type::Number, *expr.clone())))?;
                Type::Number
            }
            UnaryOperator::Not => {
                let _expr_type = result_type(expr, Some((Type::Boolean, *expr.clone())))?;
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
        return Err(TypeError {
            context_expr,
            origin_expr: expr.clone(),
            expected: expected_type,
            found: ty,
        });
    }

    Ok(ty)
}

pub trait ValidateTypes {
    fn validate_types(&self) -> Result<(), TypeError>;
}

impl ValidateTypes for AstRoot<'_> {
    fn validate_types(&self) -> Result<(), TypeError> {
        for procedure in &self.procedures {
            procedure.validate_types()?;
        }
        for statement in &self.statements.0 {
            statement.inner.validate_types()?;
        }
        Ok(())
    }
}

impl ValidateTypes for ProcedureDefinition<'_> {
    fn validate_types(&self) -> Result<(), TypeError> {
        self.body.validate_types()
    }
}

impl ValidateTypes for Statement<'_> {
    fn validate_types(&self) -> Result<(), TypeError> {
        match &self {
            Statement::Goto(stmt) => stmt.validate_types(),
            Statement::Swap(stmt) => stmt.validate_types(),
            Statement::Assignment(stmt) => stmt.validate_types(),
            Statement::If(stmt) => stmt.validate_types(),
            Statement::While(stmt) => stmt.validate_types(),
            Statement::For(stmt) => stmt.validate_types(),
            Statement::Return(stmt) => stmt.validate_types(),
            Statement::BareExpr(expr) => expr.validate_types(),
        }
    }
}

impl ValidateTypes for GotoStatement {
    fn validate_types(&self) -> Result<(), TypeError> {
        Ok(())
    }
}

impl ValidateTypes for SwapStatement<'_> {
    fn validate_types(&self) -> Result<(), TypeError> {
        Ok(())
    }
}

impl ValidateTypes for AssignmentStatement<'_> {
    fn validate_types(&self) -> Result<(), TypeError> {
        self.expression.validate_types()
    }
}

impl ValidateTypes for IfStatement<'_> {
    fn validate_types(&self) -> Result<(), TypeError> {
        self.condition.validate_types()?;
        self.then_branch.validate_types()?;
        if let Some(else_branch) = &self.else_branch {
            else_branch.validate_types()?;
        }
        Ok(())
    }
}

impl ValidateTypes for WhileStatement<'_> {
    fn validate_types(&self) -> Result<(), TypeError> {
        self.condition.validate_types()?;
        self.body.validate_types()?;
        Ok(())
    }
}

impl ValidateTypes for ForStatement<'_> {
    fn validate_types(&self) -> Result<(), TypeError> {
        self.start_expr.validate_types()?;
        self.end_expr.validate_types()?;
        self.body.validate_types()?;
        Ok(())
    }
}

impl ValidateTypes for ReturnStatement<'_> {
    fn validate_types(&self) -> Result<(), TypeError> {
        if let Some(expr) = &self.expr {
            expr.validate_types()?;
        }
        Ok(())
    }
}

impl ValidateTypes for Block<'_> {
    fn validate_types(&self) -> Result<(), TypeError> {
        for statement in &self.0 {
            statement.inner.validate_types()?;
        }
        Ok(())
    }
}

impl ValidateTypes for Spanned<Expr<'_>> {
    fn validate_types(&self) -> Result<(), TypeError> {
        result_type(self, None).map(drop)
    }
}
