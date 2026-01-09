use crate::{token::Token, util::Spanned};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,

    And,
    Or,

    Lt,
    Gt,
    Lte,
    Gte,
    Eq,
    Neq,
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op_str = match self {
            BinaryOperator::Add => "+",
            BinaryOperator::Sub => "-",
            BinaryOperator::Mul => "*",
            BinaryOperator::Div => "/",
            BinaryOperator::And => "and",
            BinaryOperator::Or => "or",
            BinaryOperator::Lt => "<",
            BinaryOperator::Gt => ">",
            BinaryOperator::Lte => "<=",
            BinaryOperator::Gte => ">=",
            BinaryOperator::Eq => "==",
            BinaryOperator::Neq => "!=",
        };
        write!(f, "{}", op_str)
    }
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum BinaryOperationFromTokenError<'a> {
    #[error("Token is not a binary operator: {0:?}")]
    NotABinaryOperator(Token<'a>),
}

impl<'a> TryFrom<Token<'a>> for BinaryOperator {
    type Error = BinaryOperationFromTokenError<'a>;

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::Add => Ok(BinaryOperator::Add),
            Token::Subtract => Ok(BinaryOperator::Sub),
            Token::Multiply => Ok(BinaryOperator::Mul),
            Token::Divide => Ok(BinaryOperator::Div),
            Token::And => Ok(BinaryOperator::And),
            Token::Or => Ok(BinaryOperator::Or),
            Token::Lt => Ok(BinaryOperator::Lt),
            Token::Gt => Ok(BinaryOperator::Gt),
            Token::Lte => Ok(BinaryOperator::Lte),
            Token::Gte => Ok(BinaryOperator::Gte),
            Token::Eq => Ok(BinaryOperator::Eq),
            Token::Neq => Ok(BinaryOperator::Neq),
            _ => Err(BinaryOperationFromTokenError::NotABinaryOperator(value)),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum UnaryOperator {
    Neg,
    Not,
}

impl std::fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op_str = match self {
            UnaryOperator::Neg => "-",
            UnaryOperator::Not => "not",
        };
        write!(f, "{}", op_str)
    }
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum ArrayIndex<'a> {
    SingleIndex(Box<Spanned<Expr<'a>>>),
    Slice {
        start: Option<Box<Spanned<Expr<'a>>>>,
        colon: Spanned<()>,
        end: Option<Box<Spanned<Expr<'a>>>>,
    },
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum Expr<'a> {
    NumberLiteral(f64),
    BooleanLiteral(bool),
    VariableAccess(&'a str),
    FunctionCall {
        left: Spanned<&'a str>,
        arguments: Spanned<Vec<Spanned<Expr<'a>>>>,
    },
    ArrayAccess {
        left: Box<Spanned<Expr<'a>>>,
        right: Spanned<ArrayIndex<'a>>,
    },
    BinaryOp {
        left: Box<Spanned<Expr<'a>>>,
        op: Spanned<BinaryOperator>,
        right: Box<Spanned<Expr<'a>>>,
    },
    UnaryOp {
        op: Spanned<UnaryOperator>,
        expr: Box<Spanned<Expr<'a>>>,
    },
}
