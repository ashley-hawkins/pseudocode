use crate::{expr::Expr, util::Spanned};

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct AssertStatement<'a> {
    pub condition: Spanned<Expr<'a>>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct GotoStatement {
    pub line_number: Spanned<usize>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum AssignmentLhs<'a> {
    Variable(Spanned<&'a str>),
    ArrayAccess {
        left: Box<Spanned<Expr<'a>>>,
        right: Box<Spanned<Expr<'a>>>,
    },
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct SwapStatement<'a> {
    pub lhs: AssignmentLhs<'a>,
    pub rhs: AssignmentLhs<'a>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct AssignmentStatement<'a> {
    pub lhs: AssignmentLhs<'a>,
    pub expression: Spanned<Expr<'a>>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct Block<'a>(pub Vec<Spanned<Statement<'a>>>);

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct IfStatement<'a> {
    pub condition: Spanned<Expr<'a>>,
    pub then_branch: Block<'a>,
    pub else_branch: Option<Block<'a>>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct WhileStatement<'a> {
    pub condition: Spanned<Expr<'a>>,
    pub body: Block<'a>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct ForStatement<'a> {
    pub loop_variable: &'a str,
    pub start_expr: Spanned<Expr<'a>>,
    pub end_expr: Spanned<Expr<'a>>,
    pub body: Block<'a>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct ReturnStatement<'a> {
    pub expr: Option<Spanned<Expr<'a>>>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum DebugArgument<'a> {
    String(&'a str),
    Expr(Spanned<Expr<'a>>),
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct DebugStatement<'a> {
    pub with_newline: bool,
    pub args: Vec<DebugArgument<'a>>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug, derive_more::From)]
pub enum Statement<'a> {
    Assert(AssertStatement<'a>),
    Goto(GotoStatement),
    Swap(SwapStatement<'a>),
    Assignment(AssignmentStatement<'a>),
    If(IfStatement<'a>),
    While(WhileStatement<'a>),
    For(ForStatement<'a>),
    Return(ReturnStatement<'a>),
    BareExpr(Spanned<Expr<'a>>),
    Debug(DebugStatement<'a>),
    DebugStack,
}
