use crate::util::SourceSpan;

enum BinaryOperation {
    Add,
    Subtract,
    Multiply,
    Divide,
    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    Neq,
    And,
    Or,
    Assign,
}

enum UnaryOperation {
    Negate,
    Not,
}

struct FunctionMetadata {
    name: String,
    parameters: Vec<String>,
    offset: usize,
}

enum Value {
    Number(f64),
    Bool(bool),
    String(String),
    Function(FunctionMetadata),
}

enum Label {
    Function(String),
    Line(usize),
}

// Generic instruction form where control-flow targets can vary (labels in pass 1, indices in pass 2).
enum Instruction<Target> {
    // Special instruction that marks the beginning of a function. This is used to validate the parameter list during FunctionCall instructions.
    FunctionHeader {
        parameters: Vec<String>,
    },
    Push(Value),
    PushFromEnvironment(String),
    PopIntoEnvironment(String),
    Binary {
        op: BinaryOperation,
        lhs_span: SourceSpan,
        rhs_span: SourceSpan,
    },
    Unary(UnaryOperation),
    // The same as a jump but it pushes a new environment frame to the procedure stack (which is separate from the instruction stack)
    FunctionCall {
        target: Target,
        arg_count: usize,
    },
    Jump {
        is_conditional: bool,
        target: Target,
    },
    ReturnNothing,
    ReturnValue,
}

// Uses labels to reference instructions as the labels are only during the second pass.
type InstructionRelative = Instruction<Label>;
// Uses instruction indices instead of labels.
type InstructionAbsolute = Instruction<usize>;
