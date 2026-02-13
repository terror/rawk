#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum AssignOp {
  Add,
  Assign,
  Divide,
  Modulo,
  Multiply,
  Power,
  Subtract,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum BinaryOp {
  Add,
  And,

  Divide,
  Equal,
  Greater,
  GreaterEqual,
  In,
  Less,
  LessEqual,
  Match,
  Modulo,
  Multiply,
  NotEqual,
  NotMatch,
  Or,
  Power,
  Subtract,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Block {
  pub(crate) items: Vec<BlockItem>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum BlockItem {
  Block(Block),
  Expression(Expression),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Expression {
  Assignment {
    operator: AssignOp,
    target: Box<Expression>,
    value: Box<Expression>,
  },
  Binary {
    left: Box<Expression>,
    operator: BinaryOp,
    right: Box<Expression>,
  },
  FunctionCall {
    arguments: Vec<Expression>,
    name: String,
  },
  Identifier(String),
  Index {
    indices: Vec<Expression>,
    name: String,
  },
  Number(String),
  PostDecrement(Box<Expression>),
  PostIncrement(Box<Expression>),
  PreDecrement(Box<Expression>),
  PreIncrement(Box<Expression>),
  Regex(String),
  String(String),
  Ternary {
    condition: Box<Expression>,
    else_branch: Box<Expression>,
    then_branch: Box<Expression>,
  },
  Unary {
    operand: Box<Expression>,
    operator: UnaryOp,
  },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FunctionDefinition {
  pub(crate) body: Block,
  pub(crate) name: String,
  pub(crate) parameters: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Pattern {
  Begin,
  End,
  Expression(Expression),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct PatternAction {
  pub(crate) action: Block,
  pub(crate) pattern: Option<Pattern>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Program {
  pub(crate) items: Vec<TopLevelItem>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum TopLevelItem {
  Function(FunctionDefinition),
  PatternAction(PatternAction),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum UnaryOp {
  FieldAccess,
  Negate,
  Not,
  Positive,
}
