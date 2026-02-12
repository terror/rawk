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
pub(crate) struct FunctionDefinition {
  pub(crate) body: Block,
  pub(crate) name: String,
  pub(crate) parameters: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct PatternAction {
  pub(crate) action: Block,
  pub(crate) pattern: Option<Pattern>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Pattern {
  Begin,
  End,
  ExpressionStub,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Block {
  pub(crate) items: Vec<BlockItem>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum BlockItem {
  Block(Block),
  TokenStub,
}
