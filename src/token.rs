use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Token {
  Begin,
  Comma,
  End,
  Function,
  Identifier(String),
  Integer(String),
  LBrace,
  LParen,
  RBrace,
  RParen,
  Semicolon,
  String(String),
}

impl Display for Token {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Self::Begin => write!(f, "BEGIN"),
      Self::Comma => write!(f, ","),
      Self::End => write!(f, "END"),
      Self::Function => write!(f, "function"),
      Self::Identifier(identifier) => write!(f, "{identifier}"),
      Self::Integer(integer) => write!(f, "{integer}"),
      Self::LBrace => write!(f, "{{"),
      Self::LParen => write!(f, "("),
      Self::RBrace => write!(f, "}}"),
      Self::RParen => write!(f, ")"),
      Self::Semicolon => write!(f, ";"),
      Self::String(string) => write!(f, "\"{string}\""),
    }
  }
}
