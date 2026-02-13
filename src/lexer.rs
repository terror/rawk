use super::*;

pub(crate) type Span = SimpleSpan<usize>;
pub(crate) type Spanned<T> = (T, Span);
pub(crate) type LexError<'src> = Rich<'src, char, Span>;

pub(crate) fn lex(
  source: &str,
) -> (Option<Vec<Spanned<Token>>>, Vec<LexError<'_>>) {
  let result = lexer().parse(source);
  (result.output().cloned(), result.into_errors())
}

pub(crate) fn lexer<'src>()
-> impl Parser<'src, &'src str, Vec<Spanned<Token>>, extra::Err<LexError<'src>>>
{
  let whitespace = one_of(" \t\r\n").repeated().at_least(1).ignored();

  let comment = just('#')
    .then(any().filter(|c| *c != '\n').repeated())
    .ignored();

  let padding = whitespace.or(comment).repeated();

  token_parser()
    .map_with(|token, extra| (token, extra.span()))
    .padded_by(padding)
    .repeated()
    .collect()
    .then_ignore(end())
}

fn token_parser<'src>()
-> impl Parser<'src, &'src str, Token, extra::Err<LexError<'src>>> {
  let identifier =
    text::ascii::ident().map(|identifier: &str| match identifier {
      "function" => Token::Function,
      "BEGIN" => Token::Begin,
      "END" => Token::End,
      "if" => Token::If,
      "else" => Token::Else,
      "for" => Token::For,
      "while" => Token::While,
      "do" => Token::Do,
      "switch" => Token::Switch,
      "case" => Token::Case,
      "default" => Token::Default,
      "print" => Token::Print,
      "printf" => Token::Printf,
      "delete" => Token::Delete,
      "break" => Token::Break,
      "continue" => Token::Continue,
      "next" => Token::Next,
      "return" => Token::Return,
      "getline" => Token::Getline,
      "in" => Token::In,
      _ => Token::Identifier(identifier.to_string()),
    });

  let integer =
    text::int(10).map(|integer: &str| Token::Integer(integer.to_string()));

  let single_quoted = just('\'')
    .ignore_then(any().filter(|c| *c != '\'').repeated().to_slice())
    .then_ignore(just('\''))
    .map(|s: &str| Token::String(s.to_string()));

  let double_quoted = just('"')
    .ignore_then(any().filter(|c| *c != '"').repeated().to_slice())
    .then_ignore(just('"'))
    .map(|s: &str| Token::String(s.to_string()));

  let multi_char_operators = choice((
    just("+=").to(Token::PlusAssign),
    just("-=").to(Token::MinusAssign),
    just("*=").to(Token::StarAssign),
    just("/=").to(Token::SlashAssign),
    just("%=").to(Token::PercentAssign),
    just("^=").to(Token::CaretAssign),
    just("||").to(Token::OrOr),
    just("&&").to(Token::AndAnd),
    just("!~").to(Token::BangTilde),
    just("<=").to(Token::LessEqual),
    just("==").to(Token::EqualEqual),
    just("!=").to(Token::BangEqual),
    just(">=").to(Token::GreaterEqual),
    just("++").to(Token::PlusPlus),
    just("--").to(Token::MinusMinus),
    just(">>").to(Token::GreaterGreater),
  ));

  let single_char_operators = choice((
    just('+').to(Token::Plus),
    just('-').to(Token::Minus),
    just('*').to(Token::Star),
    just('/').to(Token::Slash),
    just('%').to(Token::Percent),
    just('^').to(Token::Caret),
    just('=').to(Token::Assign),
    just('?').to(Token::Question),
    just(':').to(Token::Colon),
    just('~').to(Token::Tilde),
    just('<').to(Token::Less),
    just('>').to(Token::Greater),
    just('$').to(Token::Dollar),
    just('|').to(Token::Pipe),
  ));

  let operators = choice((multi_char_operators, single_char_operators));

  let punctuation = choice((
    just('{').to(Token::LBrace),
    just('}').to(Token::RBrace),
    just('[').to(Token::LBracket),
    just(']').to(Token::RBracket),
    just('(').to(Token::LParen),
    just(')').to(Token::RParen),
    just(',').to(Token::Comma),
    just(';').to(Token::Semicolon),
  ));

  choice((
    identifier,
    integer,
    single_quoted,
    double_quoted,
    operators,
    punctuation,
  ))
}

#[cfg(test)]
mod tests {
  use super::*;

  struct Test<'src> {
    expected: Vec<(Token, Range<usize>)>,
    input: &'src str,
  }

  impl<'src> Test<'src> {
    fn expected(
      self,
      expected: impl IntoIterator<Item = (Token, Range<usize>)>,
    ) -> Self {
      Self {
        expected: expected.into_iter().collect(),
        ..self
      }
    }

    fn input(self, input: &'src str) -> Self {
      Self { input, ..self }
    }

    fn new() -> Self {
      Self {
        expected: Vec::new(),
        input: "",
      }
    }

    fn run(self) {
      let (tokens, errors) = super::lex(self.input);

      assert!(errors.is_empty(), "{errors:?}");

      let actual = tokens
        .unwrap()
        .into_iter()
        .map(|(token, span)| (token, span.start..span.end))
        .collect::<Vec<_>>();

      assert_eq!(actual, self.expected);
    }
  }

  #[test]
  fn identifiers_and_keywords() {
    Test::new()
      .input(
        "function BEGIN END if else for while do switch case default print printf delete break continue next return getline in foo _bar baz123",
      )
      .expected([
        (Token::Function, 0..8),
        (Token::Begin, 9..14),
        (Token::End, 15..18),
        (Token::If, 19..21),
        (Token::Else, 22..26),
        (Token::For, 27..30),
        (Token::While, 31..36),
        (Token::Do, 37..39),
        (Token::Switch, 40..46),
        (Token::Case, 47..51),
        (Token::Default, 52..59),
        (Token::Print, 60..65),
        (Token::Printf, 66..72),
        (Token::Delete, 73..79),
        (Token::Break, 80..85),
        (Token::Continue, 86..94),
        (Token::Next, 95..99),
        (Token::Return, 100..106),
        (Token::Getline, 107..114),
        (Token::In, 115..117),
        (Token::Identifier("foo".to_string()), 118..121),
        (Token::Identifier("_bar".to_string()), 122..126),
        (Token::Identifier("baz123".to_string()), 127..133),
      ])
      .run();
  }

  #[test]
  fn integers_strings_operators_and_punctuation() {
    Test::new()
      .input(
        "123 'foo' \"bar\" + - * / % ^ = += -= *= /= %= ^= ? : || && ~ !~ < <= == != > >= ++ -- $ [ ] >> | { } ( ) , ;",
      )
      .expected([
        (Token::Integer("123".to_string()), 0..3),
        (Token::String("foo".to_string()), 4..9),
        (Token::String("bar".to_string()), 10..15),
        (Token::Plus, 16..17),
        (Token::Minus, 18..19),
        (Token::Star, 20..21),
        (Token::Slash, 22..23),
        (Token::Percent, 24..25),
        (Token::Caret, 26..27),
        (Token::Assign, 28..29),
        (Token::PlusAssign, 30..32),
        (Token::MinusAssign, 33..35),
        (Token::StarAssign, 36..38),
        (Token::SlashAssign, 39..41),
        (Token::PercentAssign, 42..44),
        (Token::CaretAssign, 45..47),
        (Token::Question, 48..49),
        (Token::Colon, 50..51),
        (Token::OrOr, 52..54),
        (Token::AndAnd, 55..57),
        (Token::Tilde, 58..59),
        (Token::BangTilde, 60..62),
        (Token::Less, 63..64),
        (Token::LessEqual, 65..67),
        (Token::EqualEqual, 68..70),
        (Token::BangEqual, 71..73),
        (Token::Greater, 74..75),
        (Token::GreaterEqual, 76..78),
        (Token::PlusPlus, 79..81),
        (Token::MinusMinus, 82..84),
        (Token::Dollar, 85..86),
        (Token::LBracket, 87..88),
        (Token::RBracket, 89..90),
        (Token::GreaterGreater, 91..93),
        (Token::Pipe, 94..95),
        (Token::LBrace, 96..97),
        (Token::RBrace, 98..99),
        (Token::LParen, 100..101),
        (Token::RParen, 102..103),
        (Token::Comma, 104..105),
        (Token::Semicolon, 106..107),
      ])
      .run();
  }

  #[test]
  fn invalid_input_reports_errors() {
    let (tokens, errors) = super::lex("@");

    assert_eq!(tokens, None);

    let actual = errors
      .into_iter()
      .map(|error| error.to_string())
      .collect::<Vec<_>>();

    assert_eq!(
      actual,
      vec![
        "found '@' expected ' ', '\t', '\r', '\n', '#', identifier, non-zero digit, '0', ''', '\"', '+', '-', '*', '/', '%', '^', '|', '&', '!', '<', '=', '>', '?', ':', '~', '$', '{', '}', '[', ']', '(', ')', ',', ';', or end of input".to_string(),
      ],
    );
  }

  #[test]
  fn lexes_adjacent_tokens_without_whitespace() {
    Test::new()
      .input("foo{bar}(1);")
      .expected([
        (Token::Identifier("foo".to_string()), 0..3),
        (Token::LBrace, 3..4),
        (Token::Identifier("bar".to_string()), 4..7),
        (Token::RBrace, 7..8),
        (Token::LParen, 8..9),
        (Token::Integer("1".to_string()), 9..10),
        (Token::RParen, 10..11),
        (Token::Semicolon, 11..12),
      ])
      .run();
  }

  #[test]
  fn trailing_comment_without_newline_is_ignored() {
    Test::new()
      .input("foo # bar")
      .expected([(Token::Identifier("foo".to_string()), 0..3)])
      .run();
  }

  #[test]
  fn whitespace_and_comments_are_ignored() {
    Test::new()
      .input("foo # bar\n  # baz\n123\t# qux\n\"bar\" # bob")
      .expected([
        (Token::Identifier("foo".to_string()), 0..3),
        (Token::Integer("123".to_string()), 18..21),
        (Token::String("bar".to_string()), 28..33),
      ])
      .run();
  }
}
