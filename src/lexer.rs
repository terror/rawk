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

  let punctuation = choice((
    just('{').to(Token::LBrace),
    just('}').to(Token::RBrace),
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
      .input("function BEGIN END foo _bar baz123")
      .expected([
        (Token::Function, 0..8),
        (Token::Begin, 9..14),
        (Token::End, 15..18),
        (Token::Identifier("foo".to_string()), 19..22),
        (Token::Identifier("_bar".to_string()), 23..27),
        (Token::Identifier("baz123".to_string()), 28..34),
      ])
      .run();
  }

  #[test]
  fn integers_strings_and_punctuation() {
    Test::new()
      .input("123 'foo' \"bar\" { } ( ) , ;")
      .expected([
        (Token::Integer("123".to_string()), 0..3),
        (Token::String("foo".to_string()), 4..9),
        (Token::String("bar".to_string()), 10..15),
        (Token::LBrace, 16..17),
        (Token::RBrace, 18..19),
        (Token::LParen, 20..21),
        (Token::RParen, 22..23),
        (Token::Comma, 24..25),
        (Token::Semicolon, 26..27),
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
        "found '@' expected ' ', '\t', '\r', '\n', '#', identifier, non-zero digit, '0', ''', '\"', '{', '}', '(', ')', ',', ';', or end of input".to_string(),
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
