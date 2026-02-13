use super::*;

pub(crate) type Span = SimpleSpan<usize>;
pub(crate) type Spanned<T> = (T, Span);
pub(crate) type LexError<'src> = Rich<'src, char, Span>;

fn identifier_parser<'src>()
-> impl Parser<'src, &'src str, Token, extra::Err<LexError<'src>>> {
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
  })
}

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

fn number_parser<'src>()
-> impl Parser<'src, &'src str, Token, extra::Err<LexError<'src>>> {
  let digits = one_of("0123456789").repeated().at_least(1);

  let exponent = one_of("eE").then(one_of("+-").or_not()).then(digits);

  let hexadecimal = choice((just("0x"), just("0X")))
    .then(one_of("0123456789abcdefABCDEF").repeated().at_least(1))
    .to_slice();

  let float_with_leading_digits = digits
    .then(just('.'))
    .then(one_of("0123456789").repeated())
    .then(exponent.or_not())
    .to_slice();

  let float_without_leading_digits =
    just('.').then(digits).then(exponent.or_not()).to_slice();

  let scientific = digits.then(exponent).to_slice();
  let integer = text::int(10);

  choice((
    hexadecimal,
    float_with_leading_digits,
    float_without_leading_digits,
    scientific,
    integer,
  ))
  .map(|number: &str| Token::Integer(number.to_string()))
}

fn operator_parser<'src>()
-> impl Parser<'src, &'src str, Token, extra::Err<LexError<'src>>> {
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

  choice((multi_char_operators, single_char_operators))
}

fn punctuation_parser<'src>()
-> impl Parser<'src, &'src str, Token, extra::Err<LexError<'src>>> {
  choice((
    just('{').to(Token::LBrace),
    just('}').to(Token::RBrace),
    just('[').to(Token::LBracket),
    just(']').to(Token::RBracket),
    just('(').to(Token::LParen),
    just(')').to(Token::RParen),
    just(',').to(Token::Comma),
    just(';').to(Token::Semicolon),
  ))
}

fn string_parser<'src>()
-> impl Parser<'src, &'src str, Token, extra::Err<LexError<'src>>> {
  let single_quoted = just('\'')
    .ignore_then(any().filter(|c| *c != '\'').repeated().to_slice())
    .then_ignore(just('\''))
    .map(|s: &str| Token::String(s.to_string()));

  let double_quoted = just('"')
    .ignore_then(any().filter(|c| *c != '"').repeated().to_slice())
    .then_ignore(just('"'))
    .map(|s: &str| Token::String(s.to_string()));

  choice((single_quoted, double_quoted))
}

fn token_parser<'src>()
-> impl Parser<'src, &'src str, Token, extra::Err<LexError<'src>>> {
  choice((
    identifier_parser(),
    number_parser(),
    operator_parser(),
    punctuation_parser(),
    string_parser(),
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
        "found '@' expected ' ', '\t', '\r', '\n', '#', identifier, '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', non-zero digit, '+', '-', '*', '/', '%', '^', '|', '&', '!', '<', '=', '>', '?', ':', '~', '$', '{', '}', '[', ']', '(', ')', ',', ';', ''', '\"', or end of input".to_string(),
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
  fn numbers_strings_operators_and_punctuation() {
    Test::new()
      .input(
        "123 1.5 .5 1. 1e3 1E-2 0x10 0X1F 'foo' \"bar\" + - * / % ^ = += -= *= /= %= ^= ? : || && ~ !~ < <= == != > >= ++ -- $ [ ] >> | { } ( ) , ;",
      )
      .expected([
        (Token::Integer("123".to_string()), 0..3),
        (Token::Integer("1.5".to_string()), 4..7),
        (Token::Integer(".5".to_string()), 8..10),
        (Token::Integer("1.".to_string()), 11..13),
        (Token::Integer("1e3".to_string()), 14..17),
        (Token::Integer("1E-2".to_string()), 18..22),
        (Token::Integer("0x10".to_string()), 23..27),
        (Token::Integer("0X1F".to_string()), 28..32),
        (Token::String("foo".to_string()), 33..38),
        (Token::String("bar".to_string()), 39..44),
        (Token::Plus, 45..46),
        (Token::Minus, 47..48),
        (Token::Star, 49..50),
        (Token::Slash, 51..52),
        (Token::Percent, 53..54),
        (Token::Caret, 55..56),
        (Token::Assign, 57..58),
        (Token::PlusAssign, 59..61),
        (Token::MinusAssign, 62..64),
        (Token::StarAssign, 65..67),
        (Token::SlashAssign, 68..70),
        (Token::PercentAssign, 71..73),
        (Token::CaretAssign, 74..76),
        (Token::Question, 77..78),
        (Token::Colon, 79..80),
        (Token::OrOr, 81..83),
        (Token::AndAnd, 84..86),
        (Token::Tilde, 87..88),
        (Token::BangTilde, 89..91),
        (Token::Less, 92..93),
        (Token::LessEqual, 94..96),
        (Token::EqualEqual, 97..99),
        (Token::BangEqual, 100..102),
        (Token::Greater, 103..104),
        (Token::GreaterEqual, 105..107),
        (Token::PlusPlus, 108..110),
        (Token::MinusMinus, 111..113),
        (Token::Dollar, 114..115),
        (Token::LBracket, 116..117),
        (Token::RBracket, 118..119),
        (Token::GreaterGreater, 120..122),
        (Token::Pipe, 123..124),
        (Token::LBrace, 125..126),
        (Token::RBrace, 127..128),
        (Token::LParen, 129..130),
        (Token::RParen, 131..132),
        (Token::Comma, 133..134),
        (Token::Semicolon, 135..136),
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
