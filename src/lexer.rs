use super::*;

pub(crate) type Span = SimpleSpan<usize>;
pub(crate) type Spanned<T> = (T, Span);
pub(crate) type LexError<'src> = Rich<'src, char, Span>;

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

pub(crate) fn lex(
  source: &str,
) -> (Option<Vec<Spanned<Token>>>, Vec<LexError<'_>>) {
  let result = lexer().parse(source);
  (result.output().cloned(), result.into_errors())
}
