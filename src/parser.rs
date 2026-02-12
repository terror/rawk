use super::*;

pub(crate) type ParseError<'src> = Rich<'src, Token, Span>;

pub(crate) fn parse(
  tokens: Option<Vec<Spanned<Token>>>,
  source_len: usize,
) -> (Option<Program>, Vec<ParseError<'static>>) {
  let Some(tokens) = tokens else {
    return (None, Vec::new());
  };

  let result = parser().parse(
    Stream::from_iter(tokens)
      .map(Span::new((), source_len..source_len), |(token, span)| {
        (token, span)
      }),
  );

  (
    result.output().cloned(),
    result
      .into_errors()
      .into_iter()
      .map(Rich::into_owned)
      .collect(),
  )
}

fn parser<'src, I>()
-> impl Parser<'src, I, Program, extra::Err<ParseError<'src>>>
where
  I: ValueInput<'src, Token = Token, Span = Span>,
{
  let identifier = select! { Token::Identifier(identifier) => identifier };

  let pattern = choice((
    just(Token::Begin).to(Pattern::Begin),
    just(Token::End).to(Pattern::End),
    identifier.to(Pattern::ExpressionStub),
  ));

  let block_atom = choice((
    just(Token::Function).ignored(),
    just(Token::Begin).ignored(),
    just(Token::End).ignored(),
    just(Token::LParen).ignored(),
    just(Token::RParen).ignored(),
    just(Token::Comma).ignored(),
    just(Token::Semicolon).ignored(),
    select! { Token::Identifier(_) => () },
    select! { Token::Integer(_) => () },
    select! { Token::String(_) => () },
  ))
  .to(BlockItem::TokenStub);

  let block = recursive(|block| {
    let item = block.clone().map(BlockItem::Block).or(block_atom.clone());

    item
      .repeated()
      .collect::<Vec<_>>()
      .delimited_by(just(Token::LBrace), just(Token::RBrace))
      .map(|items| Block { items })
  });

  let parameter_list = identifier
    .separated_by(just(Token::Comma))
    .allow_trailing()
    .collect::<Vec<_>>();

  let function_definition = just(Token::Function)
    .ignore_then(identifier)
    .then(
      parameter_list
        .or_not()
        .delimited_by(just(Token::LParen), just(Token::RParen)),
    )
    .then(block.clone())
    .map(|((name, parameters), body)| {
      TopLevelItem::Function(FunctionDefinition {
        name,
        parameters: parameters.unwrap_or_default(),
        body,
      })
    });

  let pattern_action =
    pattern
      .or_not()
      .then(block.clone())
      .map(|(pattern, action)| {
        TopLevelItem::PatternAction(PatternAction { action, pattern })
      });

  function_definition
    .or(pattern_action)
    .repeated()
    .collect::<Vec<_>>()
    .then_ignore(end())
    .map(|items| Program { items })
}
