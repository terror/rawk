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

#[cfg(test)]
mod tests {
  use super::*;

  struct Test<'src> {
    expected: Program,
    input: &'src str,
  }

  impl<'src> Test<'src> {
    fn expected(self, expected: Program) -> Self {
      Self { expected, ..self }
    }

    fn input(self, input: &'src str) -> Self {
      Self { input, ..self }
    }

    fn new() -> Self {
      Self {
        expected: Program { items: Vec::new() },
        input: "",
      }
    }

    fn run(self) {
      let (tokens, lex_errors) = lexer::lex(self.input);

      assert!(lex_errors.is_empty(), "{lex_errors:?}");

      let (program, parse_errors) = super::parse(tokens, self.input.len());

      assert!(parse_errors.is_empty(), "{parse_errors:?}");
      assert_eq!(program.unwrap(), self.expected);
    }
  }

  #[test]
  fn invalid_input_reports_errors() {
    let (tokens, lex_errors) = lexer::lex("function foo(bar { baz }");

    assert!(lex_errors.is_empty(), "{lex_errors:?}");

    let (program, parse_errors) =
      super::parse(tokens, "function foo(bar { baz }".len());

    assert_eq!(program, None);

    let actual = parse_errors
      .into_iter()
      .map(|error| error.to_string())
      .collect::<Vec<_>>();

    assert_eq!(actual, vec!["found '{' expected ',', or ')'".to_string()]);
  }

  #[test]
  fn no_tokens_returns_no_errors() {
    let (program, errors) = super::parse(None, 0);

    assert_eq!(program, None);
    assert_eq!(errors.len(), 0);
  }

  #[test]
  fn parses_empty_program() {
    Test::new()
      .input("")
      .expected(Program { items: Vec::new() })
      .run();
  }

  #[test]
  fn parses_function_definitions() {
    Test::new()
      .input("function foo() { bar } function baz(qux, bob,) { foo }")
      .expected(Program {
        items: vec![
          TopLevelItem::Function(FunctionDefinition {
            body: Block {
              items: vec![BlockItem::TokenStub],
            },
            name: "foo".to_string(),
            parameters: Vec::new(),
          }),
          TopLevelItem::Function(FunctionDefinition {
            body: Block {
              items: vec![BlockItem::TokenStub],
            },
            name: "baz".to_string(),
            parameters: vec!["qux".to_string(), "bob".to_string()],
          }),
        ],
      })
      .run();
  }

  #[test]
  fn parses_nested_blocks() {
    Test::new()
      .input("{ foo { bar { baz } } qux }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![
              BlockItem::TokenStub,
              BlockItem::Block(Block {
                items: vec![
                  BlockItem::TokenStub,
                  BlockItem::Block(Block {
                    items: vec![BlockItem::TokenStub],
                  }),
                ],
              }),
              BlockItem::TokenStub,
            ],
          },
          pattern: None,
        })],
      })
      .run();
  }

  #[test]
  fn parses_pattern_actions() {
    Test::new()
      .input("BEGIN { foo } END { bar } baz { qux } { bob }")
      .expected(Program {
        items: vec![
          TopLevelItem::PatternAction(PatternAction {
            action: Block {
              items: vec![BlockItem::TokenStub],
            },
            pattern: Some(Pattern::Begin),
          }),
          TopLevelItem::PatternAction(PatternAction {
            action: Block {
              items: vec![BlockItem::TokenStub],
            },
            pattern: Some(Pattern::End),
          }),
          TopLevelItem::PatternAction(PatternAction {
            action: Block {
              items: vec![BlockItem::TokenStub],
            },
            pattern: Some(Pattern::ExpressionStub),
          }),
          TopLevelItem::PatternAction(PatternAction {
            action: Block {
              items: vec![BlockItem::TokenStub],
            },
            pattern: None,
          }),
        ],
      })
      .run();
  }
}
