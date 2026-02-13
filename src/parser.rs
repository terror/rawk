use super::*;

pub(crate) type ParseError<'src> = Rich<'src, Token, Span>;

fn atom_parser<'src, I>(
  expr: impl Parser<'src, I, Expression, extra::Err<ParseError<'src>>>
  + Clone
  + 'src,
) -> impl Parser<'src, I, Expression, extra::Err<ParseError<'src>>> + Clone
where
  I: ValueInput<'src, Token = Token, Span = Span>,
{
  let identifier = select! { Token::Identifier(identifier) => identifier };

  let number =
    select! { Token::Number(number) => Expression::Number(number.lexeme) };

  let string = select! { Token::String(string) => Expression::String(string) };

  let regex = select! { Token::Regex(regex) => Expression::Regex(regex) };

  let function_call = identifier
    .then(
      expr
        .clone()
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LParen), just(Token::RParen)),
    )
    .map(|(name, arguments)| Expression::FunctionCall { arguments, name });

  let array_subscript = identifier
    .then(
      expr
        .clone()
        .separated_by(just(Token::Comma))
        .at_least(1)
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LBracket), just(Token::RBracket)),
    )
    .map(|(name, indices)| Expression::Index { indices, name });

  let variable = array_subscript
    .clone()
    .or(identifier.map(Expression::Identifier));

  let getline = choice((
    select! { Token::String(string) => string }
      .then_ignore(just(Token::Pipe))
      .then_ignore(just(Token::Getline))
      .map(Some),
    just(Token::Getline).to(None),
  ))
  .then(variable.or_not())
  .then(
    just(Token::Less)
      .ignore_then(select! { Token::String(string) => string })
      .or_not(),
  )
  .map(|((command, target), input)| Expression::Getline {
    command,
    input,
    target: target.map(Box::new),
  });

  let grouped = expr.delimited_by(just(Token::LParen), just(Token::RParen));

  let ident_expr = identifier.map(Expression::Identifier);

  choice((
    number,
    getline,
    string,
    regex,
    function_call,
    array_subscript,
    grouped,
    ident_expr,
  ))
}

fn block_parser<'src, I>(
  expr: impl Parser<'src, I, Expression, extra::Err<ParseError<'src>>>
  + Clone
  + 'src,
  expr_list: impl Parser<'src, I, Vec<Expression>, extra::Err<ParseError<'src>>>
  + Clone
  + 'src,
  output_redirection: impl Parser<
    'src,
    I,
    OutputRedirection,
    extra::Err<ParseError<'src>>,
  > + Clone
  + 'src,
) -> impl Parser<'src, I, Block, extra::Err<ParseError<'src>>> + Clone
where
  I: ValueInput<'src, Token = Token, Span = Span>,
{
  recursive(|block| {
    let statement = recursive(|statement| {
      statement_parser(
        expr.clone(),
        expr_list.clone(),
        output_redirection.clone(),
        block.clone(),
        statement,
      )
    });

    block
      .clone()
      .map(BlockItem::Block)
      .or(statement)
      .repeated()
      .collect::<Vec<_>>()
      .delimited_by(just(Token::LBrace), just(Token::RBrace))
      .map(|items| Block { items })
  })
}

fn expr_list_parser<'src, I>(
  expr: impl Parser<'src, I, Expression, extra::Err<ParseError<'src>>>
  + Clone
  + 'src,
) -> impl Parser<'src, I, Vec<Expression>, extra::Err<ParseError<'src>>> + Clone
where
  I: ValueInput<'src, Token = Token, Span = Span>,
{
  expr
    .separated_by(just(Token::Comma))
    .allow_trailing()
    .at_least(1)
    .collect::<Vec<_>>()
}

fn expression_parser<'src, I>()
-> impl Parser<'src, I, Expression, extra::Err<ParseError<'src>>> + Clone
where
  I: ValueInput<'src, Token = Token, Span = Span>,
{
  recursive(|expr| {
    let atom = atom_parser(expr.clone()).boxed();

    let additive_expr = parser_additive_pratt(atom).boxed();

    let concat_start = choice((
      select! { Token::Identifier(_) => () },
      select! { Token::Number(_) => () },
      select! { Token::String(_) => () },
      select! { Token::Regex(_) => () },
      just(Token::LParen).ignored(),
      just(Token::Getline).ignored(),
      just(Token::Dollar).ignored(),
      just(Token::PlusPlus).ignored(),
      just(Token::MinusMinus).ignored(),
    ));

    let concat_expr = additive_expr
      .clone()
      .foldl(
        additive_expr.and_is(concat_start).repeated(),
        |left, right| Expression::Binary {
          left: Box::new(left),
          operator: BinaryOp::Concat,
          right: Box::new(right),
        },
      )
      .boxed();

    let pratt_expr = parser_logical_pratt(concat_expr).boxed();

    let ternary_expr = pratt_expr.clone().foldl(
      just(Token::Question)
        .ignore_then(expr.clone())
        .then_ignore(just(Token::Colon))
        .then(expr.clone())
        .repeated(),
      |condition, (then_branch, else_branch)| Expression::Ternary {
        condition: Box::new(condition),
        else_branch: Box::new(else_branch),
        then_branch: Box::new(then_branch),
      },
    );

    let assign_op = choice((
      just(Token::Assign).to(AssignOp::Assign),
      just(Token::PlusAssign).to(AssignOp::Add),
      just(Token::MinusAssign).to(AssignOp::Subtract),
      just(Token::StarAssign).to(AssignOp::Multiply),
      just(Token::SlashAssign).to(AssignOp::Divide),
      just(Token::PercentAssign).to(AssignOp::Modulo),
      just(Token::CaretAssign).to(AssignOp::Power),
    ));

    ternary_expr.then(assign_op.then(expr).or_not()).map(
      |(target, assignment)| match assignment {
        Some((operator, value)) => Expression::Assignment {
          operator,
          target: Box::new(target),
          value: Box::new(value),
        },
        None => target,
      },
    )
  })
}

fn if_statement_parser<'src, I>(
  expr: impl Parser<'src, I, Expression, extra::Err<ParseError<'src>>>
  + Clone
  + 'src,
  block: impl Parser<'src, I, Block, extra::Err<ParseError<'src>>> + Clone + 'src,
  statement: impl Parser<'src, I, BlockItem, extra::Err<ParseError<'src>>>
  + Clone
  + 'src,
) -> impl Parser<'src, I, BlockItem, extra::Err<ParseError<'src>>> + Clone
where
  I: ValueInput<'src, Token = Token, Span = Span>,
{
  just(Token::If)
    .ignore_then(
      expr
        .clone()
        .delimited_by(just(Token::LParen), just(Token::RParen)),
    )
    .then(block.clone())
    .then(
      just(Token::Else)
        .ignore_then(block.clone().or(statement.clone().try_map(
          |statement, span| match statement {
            if_statement @ BlockItem::If { .. } => Ok(Block {
              items: vec![if_statement],
            }),
            _ => Err(Rich::custom(span, "expected if statement")),
          },
        )))
        .or_not(),
    )
    .map(|((condition, then_branch), else_branch)| BlockItem::If {
      condition,
      else_branch,
      then_branch,
    })
}

fn output_redirection_parser<'src, I>(
  string: impl Parser<'src, I, String, extra::Err<ParseError<'src>>> + Clone + 'src,
) -> impl Parser<'src, I, OutputRedirection, extra::Err<ParseError<'src>>> + Clone
where
  I: ValueInput<'src, Token = Token, Span = Span>,
{
  choice((
    just(Token::GreaterGreater)
      .ignore_then(string.clone())
      .map(OutputRedirection::Append),
    just(Token::Greater)
      .ignore_then(string.clone())
      .map(OutputRedirection::Write),
    just(Token::Pipe)
      .ignore_then(string)
      .map(OutputRedirection::Pipe),
  ))
}

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

  let expr = expression_parser();

  let expr_list = expr_list_parser(expr.clone());

  let output_redirection =
    output_redirection_parser(select! { Token::String(string) => string });

  let block = block_parser(expr.clone(), expr_list, output_redirection);

  let function_definition = just(Token::Function)
    .ignore_then(identifier)
    .then(
      identifier
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
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

  let expression_range = expr
    .clone()
    .then_ignore(just(Token::Comma))
    .then(expr.clone())
    .map(|(start, end)| Pattern::Range { end, start });

  let pattern = choice((
    just(Token::Begin).to(Pattern::Begin),
    just(Token::End).to(Pattern::End),
    expression_range,
    expr.map(Pattern::Expression),
  ));

  function_definition
    .or(pattern.or_not().then(block).map(|(pattern, action)| {
      TopLevelItem::PatternAction(PatternAction { action, pattern })
    }))
    .repeated()
    .collect::<Vec<_>>()
    .then_ignore(end())
    .map(|items| Program { items })
}

fn parser_additive_pratt<'src, I>(
  atom: impl Parser<'src, I, Expression, extra::Err<ParseError<'src>>>
  + Clone
  + 'src,
) -> impl Parser<'src, I, Expression, extra::Err<ParseError<'src>>> + Clone
where
  I: ValueInput<'src, Token = Token, Span = Span>,
{
  macro_rules! binary {
    ($assoc:expr, $token:expr, $op:expr) => {
      infix($assoc, just($token), |l, _, r, _| Expression::Binary {
        left: Box::new(l),
        operator: $op,
        right: Box::new(r),
      })
    };
  }

  atom.pratt((
    postfix(13, just(Token::PlusPlus), |lhs, _, _| {
      Expression::PostIncrement(Box::new(lhs))
    }),
    postfix(13, just(Token::MinusMinus), |lhs, _, _| {
      Expression::PostDecrement(Box::new(lhs))
    }),
    prefix(12, just(Token::PlusPlus), |_, rhs, _| {
      Expression::PreIncrement(Box::new(rhs))
    }),
    prefix(12, just(Token::MinusMinus), |_, rhs, _| {
      Expression::PreDecrement(Box::new(rhs))
    }),
    prefix(11, just(Token::Dollar), |_, rhs, _| Expression::Unary {
      operand: Box::new(rhs),
      operator: UnaryOp::FieldAccess,
    }),
    prefix(10, just(Token::Bang), |_, rhs, _| Expression::Unary {
      operand: Box::new(rhs),
      operator: UnaryOp::Not,
    }),
    prefix(10, just(Token::Tilde), |_, rhs, _| Expression::Unary {
      operand: Box::new(rhs),
      operator: UnaryOp::BitwiseNot,
    }),
    prefix(10, just(Token::Minus), |_, rhs, _| Expression::Unary {
      operand: Box::new(rhs),
      operator: UnaryOp::Negate,
    }),
    prefix(10, just(Token::Plus), |_, rhs, _| Expression::Unary {
      operand: Box::new(rhs),
      operator: UnaryOp::Positive,
    }),
    binary!(right(9), Token::Caret, BinaryOp::Power),
    binary!(left(8), Token::Star, BinaryOp::Multiply),
    binary!(left(8), Token::Slash, BinaryOp::Divide),
    binary!(left(8), Token::Percent, BinaryOp::Modulo),
    binary!(left(7), Token::Plus, BinaryOp::Add),
    binary!(left(7), Token::Minus, BinaryOp::Subtract),
  ))
}

fn parser_logical_pratt<'src, I>(
  concat_expr: impl Parser<'src, I, Expression, extra::Err<ParseError<'src>>>
  + Clone
  + 'src,
) -> impl Parser<'src, I, Expression, extra::Err<ParseError<'src>>> + Clone
where
  I: ValueInput<'src, Token = Token, Span = Span>,
{
  macro_rules! binary {
    ($assoc:expr, $token:expr, $op:expr) => {
      infix($assoc, just($token), |l, _, r, _| Expression::Binary {
        left: Box::new(l),
        operator: $op,
        right: Box::new(r),
      })
    };
  }

  concat_expr.pratt((
    binary!(left(5), Token::Less, BinaryOp::Less),
    binary!(left(5), Token::LessEqual, BinaryOp::LessEqual),
    binary!(left(5), Token::EqualEqual, BinaryOp::Equal),
    binary!(left(5), Token::BangEqual, BinaryOp::NotEqual),
    binary!(left(5), Token::GreaterEqual, BinaryOp::GreaterEqual),
    binary!(left(5), Token::Greater, BinaryOp::Greater),
    binary!(left(4), Token::Tilde, BinaryOp::Match),
    binary!(left(4), Token::BangTilde, BinaryOp::NotMatch),
    infix(left(3), just(Token::In), |l, _, r, _| Expression::Binary {
      left: Box::new(l),
      operator: BinaryOp::In,
      right: Box::new(r),
    }),
    infix(left(2), just(Token::AndAnd), |l, _, r, _| {
      Expression::Binary {
        left: Box::new(l),
        operator: BinaryOp::And,
        right: Box::new(r),
      }
    }),
    infix(left(1), just(Token::OrOr), |l, _, r, _| {
      Expression::Binary {
        left: Box::new(l),
        operator: BinaryOp::Or,
        right: Box::new(r),
      }
    }),
  ))
}

fn statement_parser<'src, I>(
  expr: impl Parser<'src, I, Expression, extra::Err<ParseError<'src>>>
  + Clone
  + 'src,
  expr_list: impl Parser<'src, I, Vec<Expression>, extra::Err<ParseError<'src>>>
  + Clone
  + 'src,
  output_redirection: impl Parser<
    'src,
    I,
    OutputRedirection,
    extra::Err<ParseError<'src>>,
  > + Clone
  + 'src,
  block: impl Parser<'src, I, Block, extra::Err<ParseError<'src>>> + Clone + 'src,
  statement: impl Parser<'src, I, BlockItem, extra::Err<ParseError<'src>>>
  + Clone
  + 'src,
) -> impl Parser<'src, I, BlockItem, extra::Err<ParseError<'src>>> + Clone
where
  I: ValueInput<'src, Token = Token, Span = Span>,
{
  let print_item = just(Token::Print)
    .ignore_then(expr_list.clone())
    .then(output_redirection.clone().or_not())
    .map(|(arguments, redirection)| BlockItem::Print {
      arguments,
      redirection,
    });

  let printf_item = just(Token::Printf)
    .ignore_then(select! { Token::String(string) => string })
    .then_ignore(just(Token::Comma))
    .then(expr_list.clone())
    .then(output_redirection.clone().or_not())
    .map(|((format, arguments), redirection)| BlockItem::Printf {
      arguments,
      format,
      redirection,
    });

  let for_classic = expr_list
    .clone()
    .or_not()
    .then_ignore(just(Token::Semicolon))
    .then(expr_list.clone().or_not())
    .then_ignore(just(Token::Semicolon))
    .then(expr_list.clone().or_not())
    .map(|((initializer, condition), update)| (initializer, condition, update));

  let for_simple = expr_list
    .clone()
    .map(|initializer| (Some(initializer), None, None));

  let for_statement = just(Token::For)
    .ignore_then(
      choice((for_classic, for_simple, empty().to((None, None, None))))
        .delimited_by(just(Token::LParen), just(Token::RParen)),
    )
    .then(block.clone())
    .map(|((initializer, condition, update), body)| BlockItem::For {
      body,
      condition,
      initializer,
      update,
    });

  let do_while_statement = just(Token::Do)
    .ignore_then(block.clone())
    .then_ignore(just(Token::While))
    .then(
      expr
        .clone()
        .delimited_by(just(Token::LParen), just(Token::RParen)),
    )
    .map(|(body, condition)| BlockItem::DoWhile { body, condition });

  let while_statement = just(Token::While)
    .ignore_then(
      expr
        .clone()
        .delimited_by(just(Token::LParen), just(Token::RParen)),
    )
    .then(block.clone())
    .map(|(condition, body)| BlockItem::While { body, condition });

  let controlflow_statement = choice((
    just(Token::Break).to(BlockItem::Break),
    just(Token::Continue).to(BlockItem::Continue),
    just(Token::Next).to(BlockItem::Next),
    just(Token::Return)
      .ignore_then(expr.clone())
      .map(BlockItem::Return),
  ));

  choice((
    print_item,
    printf_item,
    for_statement,
    do_while_statement,
    while_statement,
    if_statement_parser(expr.clone(), block.clone(), statement.clone()),
    switch_statement_parser(expr.clone(), statement),
    controlflow_statement,
    just(Token::Delete)
      .ignore_then(expr.clone())
      .map(BlockItem::Delete),
    expr.map(BlockItem::Expression),
  ))
  .then_ignore(just(Token::Semicolon).repeated())
}

fn switch_statement_parser<'src, I>(
  expr: impl Parser<'src, I, Expression, extra::Err<ParseError<'src>>>
  + Clone
  + 'src,
  statement: impl Parser<'src, I, BlockItem, extra::Err<ParseError<'src>>>
  + Clone
  + 'src,
) -> impl Parser<'src, I, BlockItem, extra::Err<ParseError<'src>>> + Clone
where
  I: ValueInput<'src, Token = Token, Span = Span>,
{
  let switch_label = choice((
    just(Token::Case)
      .ignore_then(expr.clone())
      .map(SwitchLabel::Case),
    just(Token::Default).to(SwitchLabel::Default),
  ));

  let switch_case = switch_label
    .then_ignore(just(Token::Colon))
    .then(
      statement
        .clone()
        .then_ignore(just(Token::Semicolon).repeated())
        .repeated()
        .collect::<Vec<_>>(),
    )
    .map(|(label, statements)| SwitchCase { label, statements });

  just(Token::Switch)
    .ignore_then(expr.delimited_by(just(Token::LParen), just(Token::RParen)))
    .then(
      switch_case
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LBrace), just(Token::RBrace)),
    )
    .map(|(expression, cases)| BlockItem::Switch { cases, expression })
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
  fn parses_arithmetic_expressions() {
    Test::new()
      .input("{ 1 + 2 * 3 }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![BlockItem::Expression(Expression::Binary {
              left: Box::new(Expression::Number("1".to_string())),
              operator: BinaryOp::Add,
              right: Box::new(Expression::Binary {
                left: Box::new(Expression::Number("2".to_string())),
                operator: BinaryOp::Multiply,
                right: Box::new(Expression::Number("3".to_string())),
              }),
            })],
          },
          pattern: None,
        })],
      })
      .run();
  }

  #[test]
  fn parses_array_subscript() {
    Test::new()
      .input("{ a[1] }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![BlockItem::Expression(Expression::Index {
              indices: vec![Expression::Number("1".to_string())],
              name: "a".to_string(),
            })],
          },
          pattern: None,
        })],
      })
      .run();
  }

  #[test]
  fn parses_assignment() {
    Test::new()
      .input("{ a = 1 }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![BlockItem::Expression(Expression::Assignment {
              operator: AssignOp::Assign,
              target: Box::new(Expression::Identifier("a".to_string())),
              value: Box::new(Expression::Number("1".to_string())),
            })],
          },
          pattern: None,
        })],
      })
      .run();
  }

  #[test]
  fn parses_chained_assignment() {
    Test::new()
      .input("{ a = b = 1 }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![BlockItem::Expression(Expression::Assignment {
              operator: AssignOp::Assign,
              target: Box::new(Expression::Identifier("a".to_string())),
              value: Box::new(Expression::Assignment {
                operator: AssignOp::Assign,
                target: Box::new(Expression::Identifier("b".to_string())),
                value: Box::new(Expression::Number("1".to_string())),
              }),
            })],
          },
          pattern: None,
        })],
      })
      .run();
  }

  #[test]
  fn parses_comparison_operators() {
    Test::new()
      .input("{ a < b }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![BlockItem::Expression(Expression::Binary {
              left: Box::new(Expression::Identifier("a".to_string())),
              operator: BinaryOp::Less,
              right: Box::new(Expression::Identifier("b".to_string())),
            })],
          },
          pattern: None,
        })],
      })
      .run();
  }

  #[test]
  fn parses_complex_expression() {
    Test::new()
      .input("{ $1 > 0 && $2 ~ /foo/ }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![BlockItem::Expression(Expression::Binary {
              left: Box::new(Expression::Binary {
                left: Box::new(Expression::Unary {
                  operand: Box::new(Expression::Number("1".to_string())),
                  operator: UnaryOp::FieldAccess,
                }),
                operator: BinaryOp::Greater,
                right: Box::new(Expression::Number("0".to_string())),
              }),
              operator: BinaryOp::And,
              right: Box::new(Expression::Binary {
                left: Box::new(Expression::Unary {
                  operand: Box::new(Expression::Number("2".to_string())),
                  operator: UnaryOp::FieldAccess,
                }),
                operator: BinaryOp::Match,
                right: Box::new(Expression::Regex("foo".to_string())),
              }),
            })],
          },
          pattern: None,
        })],
      })
      .run();
  }

  #[test]
  fn parses_compound_assignment() {
    Test::new()
      .input("{ a += 1 }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![BlockItem::Expression(Expression::Assignment {
              operator: AssignOp::Add,
              target: Box::new(Expression::Identifier("a".to_string())),
              value: Box::new(Expression::Number("1".to_string())),
            })],
          },
          pattern: None,
        })],
      })
      .run();
  }

  #[test]
  fn parses_concatenation_expression() {
    Test::new()
      .input("{ a b < c }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![BlockItem::Expression(Expression::Binary {
              left: Box::new(Expression::Binary {
                left: Box::new(Expression::Identifier("a".to_string())),
                operator: BinaryOp::Concat,
                right: Box::new(Expression::Identifier("b".to_string())),
              }),
              operator: BinaryOp::Less,
              right: Box::new(Expression::Identifier("c".to_string())),
            })],
          },
          pattern: None,
        })],
      })
      .run();
  }

  #[test]
  fn parses_controlflow_and_memory_statements() {
    Test::new()
      .input("{ delete a[1]; return b; continue }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![
              BlockItem::Delete(Expression::Index {
                indices: vec![Expression::Number("1".to_string())],
                name: "a".to_string(),
              }),
              BlockItem::Return(Expression::Identifier("b".to_string())),
              BlockItem::Continue,
            ],
          },
          pattern: None,
        })],
      })
      .run();
  }

  #[test]
  fn parses_empty_program() {
    Test::new()
      .input("")
      .expected(Program { items: Vec::new() })
      .run();
  }

  #[test]
  fn parses_exponentiation_right_associative() {
    Test::new()
      .input("{ 2 ^ 3 ^ 4 }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![BlockItem::Expression(Expression::Binary {
              left: Box::new(Expression::Number("2".to_string())),
              operator: BinaryOp::Power,
              right: Box::new(Expression::Binary {
                left: Box::new(Expression::Number("3".to_string())),
                operator: BinaryOp::Power,
                right: Box::new(Expression::Number("4".to_string())),
              }),
            })],
          },
          pattern: None,
        })],
      })
      .run();
  }

  #[test]
  fn parses_expression_as_pattern() {
    Test::new()
      .input("$1 > 0 { foo }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![BlockItem::Expression(Expression::Identifier(
              "foo".to_string(),
            ))],
          },
          pattern: Some(Pattern::Expression(Expression::Binary {
            left: Box::new(Expression::Unary {
              operand: Box::new(Expression::Number("1".to_string())),
              operator: UnaryOp::FieldAccess,
            }),
            operator: BinaryOp::Greater,
            right: Box::new(Expression::Number("0".to_string())),
          })),
        })],
      })
      .run();
  }

  #[test]
  fn parses_expression_range_as_pattern() {
    Test::new()
      .input("foo, bar { baz }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![BlockItem::Expression(Expression::Identifier(
              "baz".to_string(),
            ))],
          },
          pattern: Some(Pattern::Range {
            end: Expression::Identifier("bar".to_string()),
            start: Expression::Identifier("foo".to_string()),
          }),
        })],
      })
      .run();
  }

  #[test]
  fn parses_field_access() {
    Test::new()
      .input("{ $1 }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![BlockItem::Expression(Expression::Unary {
              operand: Box::new(Expression::Number("1".to_string())),
              operator: UnaryOp::FieldAccess,
            })],
          },
          pattern: None,
        })],
      })
      .run();
  }

  #[test]
  fn parses_function_call_expression() {
    Test::new()
      .input("{ foo(1, 2) }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![BlockItem::Expression(Expression::FunctionCall {
              arguments: vec![
                Expression::Number("1".to_string()),
                Expression::Number("2".to_string()),
              ],
              name: "foo".to_string(),
            })],
          },
          pattern: None,
        })],
      })
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
              items: vec![BlockItem::Expression(Expression::Identifier(
                "bar".to_string(),
              ))],
            },
            name: "foo".to_string(),
            parameters: Vec::new(),
          }),
          TopLevelItem::Function(FunctionDefinition {
            body: Block {
              items: vec![BlockItem::Expression(Expression::Identifier(
                "foo".to_string(),
              ))],
            },
            name: "baz".to_string(),
            parameters: vec!["qux".to_string(), "bob".to_string()],
          }),
        ],
      })
      .run();
  }

  #[test]
  fn parses_getline_expression() {
    Test::new()
      .input("{ getline; getline foo; getline foo[1]; \"cmd\" | getline bar < \"in\" }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![
              BlockItem::Expression(Expression::Getline {
                command: None,
                input: None,
                target: None,
              }),
              BlockItem::Expression(Expression::Getline {
                command: None,
                input: None,
                target: Some(Box::new(Expression::Identifier("foo".to_string()))),
              }),
              BlockItem::Expression(Expression::Getline {
                command: None,
                input: None,
                target: Some(Box::new(Expression::Index {
                  indices: vec![Expression::Number("1".to_string())],
                  name: "foo".to_string(),
                })),
              }),
              BlockItem::Expression(Expression::Getline {
                command: Some("cmd".to_string()),
                input: Some("in".to_string()),
                target: Some(Box::new(Expression::Identifier("bar".to_string()))),
              }),
            ],
          },
          pattern: None,
        })],
      })
      .run();
  }

  #[test]
  fn parses_grouped_expression() {
    Test::new()
      .input("{ (1 + 2) * 3 }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![BlockItem::Expression(Expression::Binary {
              left: Box::new(Expression::Binary {
                left: Box::new(Expression::Number("1".to_string())),
                operator: BinaryOp::Add,
                right: Box::new(Expression::Number("2".to_string())),
              }),
              operator: BinaryOp::Multiply,
              right: Box::new(Expression::Number("3".to_string())),
            })],
          },
          pattern: None,
        })],
      })
      .run();
  }

  #[test]
  fn parses_if_else_statement() {
    Test::new()
      .input("{ if (a) { b } else { c } }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![BlockItem::If {
              condition: Expression::Identifier("a".to_string()),
              else_branch: Some(Block {
                items: vec![BlockItem::Expression(Expression::Identifier(
                  "c".to_string(),
                ))],
              }),
              then_branch: Block {
                items: vec![BlockItem::Expression(Expression::Identifier(
                  "b".to_string(),
                ))],
              },
            }],
          },
          pattern: None,
        })],
      })
      .run();
  }

  #[test]
  fn parses_in_operator() {
    Test::new()
      .input("{ a in b }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![BlockItem::Expression(Expression::Binary {
              left: Box::new(Expression::Identifier("a".to_string())),
              operator: BinaryOp::In,
              right: Box::new(Expression::Identifier("b".to_string())),
            })],
          },
          pattern: None,
        })],
      })
      .run();
  }

  #[test]
  fn parses_increment_decrement() {
    Test::new()
      .input("{ ++a }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![BlockItem::Expression(Expression::PreIncrement(
              Box::new(Expression::Identifier("a".to_string())),
            ))],
          },
          pattern: None,
        })],
      })
      .run();
  }

  #[test]
  fn parses_logical_operators() {
    Test::new()
      .input("{ a || b && c }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![BlockItem::Expression(Expression::Binary {
              left: Box::new(Expression::Identifier("a".to_string())),
              operator: BinaryOp::Or,
              right: Box::new(Expression::Binary {
                left: Box::new(Expression::Identifier("b".to_string())),
                operator: BinaryOp::And,
                right: Box::new(Expression::Identifier("c".to_string())),
              }),
            })],
          },
          pattern: None,
        })],
      })
      .run();
  }

  #[test]
  fn parses_loop_statements() {
    Test::new()
      .input(
        "{ while (a) { b } do { c } while (d) for (i = 0; i < 3; i++) { e } }",
      )
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![
              BlockItem::While {
                body: Block {
                  items: vec![BlockItem::Expression(Expression::Identifier(
                    "b".to_string(),
                  ))],
                },
                condition: Expression::Identifier("a".to_string()),
              },
              BlockItem::DoWhile {
                body: Block {
                  items: vec![BlockItem::Expression(Expression::Identifier(
                    "c".to_string(),
                  ))],
                },
                condition: Expression::Identifier("d".to_string()),
              },
              BlockItem::For {
                body: Block {
                  items: vec![BlockItem::Expression(Expression::Identifier(
                    "e".to_string(),
                  ))],
                },
                condition: Some(vec![Expression::Binary {
                  left: Box::new(Expression::Identifier("i".to_string())),
                  operator: BinaryOp::Less,
                  right: Box::new(Expression::Number("3".to_string())),
                }]),
                initializer: Some(vec![Expression::Assignment {
                  operator: AssignOp::Assign,
                  target: Box::new(Expression::Identifier("i".to_string())),
                  value: Box::new(Expression::Number("0".to_string())),
                }]),
                update: Some(vec![Expression::PostIncrement(Box::new(
                  Expression::Identifier("i".to_string()),
                ))]),
              },
            ],
          },
          pattern: None,
        })],
      })
      .run();
  }

  #[test]
  fn parses_match_operators() {
    Test::new()
      .input("{ a ~ /foo/ }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![BlockItem::Expression(Expression::Binary {
              left: Box::new(Expression::Identifier("a".to_string())),
              operator: BinaryOp::Match,
              right: Box::new(Expression::Regex("foo".to_string())),
            })],
          },
          pattern: None,
        })],
      })
      .run();
  }

  #[test]
  fn parses_negation() {
    Test::new()
      .input("{ -a }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![BlockItem::Expression(Expression::Unary {
              operand: Box::new(Expression::Identifier("a".to_string())),
              operator: UnaryOp::Negate,
            })],
          },
          pattern: None,
        })],
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
              BlockItem::Expression(Expression::Identifier("foo".to_string())),
              BlockItem::Block(Block {
                items: vec![
                  BlockItem::Expression(Expression::Identifier(
                    "bar".to_string(),
                  )),
                  BlockItem::Block(Block {
                    items: vec![BlockItem::Expression(Expression::Identifier(
                      "baz".to_string(),
                    ))],
                  }),
                ],
              }),
              BlockItem::Expression(Expression::Identifier("qux".to_string())),
            ],
          },
          pattern: None,
        })],
      })
      .run();
  }

  #[test]
  fn parses_nested_field_access() {
    Test::new()
      .input("{ $$1 }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![BlockItem::Expression(Expression::Unary {
              operand: Box::new(Expression::Unary {
                operand: Box::new(Expression::Number("1".to_string())),
                operator: UnaryOp::FieldAccess,
              }),
              operator: UnaryOp::FieldAccess,
            })],
          },
          pattern: None,
        })],
      })
      .run();
  }

  #[test]
  fn parses_pattern_actions() {
    Test::new()
      .input("BEGIN { foo } END { bar } baz { qux } /foo/ { bob } { bar }")
      .expected(Program {
        items: vec![
          TopLevelItem::PatternAction(PatternAction {
            action: Block {
              items: vec![BlockItem::Expression(Expression::Identifier(
                "foo".to_string(),
              ))],
            },
            pattern: Some(Pattern::Begin),
          }),
          TopLevelItem::PatternAction(PatternAction {
            action: Block {
              items: vec![BlockItem::Expression(Expression::Identifier(
                "bar".to_string(),
              ))],
            },
            pattern: Some(Pattern::End),
          }),
          TopLevelItem::PatternAction(PatternAction {
            action: Block {
              items: vec![BlockItem::Expression(Expression::Identifier(
                "qux".to_string(),
              ))],
            },
            pattern: Some(Pattern::Expression(Expression::Identifier(
              "baz".to_string(),
            ))),
          }),
          TopLevelItem::PatternAction(PatternAction {
            action: Block {
              items: vec![BlockItem::Expression(Expression::Identifier(
                "bob".to_string(),
              ))],
            },
            pattern: Some(Pattern::Expression(Expression::Regex(
              "foo".to_string(),
            ))),
          }),
          TopLevelItem::PatternAction(PatternAction {
            action: Block {
              items: vec![BlockItem::Expression(Expression::Identifier(
                "bar".to_string(),
              ))],
            },
            pattern: None,
          }),
        ],
      })
      .run();
  }

  #[test]
  fn parses_postfix_increment() {
    Test::new()
      .input("{ a++ }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![BlockItem::Expression(Expression::PostIncrement(
              Box::new(Expression::Identifier("a".to_string())),
            ))],
          },
          pattern: None,
        })],
      })
      .run();
  }

  #[test]
  fn parses_print_and_printf_statements() {
    Test::new()
      .input("{ print a, b >> \"foo\"; printf \"%s\", a >> \"bar\" }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![
              BlockItem::Print {
                arguments: vec![
                  Expression::Identifier("a".to_string()),
                  Expression::Identifier("b".to_string()),
                ],
                redirection: Some(OutputRedirection::Append("foo".to_string())),
              },
              BlockItem::Printf {
                arguments: vec![Expression::Identifier("a".to_string())],
                format: "%s".to_string(),
                redirection: Some(OutputRedirection::Append("bar".to_string())),
              },
            ],
          },
          pattern: None,
        })],
      })
      .run();
  }

  #[test]
  fn parses_string_expression() {
    Test::new()
      .input("{ \"hello\" }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![BlockItem::Expression(Expression::String(
              "hello".to_string(),
            ))],
          },
          pattern: None,
        })],
      })
      .run();
  }

  #[test]
  fn parses_switch_statement() {
    Test::new()
      .input("{ switch (a) { case 1: print b; break; default: next } }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![BlockItem::Switch {
              cases: vec![
                SwitchCase {
                  label: SwitchLabel::Case(Expression::Number("1".to_string())),
                  statements: vec![
                    BlockItem::Print {
                      arguments: vec![Expression::Identifier("b".to_string())],
                      redirection: None,
                    },
                    BlockItem::Break,
                  ],
                },
                SwitchCase {
                  label: SwitchLabel::Default,
                  statements: vec![BlockItem::Next],
                },
              ],
              expression: Expression::Identifier("a".to_string()),
            }],
          },
          pattern: None,
        })],
      })
      .run();
  }

  #[test]
  fn parses_ternary() {
    Test::new()
      .input("{ a ? b : c }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![BlockItem::Expression(Expression::Ternary {
              condition: Box::new(Expression::Identifier("a".to_string())),
              else_branch: Box::new(Expression::Identifier("c".to_string())),
              then_branch: Box::new(Expression::Identifier("b".to_string())),
            })],
          },
          pattern: None,
        })],
      })
      .run();
  }

  #[test]
  fn parses_unary_operators() {
    Test::new()
      .input("{ !a; ~b }")
      .expected(Program {
        items: vec![TopLevelItem::PatternAction(PatternAction {
          action: Block {
            items: vec![
              BlockItem::Expression(Expression::Unary {
                operand: Box::new(Expression::Identifier("a".to_string())),
                operator: UnaryOp::Not,
              }),
              BlockItem::Expression(Expression::Unary {
                operand: Box::new(Expression::Identifier("b".to_string())),
                operator: UnaryOp::BitwiseNot,
              }),
            ],
          },
          pattern: None,
        })],
      })
      .run();
  }
}
