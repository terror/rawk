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

  let grouped = expr.delimited_by(just(Token::LParen), just(Token::RParen));

  let ident_expr = identifier.map(Expression::Identifier);

  choice((
    number,
    string,
    regex,
    function_call,
    array_subscript,
    grouped,
    ident_expr,
  ))
}

fn expression_parser<'src, I>()
-> impl Parser<'src, I, Expression, extra::Err<ParseError<'src>>> + Clone
where
  I: ValueInput<'src, Token = Token, Span = Span>,
{
  recursive(|expr| {
    let atom = atom_parser(expr.clone());

    let pratt_expr = pratt_parser(atom);

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

  let pattern = choice((
    just(Token::Begin).to(Pattern::Begin),
    just(Token::End).to(Pattern::End),
    expr.clone().map(Pattern::Expression),
  ));

  let block_item_expr = expr.map(BlockItem::Expression);

  let block = recursive(|block| {
    let item = block
      .clone()
      .map(BlockItem::Block)
      .or(block_item_expr.clone());

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

#[allow(clippy::too_many_lines)]
fn pratt_parser<'src, I>(
  atom: impl Parser<'src, I, Expression, extra::Err<ParseError<'src>>>
  + Clone
  + 'src,
) -> impl Parser<'src, I, Expression, extra::Err<ParseError<'src>>> + Clone
where
  I: ValueInput<'src, Token = Token, Span = Span>,
{
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
    prefix(10, just(Token::Minus), |_, rhs, _| Expression::Unary {
      operand: Box::new(rhs),
      operator: UnaryOp::Negate,
    }),
    prefix(10, just(Token::Plus), |_, rhs, _| Expression::Unary {
      operand: Box::new(rhs),
      operator: UnaryOp::Positive,
    }),
    infix(right(9), just(Token::Caret), |l, _, r, _| {
      Expression::Binary {
        left: Box::new(l),
        operator: BinaryOp::Power,
        right: Box::new(r),
      }
    }),
    infix(left(8), just(Token::Star), |l, _, r, _| {
      Expression::Binary {
        left: Box::new(l),
        operator: BinaryOp::Multiply,
        right: Box::new(r),
      }
    }),
    infix(left(8), just(Token::Slash), |l, _, r, _| {
      Expression::Binary {
        left: Box::new(l),
        operator: BinaryOp::Divide,
        right: Box::new(r),
      }
    }),
    infix(left(8), just(Token::Percent), |l, _, r, _| {
      Expression::Binary {
        left: Box::new(l),
        operator: BinaryOp::Modulo,
        right: Box::new(r),
      }
    }),
    infix(left(7), just(Token::Plus), |l, _, r, _| {
      Expression::Binary {
        left: Box::new(l),
        operator: BinaryOp::Add,
        right: Box::new(r),
      }
    }),
    infix(left(7), just(Token::Minus), |l, _, r, _| {
      Expression::Binary {
        left: Box::new(l),
        operator: BinaryOp::Subtract,
        right: Box::new(r),
      }
    }),
    infix(left(5), just(Token::Less), |l, _, r, _| {
      Expression::Binary {
        left: Box::new(l),
        operator: BinaryOp::Less,
        right: Box::new(r),
      }
    }),
    infix(left(5), just(Token::LessEqual), |l, _, r, _| {
      Expression::Binary {
        left: Box::new(l),
        operator: BinaryOp::LessEqual,
        right: Box::new(r),
      }
    }),
    infix(left(5), just(Token::EqualEqual), |l, _, r, _| {
      Expression::Binary {
        left: Box::new(l),
        operator: BinaryOp::Equal,
        right: Box::new(r),
      }
    }),
    infix(left(5), just(Token::BangEqual), |l, _, r, _| {
      Expression::Binary {
        left: Box::new(l),
        operator: BinaryOp::NotEqual,
        right: Box::new(r),
      }
    }),
    infix(left(5), just(Token::GreaterEqual), |l, _, r, _| {
      Expression::Binary {
        left: Box::new(l),
        operator: BinaryOp::GreaterEqual,
        right: Box::new(r),
      }
    }),
    infix(left(5), just(Token::Greater), |l, _, r, _| {
      Expression::Binary {
        left: Box::new(l),
        operator: BinaryOp::Greater,
        right: Box::new(r),
      }
    }),
    infix(left(4), just(Token::Tilde), |l, _, r, _| {
      Expression::Binary {
        left: Box::new(l),
        operator: BinaryOp::Match,
        right: Box::new(r),
      }
    }),
    infix(left(4), just(Token::BangTilde), |l, _, r, _| {
      Expression::Binary {
        left: Box::new(l),
        operator: BinaryOp::NotMatch,
        right: Box::new(r),
      }
    }),
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

  fn assign(
    target: Expression,
    operator: AssignOp,
    value: Expression,
  ) -> Expression {
    Expression::Assignment {
      operator,
      target: Box::new(target),
      value: Box::new(value),
    }
  }

  fn binop(
    left: Expression,
    operator: BinaryOp,
    right: Expression,
  ) -> Expression {
    Expression::Binary {
      left: Box::new(left),
      operator,
      right: Box::new(right),
    }
  }

  fn expr_item(expr: Expression) -> BlockItem {
    BlockItem::Expression(expr)
  }

  fn ident(s: &str) -> Expression {
    Expression::Identifier(s.to_string())
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

  fn num(s: &str) -> Expression {
    Expression::Number(s.to_string())
  }

  #[test]
  fn parses_arithmetic_expressions() {
    Test::new()
      .input("{ 1 + 2 * 3 }")
      .expected(Program {
        items: vec![pattern_action(
          None,
          vec![expr_item(binop(
            num("1"),
            BinaryOp::Add,
            binop(num("2"), BinaryOp::Multiply, num("3")),
          ))],
        )],
      })
      .run();
  }

  #[test]
  fn parses_array_subscript() {
    Test::new()
      .input("{ a[1] }")
      .expected(Program {
        items: vec![pattern_action(
          None,
          vec![expr_item(Expression::Index {
            indices: vec![num("1")],
            name: "a".to_string(),
          })],
        )],
      })
      .run();
  }

  #[test]
  fn parses_assignment() {
    Test::new()
      .input("{ a = 1 }")
      .expected(Program {
        items: vec![pattern_action(
          None,
          vec![expr_item(assign(ident("a"), AssignOp::Assign, num("1")))],
        )],
      })
      .run();
  }

  #[test]
  fn parses_chained_assignment() {
    Test::new()
      .input("{ a = b = 1 }")
      .expected(Program {
        items: vec![pattern_action(
          None,
          vec![expr_item(assign(
            ident("a"),
            AssignOp::Assign,
            assign(ident("b"), AssignOp::Assign, num("1")),
          ))],
        )],
      })
      .run();
  }

  #[test]
  fn parses_comparison_operators() {
    Test::new()
      .input("{ a < b }")
      .expected(Program {
        items: vec![pattern_action(
          None,
          vec![expr_item(binop(ident("a"), BinaryOp::Less, ident("b")))],
        )],
      })
      .run();
  }

  #[test]
  fn parses_complex_expression() {
    Test::new()
      .input("{ $1 > 0 && $2 ~ /foo/ }")
      .expected(Program {
        items: vec![pattern_action(
          None,
          vec![expr_item(binop(
            binop(
              unary(UnaryOp::FieldAccess, num("1")),
              BinaryOp::Greater,
              num("0"),
            ),
            BinaryOp::And,
            binop(
              unary(UnaryOp::FieldAccess, num("2")),
              BinaryOp::Match,
              Expression::Regex("foo".to_string()),
            ),
          ))],
        )],
      })
      .run();
  }

  #[test]
  fn parses_compound_assignment() {
    Test::new()
      .input("{ a += 1 }")
      .expected(Program {
        items: vec![pattern_action(
          None,
          vec![expr_item(assign(ident("a"), AssignOp::Add, num("1")))],
        )],
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
        items: vec![pattern_action(
          None,
          vec![expr_item(binop(
            num("2"),
            BinaryOp::Power,
            binop(num("3"), BinaryOp::Power, num("4")),
          ))],
        )],
      })
      .run();
  }

  #[test]
  fn parses_expression_as_pattern() {
    Test::new()
      .input("$1 > 0 { foo }")
      .expected(Program {
        items: vec![pattern_action(
          Some(Pattern::Expression(binop(
            unary(UnaryOp::FieldAccess, num("1")),
            BinaryOp::Greater,
            num("0"),
          ))),
          vec![expr_item(ident("foo"))],
        )],
      })
      .run();
  }

  #[test]
  fn parses_field_access() {
    Test::new()
      .input("{ $1 }")
      .expected(Program {
        items: vec![pattern_action(
          None,
          vec![expr_item(unary(UnaryOp::FieldAccess, num("1")))],
        )],
      })
      .run();
  }

  #[test]
  fn parses_function_call_expression() {
    Test::new()
      .input("{ foo(1, 2) }")
      .expected(Program {
        items: vec![pattern_action(
          None,
          vec![expr_item(Expression::FunctionCall {
            arguments: vec![num("1"), num("2")],
            name: "foo".to_string(),
          })],
        )],
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
              items: vec![expr_item(ident("bar"))],
            },
            name: "foo".to_string(),
            parameters: Vec::new(),
          }),
          TopLevelItem::Function(FunctionDefinition {
            body: Block {
              items: vec![expr_item(ident("foo"))],
            },
            name: "baz".to_string(),
            parameters: vec!["qux".to_string(), "bob".to_string()],
          }),
        ],
      })
      .run();
  }

  #[test]
  fn parses_grouped_expression() {
    Test::new()
      .input("{ (1 + 2) * 3 }")
      .expected(Program {
        items: vec![pattern_action(
          None,
          vec![expr_item(binop(
            binop(num("1"), BinaryOp::Add, num("2")),
            BinaryOp::Multiply,
            num("3"),
          ))],
        )],
      })
      .run();
  }

  #[test]
  fn parses_in_operator() {
    Test::new()
      .input("{ a in b }")
      .expected(Program {
        items: vec![pattern_action(
          None,
          vec![expr_item(binop(ident("a"), BinaryOp::In, ident("b")))],
        )],
      })
      .run();
  }

  #[test]
  fn parses_increment_decrement() {
    Test::new()
      .input("{ ++a }")
      .expected(Program {
        items: vec![pattern_action(
          None,
          vec![expr_item(Expression::PreIncrement(Box::new(ident("a"))))],
        )],
      })
      .run();
  }

  #[test]
  fn parses_logical_operators() {
    Test::new()
      .input("{ a || b && c }")
      .expected(Program {
        items: vec![pattern_action(
          None,
          vec![expr_item(binop(
            ident("a"),
            BinaryOp::Or,
            binop(ident("b"), BinaryOp::And, ident("c")),
          ))],
        )],
      })
      .run();
  }

  #[test]
  fn parses_match_operators() {
    Test::new()
      .input("{ a ~ /foo/ }")
      .expected(Program {
        items: vec![pattern_action(
          None,
          vec![expr_item(binop(
            ident("a"),
            BinaryOp::Match,
            Expression::Regex("foo".to_string()),
          ))],
        )],
      })
      .run();
  }

  #[test]
  fn parses_negation() {
    Test::new()
      .input("{ -a }")
      .expected(Program {
        items: vec![pattern_action(
          None,
          vec![expr_item(unary(UnaryOp::Negate, ident("a")))],
        )],
      })
      .run();
  }

  #[test]
  fn parses_nested_blocks() {
    Test::new()
      .input("{ foo { bar { baz } } qux }")
      .expected(Program {
        items: vec![pattern_action(
          None,
          vec![
            expr_item(ident("foo")),
            BlockItem::Block(Block {
              items: vec![
                expr_item(ident("bar")),
                BlockItem::Block(Block {
                  items: vec![expr_item(ident("baz"))],
                }),
              ],
            }),
            expr_item(ident("qux")),
          ],
        )],
      })
      .run();
  }

  #[test]
  fn parses_nested_field_access() {
    Test::new()
      .input("{ $$1 }")
      .expected(Program {
        items: vec![pattern_action(
          None,
          vec![expr_item(unary(
            UnaryOp::FieldAccess,
            unary(UnaryOp::FieldAccess, num("1")),
          ))],
        )],
      })
      .run();
  }

  #[test]
  fn parses_pattern_actions() {
    Test::new()
      .input("BEGIN { foo } END { bar } baz { qux } /foo/ { bob } { bar }")
      .expected(Program {
        items: vec![
          pattern_action(Some(Pattern::Begin), vec![expr_item(ident("foo"))]),
          pattern_action(Some(Pattern::End), vec![expr_item(ident("bar"))]),
          pattern_action(
            Some(Pattern::Expression(ident("baz"))),
            vec![expr_item(ident("qux"))],
          ),
          pattern_action(
            Some(Pattern::Expression(Expression::Regex("foo".to_string()))),
            vec![expr_item(ident("bob"))],
          ),
          pattern_action(None, vec![expr_item(ident("bar"))]),
        ],
      })
      .run();
  }

  #[test]
  fn parses_postfix_increment() {
    Test::new()
      .input("{ a++ }")
      .expected(Program {
        items: vec![pattern_action(
          None,
          vec![expr_item(Expression::PostIncrement(Box::new(ident("a"))))],
        )],
      })
      .run();
  }

  #[test]
  fn parses_string_expression() {
    Test::new()
      .input("{ \"hello\" }")
      .expected(Program {
        items: vec![pattern_action(
          None,
          vec![expr_item(Expression::String("hello".to_string()))],
        )],
      })
      .run();
  }

  #[test]
  fn parses_ternary() {
    Test::new()
      .input("{ a ? b : c }")
      .expected(Program {
        items: vec![pattern_action(
          None,
          vec![expr_item(Expression::Ternary {
            condition: Box::new(ident("a")),
            else_branch: Box::new(ident("c")),
            then_branch: Box::new(ident("b")),
          })],
        )],
      })
      .run();
  }

  #[test]
  fn parses_unary_operators() {
    Test::new()
      .input("{ !a }")
      .expected(Program {
        items: vec![pattern_action(
          None,
          vec![expr_item(unary(UnaryOp::Not, ident("a")))],
        )],
      })
      .run();
  }

  fn pattern_action(
    pattern: Option<Pattern>,
    items: Vec<BlockItem>,
  ) -> TopLevelItem {
    TopLevelItem::PatternAction(PatternAction {
      action: Block { items },
      pattern,
    })
  }

  fn unary(operator: UnaryOp, operand: Expression) -> Expression {
    Expression::Unary {
      operand: Box::new(operand),
      operator,
    }
  }
}
