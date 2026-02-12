use super::*;

#[derive(Clap)]
#[command(name = "rawk")]
#[command(about = "A modern awk implementation in Rust")]
pub(crate) struct Arguments {
  #[arg(short = 'f', long = "file")]
  file: Option<PathBuf>,
  program: Option<String>,
}

impl Arguments {
  fn build_report<'a>(
    name: &'a str,
    report_message: &'a str,
    span: &Span,
    label_message: impl Display,
  ) -> Report<'a, (&'a str, std::ops::Range<usize>)> {
    let range = span.start..span.end;

    Report::build(ReportKind::Error, (name, range.clone()))
      .with_message(report_message)
      .with_label(
        Label::new((name, range))
          .with_message(label_message.to_string())
          .with_color(Color::Red),
      )
      .finish()
  }

  fn parse_source(name: &str, source: &str) -> Result {
    let (tokens, lex_errors) = lexer::lex(source);

    if !lex_errors.is_empty() {
      for error in &lex_errors {
        Self::build_report(name, "lex error", error.span(), error)
          .eprint((name, Source::from(source)))?;
      }

      bail!("lexing failed with {} errors", lex_errors.len());
    }

    let (program, parse_errors) = parser::parse(tokens, source.len());

    if !parse_errors.is_empty() {
      for error in &parse_errors {
        Self::build_report(name, "parse error", error.span(), error)
          .eprint((name, Source::from(source)))?;
      }

      bail!("parsing failed with {} errors", parse_errors.len());
    }

    match program {
      Some(_) => Ok(()),
      None => bail!("parser produced no output"),
    }
  }

  fn parse_file(path: &Path) -> Result {
    Self::parse_source(&path.to_string_lossy(), &fs::read_to_string(path)?)
  }

  pub(crate) fn run(self) -> Result {
    match (self.file, self.program) {
      (Some(path), None) => Self::parse_file(&path),
      (None, Some(program)) => Self::parse_source("<inline>", &program),
      (Some(_), Some(_)) => bail!("pass either --file or an inline program"),
      (None, None) => bail!("no program provided"),
    }
  }
}
