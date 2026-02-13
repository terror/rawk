use {
  anyhow::{Error, bail},
  arguments::Arguments,
  ariadne::{Color, Label, Report, ReportKind, Source},
  ast::{
    AssignOp, BinaryOp, Block, BlockItem, Expression, FunctionDefinition,
    OutputRedirection, Pattern, PatternAction, Program, SwitchCase,
    SwitchLabel, TopLevelItem, UnaryOp,
  },
  chumsky::{
    input::{Stream, ValueInput},
    pratt::*,
    prelude::*,
  },
  clap::Parser as Clap,
  lexer::{Span, Spanned},
  std::{
    fmt::{self, Display, Formatter},
    fs,
    ops::Range,
    path::{Path, PathBuf},
    process,
  },
  token::{NumberKind, NumberLiteral, Token},
};

mod arguments;
mod ast;
mod lexer;
mod parser;
mod token;

type Result<T = (), E = Error> = std::result::Result<T, E>;

fn main() {
  if let Err(error) = Arguments::parse().run() {
    eprintln!("error: {error}");
    process::exit(1);
  }
}
