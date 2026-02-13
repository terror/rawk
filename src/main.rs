use {
  anyhow::{Error, bail},
  arguments::Arguments,
  ariadne::{Color, Label, Report, ReportKind, Source},
  ast::{
    Block, BlockItem, FunctionDefinition, Pattern, PatternAction, Program,
    TopLevelItem,
  },
  chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
  },
  clap::Parser as Clap,
  lexer::{Span, Spanned},
  std::{
    fmt::{self, Display, Formatter},
    ops::Range,
    fs,
    path::{Path, PathBuf},
    process,
  },
  token::Token,
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
