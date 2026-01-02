use std::{fmt, io};
use io::{BufRead, Write};

use welly_main::{self as wm, ansi_term::Colour::{Blue}};
use welly_parser::{lexer, ast, parse};

// ----------------------------------------------------------------------------

#[derive(Default)]
pub struct Repl {
    /// The lookup tables required by the lexer.
    pub lexer: lexer::Lexer,

    /// The state required by [`wm::Repl`].
    pub inner: wm::Repl,
}

impl Repl {
    /// Returns `true` if `command` has encountered the end of the input.
    pub fn is_complete(&self) -> bool { self.inner.is_complete }

    /// Prompt for a command, and try to understand it.
    pub fn command(&mut self, input: &mut impl BufRead, output: &mut impl Write)
    -> io::Result<Option<Box<[ast::Stmt]>>> {
        writeln!(output, "\nWelly!")?;
        self.inner.command(input, output, |command| parse(&self.lexer, command))
    }
}

impl fmt::Debug for Repl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { self.inner.fmt(f) }
}

// ----------------------------------------------------------------------------

fn main() -> std::io::Result<()> {
    let mut input = io::stdin().lock();
    let mut output = io::stdout();
    let mut repl = Repl::default();
    while !repl.is_complete() {
        if let Some(stmts) = repl.command(&mut input, &mut output)? {
            for stmt in stmts {
                let stmt_output = format!("{:#?}", stmt);
                writeln!(output, "{}", Blue.paint(stmt_output))?;
            }
        }
    }
    Ok(())
}
