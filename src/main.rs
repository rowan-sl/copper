//! no this is not and will never be a self hosting compiler.
//!
//! why on earth would you want that

#[macro_use]
extern crate log;

pub mod analyzer;
pub mod codegen;
pub mod lexer2;
pub mod parse2;
pub mod parsing;
pub mod util;

use std::{fmt::Write as _, fs::OpenOptions, io::Write as _, path::PathBuf};

use anyhow::Result;
use clap::Parser;

#[derive(Debug, Parser)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// The code to compile
    file: PathBuf,

    /// The output file for the generated code.
    ///
    /// Will be automatically generated from the input file name if not given
    #[clap(long, short)]
    out: Option<PathBuf>,

    /// flags to enable internal compiler features.
    #[clap(short = 'Z', value_enum)]
    flags: Vec<Flag>,
}

#[derive(Debug, Clone, clap::ValueEnum, PartialEq, Eq)]
enum Flag {
    /// display lexed tokens
    ShowLexed,
    /// dsplay scoped tokens
    ShowScoped,
    /// display parsed AST
    ShowAst,
}

fn main() -> Result<()> {
    pretty_env_logger::formatted_builder()
        .filter_level(log::LevelFilter::Debug)
        .init();
    let mut args = Args::parse();
    if args.out.is_none() {
        let mut out = args.file.clone();
        out.set_extension("mlog");
        args.out = Some(out);
    }
    if !args.file.is_file() {
        anyhow::bail!("Input file must be a file!");
    }
    debug!("{args:#?}");
    let raw = util::load_text(&args.file)?;
    info!("Compiling {} ...", args.file.display());

    debug!("Lexing tokens");
    let tokens = lexer2::base_lex(raw, &parse2::rules())?;

    if args.flags.contains(&Flag::ShowLexed) {
        let mut dbgd = String::new();
        for tk in &tokens {
            write!(dbgd, "{:>5}:{:<3} - {:?}\n", tk.loc.line, tk.loc.ch, tk.val).unwrap();
        }
        info!("Lexed tokens:\n{}", dbgd);
    }

    debug!("Scoping tokens");
    let scoped = parse2::scoped::scope_out(tokens)?;

    if args.flags.contains(&Flag::ShowScoped) {
        //TODO nice printing like the lexed token debugging
        info!("Scoped tokens:\n{:#?}", scoped);
    }

    debug!("Parsing AST");
    let ast = parse2::build_ast(scoped)?;

    if args.flags.contains(&Flag::ShowAst) {
        info!("AST:\n{ast:#?}");
    }

    // let ast = parsing::parse(raw)?;
    // info!("Checking ast...");
    // analyzer::check_ast(&ast)?;
    // info!("Generating program structure...");
    // let prog = analyzer::interpret_ast(ast)?;
    // debug!("program: {:#?}", prog);

    // info!("Generating mlog code...");
    // let mut gen = codegen::MlogEmitter::new();

    // gen.include(codegen::PRELUDE.to_string());
    // gen.include(codegen::PREPARE.to_string());
    // //* codegen goes here

    // //* codegen ends here
    // gen.include(codegen::CLEANUP.to_string());

    // let raw_output = gen.into_output();
    // let mut out = OpenOptions::new()
    //     .write(true)
    //     .create(true)
    //     .open(args.out.unwrap())?;
    // out.write_all(raw_output.as_bytes())?;

    info!("Done");
    Ok(())
}
