#[macro_use]
extern crate log;

pub mod parsing;
pub mod util;
pub mod codegen;

use std::{path::PathBuf, fs::OpenOptions, io::Write};

use anyhow::Result;
use clap::Parser;

/// Compiler for the copper language, currently targeting mlog (mindustry logic language)
#[derive(Debug, Parser)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// The code to compile
    file: PathBuf,

    /// The output file, usually has the `.mlog` extension.
    ///
    /// Will be automatically generated from the input file name if not given
    out: Option<PathBuf>,
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
    let _ast = parsing::parse(raw)?;
    info!("Generating mlog code...");
    let mut gen = codegen::MlogEmitter::new();

    gen.include(codegen::PRELUDE.to_string());
    //* codegen goes here
    //* codegen ends here
    gen.include(codegen::CLEANUP.to_string());

    let raw_output = gen.into_output();
    let mut out = OpenOptions::new().write(true).create(true).open(args.out.unwrap())?;
    out.write_all(raw_output.as_bytes())?;

    info!("Done");
    Ok(())
}
