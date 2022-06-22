//! no this is not and will never be a self hosting compiler.
//!
//! why on earth would you want that

#[macro_use]
extern crate log;

//TODO code in this modlue is broken, works off of the old AST system
pub mod analyzer;
pub mod codegen;
pub mod lexer2;
pub mod lir;
pub mod parse2;
pub mod util;

use std::{fmt::Write as _, fs::OpenOptions, io::Write as _, path::PathBuf};

use anyhow::Result;
use clap::Parser;

use crate::parse2::AstNode;

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
        debug!("Lexed tokens:\n{}", dbgd);
    }

    debug!("Scoping tokens");
    let scoped = parse2::scoped::scope_out(tokens)?;

    if args.flags.contains(&Flag::ShowScoped) {
        //TODO nice printing like the lexed token debugging
        debug!("Scoped tokens:\n{:#?}", scoped);
    }

    debug!("Parsing AST");
    let ast = AstNode::parse(scoped)?;

    if args.flags.contains(&Flag::ShowAst) {
        debug!("AST:\n{ast:#?}");
    }

    info!("Generating program structure...");
    let prog = analyzer::interpret_ast(ast)?;
    debug!("program: {:#?}", prog);

    // let main_fn_code = prog.raw_functions.remove(&"main".to_string()).unwrap();
    // analyzer::walk::walk_controll_flow(
    //     "main".to_string(),
    //     main_fn_code.code,
    //     &prog.raw_functions,
    //     &prog.global_const,
    //     &prog.const_idents,
    // );

    info!("Performing validiation and generating IR");

    let mut function_builtins = std::collections::HashMap::new();
    function_builtins.insert("println".to_string(), ());
    function_builtins.insert("printflush".to_string(), ());

    for (name, function) in &prog.raw_functions {
        info!("Validing function `{name}`");
        analyzer::walk::walk_controll_flow(
            name.clone(),
            function.code.clone(),
            &function.args,
            &prog.raw_functions,
            &function_builtins,
            &prog.global_const,
            &prog.const_idents,
        );
    }

    info!("Getting main fn LIR...");

    let raw_main_fn = prog.raw_functions.get(&String::from("main")).unwrap();

    let main_fn_lir = analyzer::walk::walk_controll_flow(
        "main".to_string(),
        raw_main_fn.code.clone(),
        &raw_main_fn.args,
        &prog.raw_functions,
        &function_builtins,
        &prog.global_const,
        &prog.const_idents,
    );

    info!("Generating mlog code...");

    let mut gen = codegen::MlogEmitter::new();

    gen.include(codegen::PRELUDE.to_string());
    gen.include(codegen::PREPARE.to_string());
    gen.emit_raw("set null \"This is where the codes goes\"\n");
    //* codegen goes here

    let main_instrs = codegen::gen::gen_instructions(String::from("main"), main_fn_lir);
    for instr in main_instrs {
        gen.emit(instr)
    }

    //* codegen ends here
    gen.emit_raw("\nset null \"This is where the codes ends\"");
    gen.include(codegen::CLEANUP.to_string());

    let raw_output = gen.into_output();
    let mut out = OpenOptions::new()
        .write(true)
        .create(true)
        .open(args.out.unwrap())?;
    out.write_all(raw_output.as_bytes())?;

    info!("Done");
    Ok(())
}
