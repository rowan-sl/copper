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
pub mod typing;
pub mod util;

use std::{collections::HashMap, fmt::Write as _, fs::OpenOptions, io::Write as _, path::PathBuf};

use anyhow::Result;
use clap::Parser;
use uuid::Uuid;

use crate::{
    codegen::{gen::Metadata, Instruction},
    parse2::AstNode,
};

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

    info!("Performing typeck...");
    analyzer::typeck::perform(&prog);

    info!("Performing validiation and generating LIR");

    info!("Generating LIR for constants...");
    let constants_lir = analyzer::walk::generate_constats_lir(prog.global_const.clone());

    info!("Generating LIR for functions...");
    let mut function_builtins = HashMap::new();
    function_builtins.insert("println".to_string(), ());
    function_builtins.insert("printflush".to_string(), ());

    let mut function_lir: HashMap<String, Vec<lir::Operation>> = HashMap::new();
    for (name, function) in &prog.raw_functions {
        info!("Generating LIR for function `{name}`");
        let lir = analyzer::walk::walk_controll_flow(
            name.clone(),
            function.code.clone(),
            &function.args,
            &prog.raw_functions,
            &function_builtins,
            &prog.global_const,
            &prog.const_idents,
        );
        function_lir.insert(name.clone(), lir);
    }

    info!("Generating mlog code...");

    let mut gen = codegen::MlogEmitter::new();

    gen.include(codegen::PRELUDE.to_string());
    gen.include(codegen::PREPARE.to_string());
    //* codegen goes here

    info!("Generating mlog for constants...");
    let constants_instrs = codegen::gen::gen_constant_bindings(constants_lir.clone());
    gen.emit_raw("# constants section\n");
    gen.emit_many(constants_instrs.clone());

    let mut function_tags: HashMap<String, Uuid> = HashMap::new();
    for (name, _) in &prog.raw_functions {
        function_tags.insert(name.clone(), Uuid::new_v4());
    }

    info!("Generating mlog for functions...");
    gen.emit_raw("\n# function section\n");
    let mut instructions: Vec<(Metadata, Instruction)> = vec![];

    let main_section_tag = Uuid::new_v4();
    instructions.push((
        Metadata::default(),
        Instruction::Jump {
            addr: codegen::MlogAddr::Tag(main_section_tag),
            condition: codegen::JumpCondition::Always,
            args: None,
        },
    ));

    const STACK_MEMCELL_NAME: &str = "bank1";
    const STACK_BASE_ADDR: usize = 0;
    const STACK_SIZE: usize = 510; //its right, dont touch

    for (name, lir) in function_lir {
        info!("Generating mlog for function {name}");
        instructions.push((
            Metadata::default(),
            Instruction::Comment(format!("code for function {name}")),
        ));
        let fn_instrs = codegen::gen::gen_instructions_for_function(
            name,
            lir,
            &constants_lir,
            &prog.const_idents,
            STACK_MEMCELL_NAME.to_string(),
            STACK_BASE_ADDR,
            STACK_SIZE,
            &prog.raw_functions,
            &function_tags,
        );

        instructions.extend(fn_instrs);
    }

    instructions.push((
        Metadata::default(),
        Instruction::Comment("__entry fn code".to_string()),
    ));

    instructions.push((
        Metadata {
            tag: Some(main_section_tag),
        },
        Instruction::NoOp,
    ));

    instructions.extend(codegen::gen::gen_instructions_for_function_call(
        "__entry".to_string(),
        "main".to_string(),
        vec![],
        "null".to_string(),
        STACK_MEMCELL_NAME.to_string(),
        STACK_BASE_ADDR,
        STACK_SIZE,
        &constants_lir,
        &prog.raw_functions,
        &function_tags,
    ));

    let pure_fn_instrs = codegen::gen::make_tags_absoulute(
        instructions,
        codegen::PRELUDE.lines().count()
            + codegen::PREPARE.lines().count()
            + constants_instrs.len(),
    );

    gen.emit_many(pure_fn_instrs);

    //* codegen ends here
    gen.include(codegen::CLEANUP.to_string());

    let raw_output = gen.into_output();
    let mut out = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(args.out.unwrap())?;
    out.write_all(raw_output.as_bytes())?;

    info!("Done");
    Ok(())
}
