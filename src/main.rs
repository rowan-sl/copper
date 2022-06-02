#[macro_use]
extern crate log;

pub mod parsing;

use std::{io::Read, fs::OpenOptions};

use anyhow::Result;

fn main() -> Result<()> {
    pretty_env_logger::formatted_builder()
        .filter_level(log::LevelFilter::Debug)
        .init();
    let mut file = OpenOptions::new().read(true).write(false).open("test.mc")?;
    let mut raw = String::new();
    file.read_to_string(&mut raw)?;
    let _ast = parsing::parse(raw)?;
    Ok(())
}

// TODO convert information to a spreadsheet
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Instruction {
    Read,
    /*
    class: io
    priority: high
    status: unimplemented
    */
    Write,
    /*
    class: io
    priority: high
    status: unimplemented
    */
    Draw,
    /*
    class: graphics
    priority: mid
    status: unimplemented
    */
    Print,
    /*
    class: io, debug
    priority: VERY HIGH
    status: unimplemented
    */
    DrawFlush,
    /*
    class: graphics
    priority: mid,
    status: unimplemented
    */
    PrintFlush,
    /*
    class: io, debug
    priority: very high
    status: unimplemented
    */
    GetLink,
    /*
    class: ??
    priority: low
    status: unimplemented
    */
    Controll,
    /*
    class: io, unit interface
    priority: high
    status: unimplemented
    */
    Radar,
    /*
    class: ??
    priority: low
    status: unimplemented
    */
    Sensor,
    /*
    class: io, unit interface
    priority: high
    status: unimplemented
    */
    Set,
    /*
    class: basic operation
    priority: very very high
    status: unimplemented
    */
    Operation,
    /*
    class: basic operation
    priority: very very high
    status: unimplemented
    */
    Wait,
    /*
    class: basic operation
    priority: very very high
    status: unimplemented
    */
    Lookup,
    /*
    class: ??
    priority: low
    status: unimplemented
    */
    PackColor,
    /*
    class: basic operation, graphics
    priority: mid
    status: unimplemented
    */
    End {},
    /*
    class: basic operation, congroll flow
    priority: very very high
    status: done

    args: none
    mlog rep: "end"
    */
    Jump {
        addr: usize,
        condition: JumpCondition,
    },
    /*
    class: basic operation, controll flow
    priority: very very high
    status: in progress

    args: {addr} {conditional} {value1} {value2}

    mlog rep:
    ```
    jump 8 equal x false
    jump 8 notEqual x false
    jump 8 lessThan x false
    jump 8 lessThanEq x false
    jump 8 greaterThan x false
    jump 8 greaterThanEq x false
    jump 8 strictEqual x false
    jump 8 always x false
    draw clear 0 0 0 0 0 0
    ```
    */
    UnitBind,
    /*
    class: unit controll
    priority: very low
    status: unimplemented
    */
    UnitControll,
    /*
    class: unit controll
    priority: very low
    status: unimplemented
    */
    UnitRadar,
    /*
    class: unit controll
    priority: very low
    status: unimplemented
    */
    UnitLocate,
    /*
    class: unit controll
    priority: very low
    status: unimplemented
    */
    NoOp,
    /*
    class: basic operation, not in mindustry
    priority: very very high
    status: unimplemented

    args: none
    mlog rep: does not exist in mlog
    */
}

impl ToString for Instruction {
    fn to_string(&self) -> String {
        use Instruction::*;
        match self {
            End {} => {
                String::from("end")
            }

            _ => todo!()
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum JumpCondition {
    Equal,
    NotEqual,
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    StrictEqual,
    /// Note: this is the only one that does NOT take arguments
    Always,
}

impl ToString for JumpCondition {
    fn to_string(&self) -> String {
        use JumpCondition::*;
        match self {
            Equal => String::from("equal"),
            NotEqual => String::from("notEqual"),
            LessThan => String::from("lessThan"),
            LessThanEq => String::from("lessThanEq"),
            GreaterThan => String::from("greaterThan"),
            GreaterThanEq => String::from("greaterThanEq"),
            StrictEqual => String::from("strictEqual"),
            Always => String::from("always"),
        }
    }
}
