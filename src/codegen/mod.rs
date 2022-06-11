pub const PRELUDE: &'static str = include_str!("../template/prelude.mlog");
pub const PREPARE: &'static str = include_str!("../template/prepare.mlog");
pub const CLEANUP: &'static str = include_str!("../template/cleanup.mlog");

#[derive(Debug, Clone, PartialEq)]
pub struct MlogEmitter {
    raw: String,
}

impl MlogEmitter {
    pub const fn new() -> Self {
        Self { raw: String::new() }
    }

    /// include a section of mlog code. code MUST end with a newline
    pub fn include(&mut self, code: String) {
        self.raw.push_str(&code);
    }

    pub fn into_output(self) -> String {
        self.raw
    }

    pub fn emit_raw(&mut self, line: &str) {
        self.raw.push_str(line);
        self.raw.push('\n');
    }

    pub fn emit(&mut self, inst: Instruction) {
        match inst {
            Instruction::End {} => {
                self.raw.push_str("end");
                self.raw.push('\n');
            }
            Instruction::NoOp => {
                self.raw.push_str("noop");
                self.raw.push('\n');
            }
            Instruction::WaitForReset => {
                self.raw.push_str("jump 24 always 0 0");
                self.raw.push('\n');
            }
            other => warn!("Unsuported instruction {other:#?}"),
        }
    }
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
    status: implemented

    args: none
    mlog rep: exists (`noop`), shows up as `invalid instruction` in game
    */
    WaitForReset,
    /*
    class: system operation, not in mindustry, requires prelude
    priority: high
    status: implemented

    args: none
    mlog rep: `jump <> always 0 0` (spacific line in preulde)
    */
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
