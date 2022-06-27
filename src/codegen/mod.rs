use uuid::Uuid;

use crate::parse2::types::lexer_tokens::Op;

pub mod gen;

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
                self.raw.push_str("wait 0");
                self.raw.push('\n');
            }
            Instruction::WaitForReset => {
                // MAKE SHURE THAT THE ADDR \/ HERE IS ACTUALLY THE PROPER LINE IN THE PREULDE!!!!!!!!!!!!
                // IF IT NOT WE WILL ALL SUFFER
                self.raw.push_str("jump 26 always 0 0");
                self.raw.push('\n');
            }
            Instruction::Trap => {
                // NOTE: since this is multi-instruction, an edge case must be added to Instruction::worth to make this work
                self.emit(Instruction::Print {
                    stuff: "\"Error: Encountered trap instr at addr #\"".into(),
                });
                self.emit(Instruction::Print {
                    stuff: "@counter".into(),
                });
                self.emit(Instruction::PrintFlush {
                    to: "message1".into(),
                });
                self.emit(Instruction::WaitForReset);
            }
            Instruction::Read {
                out_var,
                bank_id,
                addr,
            } => {
                self.raw
                    .push_str(&format!("read {out_var} {bank_id} {addr}\n"));
            }
            Instruction::Write {
                in_var,
                bank_id,
                addr,
            } => {
                self.raw
                    .push_str(&format!("write {in_var} {bank_id} {addr}\n"));
            }
            Instruction::Set { ident, value } => {
                self.raw.push_str(&format!("set {ident} {value}\n"));
            }
            Instruction::SetToTagAddr { .. } => {
                unreachable!("this should be converted into a Set instr before emission")
            }
            Instruction::Print { stuff } => {
                self.raw.push_str(&format!("print {stuff}\n"));
            }
            Instruction::PrintFlush { to } => {
                self.raw.push_str(&format!("printflush {to}\n"));
            }
            Instruction::Operation { op, out, a, b } => {
                self.raw
                    .push_str(&format!("op {} {out} {a} {b}\n", op.to_string()));
            }
            Instruction::Jump {
                addr,
                condition,
                args,
            } => {
                let addr = match addr {
                    MlogAddr::Raw(addr) => addr,
                    MlogAddr::Tag(..) => {
                        unreachable!("Tag addrs must be filtered out before emission")
                    }
                };
                self.raw.push_str(&if let Some((arg1, arg2)) = args {
                    format!("jump {addr} {c} {arg1} {arg2}\n", c = condition.to_string())
                } else {
                    format!("jump {addr} {c} null null\n", c = condition.to_string())
                })
            }
            Instruction::Comment(text) => {
                self.raw.push_str(&format!("# {text}\n"));
            }
            other => panic!("Unsuported instruction {other:#?}"),
        }
    }

    pub fn emit_many(&mut self, instrs: Vec<Instruction>) {
        for instr in instrs {
            self.emit(instr);
        }
    }
}

/// Note: for all of these, the input can be literals or variables
///
/// for colors, types are `int` | `ident`
///
/// for positions/degrees, values can be `int` | `float` | `ident`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DrawType {
    /// clears the display with a spacific color
    ///
    /// draw clear <r> <g> <b> (extra): 0 0 0
    Clear { r: String, g: String, b: String },
    /// Sets color for next drawing operations
    ///
    /// draw color <r> <g> <b> <a> (extra): 0 0
    SetColor {
        r: String,
        g: String,
        b: String,
        a: String,
    },
    /// The same as SetColor, but uses packed colors
    ///
    /// Packed color literals are written as hex codes with the % prefix, ex `%ff0000` would be red
    ///
    /// draw col <color> (extra): 0 0 0 0 0
    SetColorPacked { h: String },
    /// Sets line width for other draw instructions
    ///
    /// draw stroke <width> (extra): 0 0 0 0 0
    SetLineWidth { width: String },
    /// Draws a line from (x1, y1) to (x2, y2)
    ///
    /// draw line <x> <y> <x2> <y2> (extra): 0 0
    DrawLine {
        x1: String,
        y1: String,
        x2: String,
        y2: String,
    },
    /// Draws a rectangle with the bottom left corner at (x, y) and with a size of `width` and `height`
    ///
    /// draw rect <x> <y> <w> <h> (extra): 0 0
    DrawRect {
        x: String,
        y: String,
        w: String,
        h: String,
    },
    /// Draws the outline of a rectangle
    ///
    /// draw lineRect <x> <y> <w> <h> (extra): 0 0
    DrawOutlineRect {
        x: String,
        y: String,
        w: String,
        h: String,
    },
    /// Draws a regular polygon at (x, y) with s sides, radius ra, and rotation ro
    ///
    /// draw poly <x> <y> <s> <ra> <ro> (extra): 0
    DrawRegularPolygon {
        x: String,
        y: String,
        s: String,
        ra: String,
        ro: String,
    },
    /// Draws the outline of a regular polygon at (x, y) with s sides, radius ra, and rotation ro
    ///
    /// draw poly <x> <y> <s> <ra> <ro> (extra): 0
    DrawOutlineRegularPolygon {
        x: String,
        y: String,
        s: String,
        ra: String,
        ro: String,
    },
    /// Draws a triangle
    ///
    /// draw triangle x y x2 y2 x3 y3
    DrawTriangle {
        x1: String,
        y1: String,
        x2: String,
        y2: String,
        x3: String,
        y3: String,
    },
    /// Draws an image (must be an ingame thing, use the constant, eg `@router` to reference it)
    ///
    /// s = size, r = rotation
    ///
    /// draw image x y @copper s r (extra): 0
    DrawImage {
        x: String,
        y: String,
        id: String,
        s: String,
        r: String,
    },
}

/// op add result a b
/// op sub result a b
/// op mul result a b
/// op div result a b
/// op idiv result a b (floor division)
/// op mod result a b
/// op pow result a b
/// op equal result a b
/// op notEqual result a b
/// op land result a b (logical and)
/// op lessThan result a b
/// op lessThanEq result a b
/// op greaterThan result a b
/// op greaterThanEq result a b
/// op strictEqual result a b
/// op or result a b (bitwise, works as logical as well)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operation {
    Add,
    Sub,
    Mul,
    Div,
    IDiv,
    Mod,
    Pow,
    Eq,
    NotEq,
    LogicalAnd,
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    StrictEqual,
    BitwiseOr,
}

impl ToString for Operation {
    fn to_string(&self) -> String {
        use Operation::*;
        match self {
            Add => "add",
            Sub => "sub",
            Mul => "mul",
            Div => "div",
            IDiv => "idiv",
            Mod => "mod",
            Pow => "pow",
            Eq => "equal",
            NotEq => "notEqual",
            LogicalAnd => "land",
            LessThan => "lessThan",
            LessThanEq => "lessThanEq",
            GreaterThan => "greaterThan",
            GreaterThanEq => "greaterThanEq",
            StrictEqual => "strictEqual",
            BitwiseOr => "or",
        }
        .to_string()
    }
}

impl From<Op> for Operation {
    fn from(op: Op) -> Self {
        match op {
            Op::Add => Self::Add,
            Op::Sub => Self::Sub,
            Op::Div => Self::Div,
            Op::Mul => Self::Mul,
            Op::Mod => Self::Mod,
            Op::Eq => Self::Eq,
            Op::Gtn => Self::GreaterThan,
            Op::Ltn => Self::LessThan,
            Op::GtnEq => Self::GreaterThanEq,
            Op::LtnEq => Self::LessThanEq,
            Op::And => Self::LogicalAnd,
            Op::Or => Self::BitwiseOr,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MlogAddr {
    Raw(usize),
    Tag(Uuid),
}

// TODO convert information to a spreadsheet
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Instruction {
    Read {
        out_var: String,
        bank_id: String,
        addr: String,
    },
    /*
    class: io
    priority: high
    status: done
    mlog rep: `read <out_var> <mem_bank_id (ex: cell1)> <addr (max is 63 for cells and 511 for banks (?))>`
    */
    Write {
        in_var: String,
        bank_id: String,
        addr: String,
    },
    /*
    class: io
    priority: high
    status: done
    mlog rep `write <in_var> <mem_bank_id (ex: cell1)> <addr (max is 63 for cells and 511 for banks (?))>`
    */
    Draw(DrawType),
    /*
    class: graphics
    priority: mid
    status: partially implemented, no generation for the final instr tho
    */
    Print {
        stuff: String,
    },
    /*
    class: io, debug
    priority: VERY HIGH
    status: done
    mlog rep: print <stuff>
    */
    DrawFlush,
    /*
    class: graphics
    priority: mid,
    status: unimplemented
    */
    PrintFlush {
        to: String,
    },
    /*
    class: io, debug
    priority: very high
    status: done
    mlog rep: printflush <to>
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
    Set {
        ident: String,
        value: String,
    },
    /*
    class: basic operation
    priority: very very high
    status: done
    mlog rep: set <ident> <value>
    */
    SetToTagAddr {
        tag: Uuid,
        ident: String,
    },
    /*
    this sets `ident` to the real address of `tag`. replaced by Set before emission
    */
    Operation {
        op: Operation,
        out: String,
        a: String,
        b: String,
    },
    /*
    class: basic operation
    priority: very very high
    status: done
    mlog rep: op <op> <out> <a> <b>
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
        addr: MlogAddr,
        condition: JumpCondition,
        args: Option<(String, String)>,
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
    Trap,
    /*
    all this does is emit an error message with the current LOC, and then emit WaitForReset

    class: system operation, not in mindustry, requires prelude
    priority: high
    status: implemented

    args: none
    */
    Comment(String),
}

impl Instruction {
    /// number of "real" mlog instructions this is equivilant to.
    ///
    /// yes, it has come to this
    pub fn worth(&self) -> usize {
        match self {
            Self::Trap => 4,
            _ => 1,
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
