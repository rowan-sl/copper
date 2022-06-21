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
                self.raw.push_str("jump 26 always 0 0");
                self.raw.push('\n');
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
            other => warn!("Unsuported instruction {other:#?}"),
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

// TODO convert information to a spreadsheet
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Instruction {
    Read {
        out_var: String,
        bank_id: String,
        addr: usize,
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
        addr: usize,
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
