use std::{fs::OpenOptions, io::Read, path::Path};

use anyhow::Result;

pub fn load_text<P: AsRef<Path>>(path: P) -> Result<String> {
    let mut file = OpenOptions::new().read(true).write(false).open(path)?;
    let mut raw = String::new();
    file.read_to_string(&mut raw)?;
    Ok(raw)
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct TmpVarAllocator {
    current: u64,
}

impl TmpVarAllocator {
    pub const fn new() -> Self {
        Self { current: 0 }
    }

    pub fn next(&mut self) -> u64 {
        let r = self.current;
        self.current += 1;
        r
    }
}
