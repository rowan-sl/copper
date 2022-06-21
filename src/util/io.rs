use std::{fs::OpenOptions, io::Read, path::Path};

use anyhow::Result;

pub fn load_text<P: AsRef<Path>>(path: P) -> Result<String> {
    let mut file = OpenOptions::new().read(true).write(false).open(path)?;
    let mut raw = String::new();
    file.read_to_string(&mut raw)?;
    Ok(raw)
}
