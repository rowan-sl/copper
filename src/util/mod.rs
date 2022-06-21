use std::{fs::OpenOptions, io::Read, path::Path};

use anyhow::Result;

pub fn load_text<P: AsRef<Path>>(path: P) -> Result<String> {
    let mut file = OpenOptions::new().read(true).write(false).open(path)?;
    let mut raw = String::new();
    file.read_to_string(&mut raw)?;
    Ok(raw)
}

/// self contains [at, len), returned contains [0, at)
#[inline(always)]
#[must_use]
#[allow(dead_code)]
pub fn take_n<T>(input: &mut Vec<T>, n: usize) -> Option<Vec<T>> {
    if n > input.len() {
        return None;
    }
    let mut new_input = input.split_off(n);
    std::mem::swap(&mut new_input, input);
    Some(new_input)
}

#[inline(always)]
#[must_use]
#[allow(dead_code)]
pub fn borrow_n<T>(input: &[T], n: usize) -> Option<&[T]> {
    if n > input.len() {
        return None;
    }
    Some(&input[..n])
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
