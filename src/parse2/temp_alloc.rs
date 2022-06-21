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
