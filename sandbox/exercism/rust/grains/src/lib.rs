pub fn square(s: u32) -> u64 {
    if s <= 0 || 64 < s {
        panic!("Square must be between 1 and 64")
    }
    1u64 << (s - 1)
}

pub fn total() -> u64 {
    (1..=64).map(square).sum()
}
