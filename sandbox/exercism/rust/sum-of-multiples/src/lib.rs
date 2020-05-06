pub fn sum_of_multiples(limit: u32, factors: &[u32]) -> u32 {
    (0..limit)
        .filter(|&x| {
            factors
                .iter()
                .any(|&y| if y == 0 { x == 0 } else { x % y == 0 })
        })
        .sum()
}
