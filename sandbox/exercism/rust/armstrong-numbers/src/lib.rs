use std::convert::TryInto;

pub fn is_armstrong_number(num: u32) -> bool {
    let ns: Vec<u32> = num
        .to_string()
        .chars()
        .map(|c| c.to_digit(10).unwrap())
        .collect();

    ns.iter()
        .map(|x| x.pow(ns.len().try_into().unwrap()))
        .sum::<u32>()
        == num
}
