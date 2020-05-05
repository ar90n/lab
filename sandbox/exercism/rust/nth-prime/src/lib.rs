use std::convert::TryInto;

pub fn nth(n: u32) -> u32 {
    if n == 0 {
        return 2;
    }

    let mut primes: Vec<u32> = vec![2];
    std::ops::RangeFrom { start: 3 }
        .step_by(2.try_into().unwrap())
        .filter(|&x| {
            let ret = !primes.iter().any(|&y| x % y == 0);
            if ret {
                primes.push(x);
            }
            ret
        })
        .nth((n - 1).try_into().unwrap())
        .unwrap()
}
