use std::convert::TryFrom;
use std::convert::TryInto;
use std::iter;

pub fn factors(n: u64) -> Vec<u64> {
    let m = (0..=n).skip_while(|x| x * x <= n).next().unwrap();
    let mut sieve: Vec<bool> = (0..=m).map(|x| x % 2 != 0).collect();
    sieve[0] = false;
    sieve[1] = false;
    sieve[2] = true;
    (3..=m).step_by(2).for_each(|v| {
        if sieve[usize::try_from(v).unwrap()] {
            (v..=m)
                .step_by(v.try_into().unwrap())
                .skip(1)
                .for_each(|x| sieve[usize::try_from(x).unwrap()] = false);
        }
    });

    let mut ret: Vec<u64> = sieve
        .iter()
        .enumerate()
        .filter_map(|(i, &f)| {
            if f {
                Some(u64::try_from(i).unwrap())
            } else {
                None
            }
        })
        .map(|p| {
            iter::repeat(p)
                .scan(n.clone(), |st, _| {
                    if *st % p == 0 {
                        *st /= p;
                        Some(p)
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>()
        })
        .flatten()
        .collect();
    let rem: u64 = ret.iter().fold(n, |r, &v| r / v);
    if rem != 1 {
        ret.push(rem);
    }
    ret
}
