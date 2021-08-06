use std::fmt::Debug;

use rand::Rng;

fn main() {
    let mut rng = rand::thread_rng();
    let mut vs0 = (0..8).into_iter().map(|_| rng.gen()).collect::<Vec<u8>>();
    let mut vs1 = vs0.clone();
    //vs0.sort();
    //dbg!(&vs0);
    qsort(&mut vs1);
    dbg!(&vs1);
}

fn qsort<T: Ord + Copy + Debug>(vs: &mut [T]) {
    let mut n = vs.len();

    let mut stack = vec![(0 as usize, n - 1)];
    while 0 < n {
        let (beg, end) = stack.pop().unwrap();
        n -= 1;
        if beg < end{
            let pivot_index = partition(&mut vs[beg..=end]) + beg;
            if beg < pivot_index {
                stack.push((beg, pivot_index - 1));
            }
            if pivot_index < end {
                stack.push((pivot_index + 1, end));
            }
        }
    }
}

fn partition<T: Ord + Copy + Debug>(vs: &mut [T]) -> usize {
    let pivot = vs[0];
    let mut li = 1;
    let mut ri = vs.len() - 1;
    while li < ri {
        while vs[li] < pivot && li < ri {
            li += 1;
        }

        while pivot < vs[ri] && li < ri {
            ri -= 1;
        }

        if vs[ri] < vs[li] {
            let tmp = vs[li];
            vs[li] = vs[ri];
            vs[ri] = tmp;
        }
    }

    if pivot < vs[li] {
        li -= 1;
    }
    let tmp = vs[li];
    vs[li] = pivot;
    vs[0] = tmp;
    return li;
}

#[test]
fn test_partition() {
    let mut vs = [149, 146, 124, 6, 123, 9, 145];
    dbg!(&vs);
    let p = partition(&mut vs);
    dbg!(&p, &vs);
}
