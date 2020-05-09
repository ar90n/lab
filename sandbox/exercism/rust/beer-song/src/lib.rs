fn f(n: u32) -> (String, &'static str, &'static str) {
    match n {
        0 => (
            String::from("no more"),
            "Go to the store and buy some more",
            "s",
        ),
        1 => (n.to_string(), "Take it down and pass it around", ""),
        _ => (n.to_string(), "Take one down and pass it around", "s"),
    }
}

pub fn verse(n: u32) -> String {
    let m = (n + 99) % 100;
    let (n, ni, ns) = f(n);
    let (m, _, ms) = f(m);
    let mut ret = format!("{} bottle{} of beer on the wall, {} bottle{} of beer.\n{}, {} bottle{} of beer on the wall.\n", n, ns, n, ns, ni, m, ms);

    if let Some(r) = ret.get_mut(0..1) {
        r.make_ascii_uppercase()
    }

    ret
}

pub fn sing(start: u32, end: u32) -> String {
    (end..(start + 1))
        .rev()
        .map(|n| verse(n))
        .collect::<Vec<String>>()
        .join("\n")
}
