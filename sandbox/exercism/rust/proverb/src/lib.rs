pub fn build_proverb(list: &[&str]) -> String {
    if list.is_empty() {
        return String::new();
    }

    let mut it = list.iter();
    let last = format!("And all for the want of a {}.", it.next().unwrap());
    let mut res = it
        .zip(list.iter())
        .map(|(h, t)| format!("For want of a {} the {} was lost.", t, h))
        .collect::<Vec<String>>();
    res.push(last);
    res.join("\n")
}
