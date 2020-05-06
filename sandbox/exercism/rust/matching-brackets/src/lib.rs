pub fn brackets_are_balanced(string: &str) -> bool {
    string
        .chars()
        .filter(|c| vec!['[', ']', '{', '}', '(', ')'].contains(c))
        .fold(Some(vec![]), |st: Option<Vec<char>>, c| match st {
            Some(mut s) => match c {
                '[' | '{' | '(' => {
                    s.push(c);
                    Some(s)
                }
                _ if !s.is_empty() => match (s.last().unwrap(), c) {
                    ('[', ']') | ('{', '}') | ('(', ')') => {
                        s.pop();
                        Some(s)
                    }
                    _ => None,
                },
                _ => None,
            },
            None => None,
        })
        .and_then(|st| if st.len() == 0 { Some(st) } else { None })
        .is_some()
}
