let accumulate: ('a => 'b, list('a)) => list('b) = (f, lst) => {
    let rec aux = lst => switch(lst) {
    | [] => [] 
    | [head, ...tail] => [f(head), ...aux(tail)]
    }
    aux(lst)
}
