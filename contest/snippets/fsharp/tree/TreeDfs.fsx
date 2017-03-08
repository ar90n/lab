type ContStep<'a> =
    | Finished
    | Step of 'a * (unit -> ContStep<'a>)

let TreeDfs (select : ('a bintree -> (unit -> ContStep<'a>) -> ContStep<'a>) -> 'a -> 'a bintree -> 'a bintree -> (unit -> ContStep<'a>) -> ContStep<'a>) (tree : 'a bintree) : 'a list =
    let rec traverse (tree : 'a bintree) (cont : unit -> ContStep<'a>) =
        match tree with
        | Empty -> cont()
        | Node( x, l, r ) as n ->  select traverse x l r cont
    let rec doit res step : 'a list =
        match step with
        | Finished -> res
        | Step(x, cont) -> doit (x::res) (cont())
    doit [] (traverse tree (fun () -> Finished)) |> List.rev

let PostDfs = TreeDfs (fun f x l r cont -> f l (fun () -> f r (fun () -> Step( x,cont))) )
let InDfs = TreeDfs (fun f x l r cont -> f l (fun () -> Step( x, (fun () -> f r cont))))
let PreDfs = TreeDfs (fun f x l r cont -> Step (x, (fun () -> f l (fun () -> f r cont))))
