type 'a graph_term = { nodes : 'a list;  edges : ('a * 'a) list };;

type 'a graph =
    | L of ( 'a * 'a ) list
    | G of 'a graph_term;;

let example_graph =
        { nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
          edges = ['h', 'g';  'k', 'f';  'f', 'b';  'f', 'c';  'c', 'b'] };;

let convert g =
    let cmp a b = a = b in
    let aux b lst = 
        let f = fst b in
        let s = snd b in
        let tmp_lst = if List.exists ( cmp f ) lst then lst else ( f :: lst ) in
        if List.exists ( cmp s ) tmp_lst then tmp_lst else ( s :: tmp_lst ) in
    match g with
    | L lst -> G {nodes = ( ( List.fold_left ( fun a b -> aux b a ) [] lst ) ); edges = lst }
    | G { nodes = n; edges = e } -> L e ;;

convert ( convert ( G example_graph ) );;
