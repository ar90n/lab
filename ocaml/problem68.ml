type 'a graph_term = { nodes : 'a list;  edges : ('a * 'a) list };;

let example_graph =
        { nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
          edges = ['h', 'g';  'k', 'f';  'f', 'b';  'f', 'c';  'c', 'b'] };;

let cycles g src = 
    let rev_dir e = 
        let rec aux acc lst = match lst with
            | [] -> acc
            | ( f, s ) as h ::t -> aux ( h :: ( s, f ) :: acc ) t in 
        aux [] e in
    let bi_edges = rev_dir g.edges in
    let erase e lst = 
        let rec aux acc lst = match lst with
            | [] -> lst
            | h::t when h = e -> acc @ t
            | h::t -> aux ( h::acc ) t in
        aux [] lst in
    let rec aux n v c =
        if 0 < List.length v && c = src then [ List.rev ( c::v ) ]
                                        else let ns = List.map snd ( List.filter ( fun a -> let fn,sn = a in fn = c && ( List.exists ( fun b -> sn = b ) n ) ) bi_edges ) in
                                             List.flatten ( List.map ( fun nei -> aux ( erase nei n ) ( c :: v ) nei ) ns ) in
    aux (src::g.nodes) [] src;;

cycles example_graph 'f' ;;
