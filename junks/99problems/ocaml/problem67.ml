type 'a graph_term = { nodes : 'a list;  edges : ('a * 'a) list };;

let example_graph =
        { nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
          edges = ['h', 'g';  'k', 'f';  'f', 'b';  'f', 'c';  'c', 'b'] };;

let paths g src dst =
    let erase e lst = 
        List.filter ( fun a -> a <> e ) lst in
    let rec aux e v c =
        if c = dst then [ List.rev ( c::v ) ]
                   else let ns = List.filter ( fun a -> let fn,_ = a in ( fn = c ) ) e in
                        List.flatten ( List.map ( fun nei -> aux ( erase nei e ) ( c :: v ) ( snd nei ) ) ns ) in
    aux g.edges [] src;;

paths example_graph 'f' 'b';;
