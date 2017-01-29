type 'a mult_tree = T of 'a * 'a mult_tree list;;

type 'a graph_term = { nodes : 'a list;  edges : ('a * 'a) list };;

let g = { nodes = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'];
          edges = [('a', 'b'); ('a', 'd'); ('b', 'c'); ('b', 'e');
                   ('c', 'e'); ('d', 'e'); ('d', 'f'); ('d', 'g');
                   ('e', 'h'); ('f', 'g'); ('g', 'h')] };;

let degree graph node =
    let rec aux deg edges = 
        match edges with
        | [] -> deg
        | h::t -> let inc = if ( fst h ) = node || ( snd h ) = node then 1 else 0 in aux ( deg + inc ) t in
    aux 0 graph.edges;;

let degree_sorted_nodes graph =
    List.sort ( fun a b -> ( fst b ) - ( fst a ) ) ( List.map ( fun a -> ( degree graph a, a )  ) graph.nodes );;

let paint graph =
    let neighbor_colors painted node =
        let neighbors = List.fold_left ( fun acc n -> if node = ( fst n ) then ( snd n )::acc
                                                      else if node = ( snd n ) then ( fst n )::acc
                                                      else acc ) [] graph.edges in
        let colors = List.fold_left ( fun acc n -> if List.exists ( fun a -> (fst n ) = a ) neighbors then (snd n)::acc else acc ) [] painted in
        List.sort ( fun a b -> a - b ) colors in
    let get_color colors =
        let rec aux lst =
            match lst with
            | [] -> 0
            | [c] -> c + 1
            | c1::c2::t when ( c1 + 1 ) < c2 -> ( c1 + 1 )
            | h::t -> aux t in
        aux colors in
    let rec aux painted nodes =
        match nodes with
        | [] -> painted
        | h::t -> let nc = neighbor_colors painted h in
                  let c = get_color nc in
                  aux ( (h,c)::painted ) t in
    aux [] graph.nodes;;
paint g;;
