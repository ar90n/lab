t ype 'a mult_tree = T of 'a * 'a mult_tree list;;

type 'a graph_term = { nodes : 'a list;  edges : ('a * 'a) list };;

let g = { nodes = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'];
          edges = [('a', 'b'); ('a', 'd'); ('b', 'c'); ('b', 'e');
                   ('c', 'e'); ('d', 'e'); ('d', 'f'); ('d', 'g');
                   ('e', 'h'); ('f', 'g'); ('g', 'h')] };;

let s_tree g = 
    let erase_node e lst = 
        let rec aux acc lst =
            match lst with
            | [] -> List.rev acc
            | h::t when h = e -> acc @ t
            | h::t -> aux ( h::acc ) t in
        aux [] lst in
    let erase_edge e lst = 
        let rec aux acc lst =
            match lst with
            | [] -> List.rev acc
            | h::t when h = e -> acc @ t
            | h::t when h = ( snd e, fst e ) -> acc @ t
            | h::t -> aux ( h::acc ) t in
        aux [] lst in
    let insert_node c t =
        let rec aux t = 
            match t with
            | T( w, cs ) when w = ( fst c ) -> if List.for_all ( fun a -> let T(nn,_) = a in ( snd c ) <> nn ) cs
            then T( w, T( (snd c ), [] ) :: cs )
            else T( w, cs )
            | T( w, cs ) -> T( w, List.map ( fun a -> aux a ) cs ) in
        aux t in
    let has_node c t =
        let rec aux t =
            match t with
            | T( w, cs ) when w = c -> true
            | T( _, [] ) -> false
            | T( w, cs ) -> List.exists ( fun a -> aux a ) cs in
        aux t in
    let find_edge nt ns es =
        let rec aux es =
            match es with
            | [] -> None
            | h::t -> if ( has_node ( fst h ) nt ) && ( List.exists ( fun a -> a = ( snd h ) ) ns )
                        then Some ( h )
                        else ( if  ( has_node ( snd h ) nt ) && ( List.exists ( fun a -> a = ( fst h ) ) ns )
                        then Some ( ( snd h, fst h ) )
                        else aux t ) in
        aux es in
    let rec aux n e t c =
        let nn = erase_node ( snd c ) n in 
        let nt = insert_node c t in
        let rec doit nt nn ee =
            match find_edge nt nn ee with
            | None -> if nn = [] then [nt] else [T('.',[])]
            | Some( ce ) -> let ne = erase_edge ce ee in List.filter ( fun a -> a <> T('.',[]) ) ( ( aux nn ne nt ce ) @ (  doit nt nn ne ) ) in
        doit nt nn e in
    List.map ( fun a -> let T( _, r ) = a in r ) ( aux g.nodes g.edges (T('.',[])) ( '.', 'a' ) );;
s_tree g;;
