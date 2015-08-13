type 'a mult_tree = T of 'a * 'a mult_tree list;;

type ('a, 'b) labeled_graph = { nodes : 'a list;
                                edges : ('a * 'a * 'b)  list }

let g = { nodes = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'];
          edges = [('a', 'b', 5); ('a', 'd', 3); ('b', 'c', 2);
                   ('b', 'e', 4); ('c', 'e', 6); ('d', 'e', 7);
                   ('d', 'f', 4); ('d', 'g', 3); ('e', 'h', 5);
                   ('f', 'g', 4); ('g', 'h', 1)] };;

let ms_tree g = 
    let insert_node c t =
        let rec aux t = 
            match t with
            | T( w, cs ) when w = ( fst c ) -> if List.for_all ( fun a -> let T(nn,_) = a in ( snd c ) <> nn ) cs
                                                    then T( w, T( (snd c ), [] ) :: cs )
                                                    else T( w, cs )
            | T( w, cs ) -> T( w, List.map ( fun a -> aux a ) cs ) in
        aux t in
    let insert_queue n pq =
        let rec aux acc pq =
            match (n,pq) with
            | (_,[]) -> List.rev ( n::acc )
            | ((_,_,nc), ((_,_,c) as h) ::t ) when c < nc -> aux ( h::acc ) t
            | (nn,h::t ) -> ( List.rev ( h::nn::acc ) ) @ t in
        aux [] pq in
    let erase e lst = 
        let rec aux acc lst =
            match lst with
            | [] -> List.rev acc
            | h::t when h = e -> acc @ t
            | h::t -> aux ( h::acc ) t in
        aux [] lst in
    let has_node c t =
        let rec aux t =
            match t with
            | T( w, cs ) when w = c -> true
            | T( _, [] ) -> false
            | T( w, cs ) -> List.exists ( fun a -> aux a ) cs in
        aux t in
    let rec aux pq tc t = 
        if pq = []
            then ( t, tc )
            else ( let hpq = List.hd pq in
                   let tpq = List.tl pq in
                   let src,dst,cost = hpq in
                   if has_node dst t
                       then aux tpq tc t
                       else ( let nt = insert_node ( src, dst ) t in
                              let ne = List.filter ( fun a ->  let _,_,cost = a in 0 < cost ) ( List.map ( fun a -> match a with
                                                                                                     | ( nsrc,ndst,ncost ) when nsrc = dst && not ( has_node ndst nt ) -> ( nsrc, ndst, ncost + cost )
                                                                                                     | ( nsrc,ndst,ncost ) when ndst = dst && not ( has_node nsrc nt ) -> ( ndst, nsrc, ncost + cost )
                                                                                                     | ( nsrc,ndst,ncost ) -> ( nsrc, ndst, 1000000 ) ) g.edges )in
                              let npq = List.fold_left ( fun a b ->  insert_queue b a ) tpq ne in
                              aux npq cost nt ) ) in
    let org = T('.',[]) in
    List.fold_left ( fun a b -> if ( snd a ) < ( snd b ) then a else b ) (org,100000) (  List.map ( fun o -> aux [('.',o,0)] 0 org ) g.nodes );;
ms_tree g;;
