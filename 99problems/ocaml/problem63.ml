type 'a mult_tree = T of 'a * 'a mult_tree list;;

let t = T('a', [T('f',[T('g',[])]); T('c',[]);
            T('b',[T('d',[]); T('e',[])])]);;

let ipl t = 
    let rec aux n t = match t with
        | T(_,lst) -> n + List.fold_left (fun a b -> a + aux ( n + 1 ) b ) 0 lst in
    aux 0 t;;

ipl t ;;
