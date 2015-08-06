type 'a mult_tree = T of 'a * 'a mult_tree list;;

let t = T('a', [T('f',[T('g',[])]); T('c',[]);
            T('b',[T('d',[]); T('e',[])])]);;

let bottom_up t = 
    let rec aux s t = match t with
        | T(w,lst) -> s @ ( List.fold_left ( fun a b -> aux a b ) [] lst ) @ [w]  in
    aux [] t;;

bottom_up t ;;
