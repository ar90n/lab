type 'a mult_tree = T of 'a * 'a mult_tree list;;

let t = T('a', [T('f',[T('g',[])]); T('c',[]);
            T('b',[T('d',[]); T('e',[])])]);;

let lispy t = 
    let rec aux s t = match t with
        | T(w,[]) -> s ^ " " ^ ( String.make 1 w )
        | T(w,lst) -> s ^ " (" ^ ( String.make 1 w ) ^ ( List.fold_left ( fun a b -> aux a b ) "" lst ) ^ ")"  in
    aux "" t;;

lispy t ;;
