type 'a mult_tree = T of 'a * 'a mult_tree list;;

let t = T('a', [T('f',[T('g',[])]); T('c',[]);
            T('b',[T('d',[]); T('e',[])])]);;

let string_of_tree t =
    let rec aux t = match t with
        | T( w, lst ) -> ( String.make 1 w ) ^ ( List.fold_left ( fun a b -> a ^ ( aux b ) ) "" lst ) ^ "^" in
    aux t;;

let rec tree_of_substring t s i len =
    if i >= len || s.[i] = '^' then List.rev t, i + 1
            else (
                let sub, j = tree_of_substring [] s (i+1) len in
                tree_of_substring (T(s.[i], sub) ::t) s j len )
let tree_of_string s =
    match tree_of_substring [] s 0 (String.length s) with
      | [t], _ -> t
      | _ -> failwith "tree_of_string";;

tree_of_string ( string_of_tree t );;
