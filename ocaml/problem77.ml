let queens_positions n =
    let gen_range n =
        let rec aux acc n = 
            if n = 0 then acc else aux (n::acc) (n-1) in
        aux [] n in
    let range =  gen_range n in
    let calc_diag x y = x + y in
    let rec aux acc_ver acc_up_diag acc_down_diag x =
        if x = 0
        then [acc_ver]
        else List.flatten ( List.map ( fun y -> aux ( y::acc_ver ) ( ( x + y )::acc_up_diag ) ( ( x - y )::acc_down_diag ) ( x - 1 ) ) ( List.filter ( fun y -> not ( List.mem y acc_ver || List.mem ( x + y ) acc_up_diag || List.mem ( x - y ) acc_down_diag ) ) range ) ) in
    aux  [] [] [] n;;

queens_positions 4;;
List.length ( queens_positions 8 );;

