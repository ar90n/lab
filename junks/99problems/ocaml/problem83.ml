module IntSet = Set.Make( struct
                            type t = int
                            let compare i j = i - j
                          end );;

let problem = "..48...17" ^
              "67.9....." ^
              "5.8.3...4" ^
              "3..74.1.." ^
              ".69...78." ^
              "..1.69..5" ^
              "1...8.3.6" ^
              ".....6.91" ^
              "24...15..";;

let problem_size = 9;;

let gen_problem_array n problem =
    if String.length problem <> 81
    then raise (Failure "invalid problem" )
    else begin
         let field = Array.make_matrix n n 0 in
         for i = 0 to n - 1 do
             for j = 0 to n - 1 do
                 let c = problem.[ i * n + j ] in
                 field.(i).(j) <- if c = '.' then 0 else ( int_of_char c ) - ( int_of_char '0' )
             done;
         done;
         field
    end in

let get_range n m = 
    let rec aux acc n = 
        if n < m then acc
                 else aux ( n::acc ) ( n - 1 ) in
    aux [] n in

let get_range_set n m = 
    let rec aux acc n = 
        if n < m then acc
                 else aux ( IntSet.add n acc ) ( n - 1 ) in
    aux IntSet.empty n in

let get_valid_numbers_by_hor field y x =
    let rec aux acc x =
        if x = -1
        then acc
        else begin
            if field.(y).(x) = 0
            then aux acc ( x - 1 )
            else aux ( IntSet.add field.(y).(x) acc ) ( x - 1 )
        end in
    let used_numbers_set = aux IntSet.empty ( (Array.length field.(y)) - 1 ) in
    let range_set = ( get_range_set ( Array.length field ) 1 ) in
    IntSet.diff range_set used_numbers_set in

let get_valid_numbers_by_ver field y x =
    let rec aux acc y =
        if y = -1
        then acc
        else begin
            if field.(y).(x) = 0
            then aux acc ( y - 1 )
            else aux ( IntSet.add field.(y).(x) acc ) ( y - 1 )
        end in
    let used_numbers_set = aux IntSet.empty ( (Array.length field.(y)) - 1 ) in
    let range_set = ( get_range_set ( Array.length field ) 1 ) in
    IntSet.diff range_set used_numbers_set in

let get_valid_numbers_by_square field y x =
    let yy = 3 * ( y / 3 ) in
    let xx = 3 * ( x / 3 ) in
    let ys = get_range ( yy + 2 ) yy in
    let xs = get_range ( xx + 2 ) xx in
    let pos = List.flatten ( List.fold_left ( fun s a -> ( List.map ( fun b -> ( b, a ) ) xs ) :: s  ) [] ys ) in
    let used_numbers_set = List.fold_right ( fun v s -> if v <> 0 then IntSet.add v s else s ) ( List.map ( fun p -> field.(snd p).(fst p) ) pos ) IntSet.empty in
    let range_set = ( get_range_set ( Array.length field ) 1 ) in
    IntSet.diff range_set used_numbers_set in

let get_valid_number field y x =
    let valid_number_by_squqre = get_valid_numbers_by_square field y x in
    let valid_number_by_ver = get_valid_numbers_by_ver field y x in
    let valid_number_by_hor = get_valid_numbers_by_hor field y x in
    IntSet.inter ( IntSet.inter valid_number_by_squqre valid_number_by_ver ) valid_number_by_hor in

let solve_sudoku field =
    let n = Array.length field in
    let rec pre_solve field =
        let c = ref 0 in
        for y = 0 to n - 1 do
            for x = 0 to n - 1 do
                if field.(y).(x) = 0 then begin
                    let n = get_valid_number field y x in
                    if IntSet.cardinal n = 1 then begin
                        field.(y).(x) <- IntSet.choose n;
                        c := !c + 1;
                    end
                end 
            done;
        done;
        if !c <> 0 then pre_solve field else field in
    let array_to_str field =
        let s = ref "" in
        for y = 0 to n - 1 do
            for x = 0 to n - 1 do
                s := !s ^ ( string_of_int field.(y).(x) )
            done;
            s := !s ^ "\n"
        done;
        !s in
    array_to_str ( pre_solve field ) in

let field = gen_problem_array problem_size problem in
print_string ( solve_sudoku field );;
