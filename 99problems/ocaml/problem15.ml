let rec replicate l n =
    let rec ext c n = if n = 0 then [] else c :: ext c ( n - 1 ) in
    match l with
    | [] as l -> l
    | h :: r -> ( ext h n ) @ replicate r n;;

replicate ["a";"b";"c"] 3;;
