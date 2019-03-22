let raindrops: int => string = n => {
    let ret = [
        if(n mod 3 == 0) "Pling" else "",
        if(n mod 5 == 0) "Plang" else "",
        if(n mod 7 == 0) "Plong" else "",
    ] |> List.fold_left((acc, s) => acc ++ s, "")
    if(ret == "") string_of_int(n) else ret
};
