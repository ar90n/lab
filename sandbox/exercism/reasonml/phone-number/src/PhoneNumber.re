let phoneNumber: string => option(string) = s => {
    let re = [%re "/^\\s*\\+?1?\\s*\\(?([2-9][0-9][0-9])\\)?[\\s,-.]*([0-9][0-9][0-9])[\\s,-.]*([0-9][0-9][0-9][0-9])[\\s]*$/"]

    switch(Js.Re.exec(s, re)) {
    | None => None
    | Some(result) => {
        result
        |> Js.Re.captures
        |> x => Array.sub(x, 1, Array.length(x) - 1)
        |> Array.map(Js.toOption)
        |> Array.fold_left((acc,x) => {
            switch((acc, x)) {
            | (None, _) => None
            | (_, None) => None
            | (Some(acc'), Some(x')) => Some(acc' ++ x')
            }
           },Some(""))
      }
    }
}
