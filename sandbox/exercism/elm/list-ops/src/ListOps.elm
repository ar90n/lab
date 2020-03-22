module ListOps exposing
    ( append
    , concat
    , filter
    , foldl
    , foldr
    , length
    , map
    , reverse
    )


rmap : (a -> b) -> List a -> List b
rmap f list =
    let
        aux rem acc =
            case rem of
                hd :: tl ->
                    aux tl (f hd :: acc)

                _ ->
                    acc
    in
    aux list []


length : List a -> Int
length list =
    foldl (\_ l -> l + 1) 0 list


reverse : List a -> List a
reverse list =
    rmap identity list


foldl : (a -> b -> b) -> b -> List a -> b
foldl f acc list =
    let
        aux rem res =
            case rem of
                hd :: tl ->
                    aux tl (f hd res)

                _ ->
                    res
    in
    aux list acc


foldr : (a -> b -> b) -> b -> List a -> b
foldr f acc list =
    reverse list |> foldl f acc


map : (a -> b) -> List a -> List b
map f list =
    rmap f list |> reverse


filter : (a -> Bool) -> List a -> List a
filter f list =
    let
        aux acc rem =
            case rem of
                hd :: tl ->
                    aux
                        (if f hd then
                            hd :: acc

                         else
                            acc
                        )
                        tl

                _ ->
                    acc
    in
    list |> aux [] |> reverse


append : List a -> List a -> List a
append xs ys =
    foldr (\e acc -> e :: acc) ys xs


concat : List (List a) -> List a
concat list =
    foldr append [] list
