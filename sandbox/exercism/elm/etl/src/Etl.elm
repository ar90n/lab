module Etl exposing (transform)

import Dict exposing (Dict)


transform : Dict Int (List String) -> Dict String Int
transform input =
    input
    |> Dict.toList
    |> List.map (\(w, lst) -> List.map (\c -> (String.toLower c, w)) lst)
    |> List.foldl (++) []
    |> Dict.fromList
