module RNATranscription exposing (toRNA)


toRNA : String -> Result Char String
toRNA dna =
    let
        aux c acc =
            case acc of
                Err v ->
                    Err v

                Ok v ->
                    case c of
                        'G' ->
                            Ok (v ++ "C")

                        'C' ->
                            Ok (v ++ "G")

                        'T' ->
                            Ok (v ++ "A")

                        'A' ->
                            Ok (v ++ "U")

                        _ ->
                            Err c
    in
    dna |> String.foldl aux (Ok "")
