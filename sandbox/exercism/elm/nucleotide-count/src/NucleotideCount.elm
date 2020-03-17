module NucleotideCount exposing (nucleotideCounts)


type alias NucleotideCounts =
    { a : Int
    , t : Int
    , c : Int
    , g : Int
    }


nucleotideCounts : String -> NucleotideCounts
nucleotideCounts sequence =
    String.toList sequence
        |> List.foldl
            (\c cnts ->
                case c of
                    'A' ->
                        { cnts | a = cnts.a + 1 }

                    'T' ->
                        { cnts | t = cnts.t + 1 }

                    'C' ->
                        { cnts | c = cnts.c + 1 }

                    'G' ->
                        { cnts | g = cnts.g + 1 }

                    _ ->
                        cnts
            )
            (NucleotideCounts 0 0 0 0)
