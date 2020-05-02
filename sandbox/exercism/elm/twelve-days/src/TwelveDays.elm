module TwelveDays exposing (recite)


recite : Int -> Int -> List String
recite start stop =
    let
        msg_lst : List ( String, String )
        msg_lst =
            [ ( "first", "a Partridge in a Pear Tree." )
            , ( "second", "two Turtle Doves" )
            , ( "third", "three French Hens" )
            , ( "fourth", "four Calling Birds" )
            , ( "fifth", "five Gold Rings" )
            , ( "sixth", "six Geese-a-Laying" )
            , ( "seventh", "seven Swans-a-Swimming" )
            , ( "eighth", "eight Maids-a-Milking" )
            , ( "ninth", "nine Ladies Dancing" )
            , ( "tenth", "ten Lords-a-Leaping" )
            , ( "eleventh", "eleven Pipers Piping" )
            , ( "twelfth", "twelve Drummers Drumming" )
            ]

        msg : String
        msg =
            "On the {} day of Christmas my true love gave to me: "

        aux acc ss =
            let
                del =
                    if List.isEmpty acc then
                        ""

                    else if List.length acc == 1 then
                        ", and "

                    else
                        ", "
            in
            case ss of
                ( d, s ) :: [] ->
                    String.replace "{}" d msg :: (s ++ del) :: acc

                ( _, s ) :: tl ->
                    aux ((s ++ del) :: acc) tl

                _ ->
                    acc
    in
    List.range start stop
        |> List.map (\x -> List.take x msg_lst)
        |> List.map (aux [] >> String.concat)
