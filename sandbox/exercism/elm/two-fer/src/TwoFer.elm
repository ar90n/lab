module TwoFer exposing (twoFer)


twoFer : Maybe String -> String
twoFer maybeName =
    let
      name =
        case maybeName of
          Nothing -> "you"
          Just n -> n
    in
      "One for " ++ name ++ ", one for me."
