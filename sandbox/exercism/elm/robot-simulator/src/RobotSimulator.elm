module RobotSimulator exposing
    ( Bearing(..)
    , Robot
    , advance
    , defaultRobot
    , simulate
    , turnLeft
    , turnRight
    )


type Bearing
    = North
    | East
    | South
    | West


type alias Robot =
    { bearing : Bearing
    , coordinates : { x : Int, y : Int }
    }


defaultRobot : Robot
defaultRobot =
    { bearing = North
    , coordinates = { x = 0, y = 0 }
    }


turnRight : Robot -> Robot
turnRight robot =
    case robot.bearing of
        North ->
            { robot | bearing = East }

        East ->
            { robot | bearing = South }

        South ->
            { robot | bearing = West }

        West ->
            { robot | bearing = North }


turnLeft : Robot -> Robot
turnLeft robot =
    robot |> (turnRight >> turnRight >> turnRight)


advance : Robot -> Robot
advance robot =
    let
        coordinates =
            robot.coordinates

        new_coordinates =
            case robot.bearing of
                North ->
                    { coordinates | y = coordinates.y + 1 }

                East ->
                    { coordinates | x = coordinates.x + 1 }

                South ->
                    { coordinates | y = coordinates.y - 1 }

                West ->
                    { coordinates | x = coordinates.x - 1 }
    in
    { robot | coordinates = new_coordinates }


simulate : String -> Robot -> Robot
simulate directions robot =
    let
        aux d r =
            case d of
                'R' ->
                    turnRight r

                'L' ->
                    turnLeft r

                'A' ->
                    advance r

                _ ->
                    r
    in
    String.toList directions |> (List.foldl aux robot)
