module Page.Repo exposing (Model, Msg, init, update, view)

import GitHub exposing (Issue, Repo)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http


type alias Model =
    { userName : String
    , projectName : String
    , state : State
    }


type State
    = Init
    | Loaded (List Issue)
    | Error Http.Error


init : String -> String -> ( Model, Cmd Msg )
init userName projectName =
    ( Model userName projectName Init
    , GitHub.getIssues
        GotIssues
        userName
        projectName
    )


type Msg
    = GotIssues (Result Http.Error (List Issue))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotIssues (OK issues) ->
            ( { model | state = Loaded issues }, Cmd.none )

        GotIssues (Err err) ->
            ( { model | state = Error err }, Cmd.none )


view : Model -> Html Msg
view { userName, projectName, state } =
    case state of
        Init ->
            text "Loading..."

        Loaded issues ->
            ul [] (List.map (viewIssue userName projectName) issues)

        Error e ->
            text (Debug.toString e)


viewIssue : String -> String -> Issue -> Html Msg
viewIssue userName projectName issue =
    li []
        [ span [] [ text ("[" ++ issue.state ++ "]") ]
        , a
            [ href (GitHub.issueUrl userName projectName issue.number)
            , target "_blank"
            ]
            [ text ("#" ++ String.fromInt issue.number), text issue.title ]
        ]
