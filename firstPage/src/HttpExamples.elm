module HttpExamples exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, Error(..), decodeString, list, string)


type alias Model =
    { nickNames : List String
    , error : Maybe String
    }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SendHttpRequest ] [ text "Get Data from server" ]
        , h3 [] [ text "Old School Main Characters" ]
        , viewErrorOrNames model
        ]


viewNickname : String -> Html Msg
viewNickname str =
    li [] [ text str ]


viewErrorOrNames : Model -> Html Msg
viewErrorOrNames model =
    case model.error of
        Just str ->
            h2 [] [ text str ]

        Nothing ->
            ul [] (List.map viewNickname model.nickNames)


type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error (List String))


url : String
url =
    "http://localhost:5019/nicknames"


getNicknames : Cmd Msg
getNicknames =
    Http.get
        { url = url
        , expect = Http.expectJson DataReceived nicknameDecoder
        }


nicknameDecoder : Decoder (List String)
nicknameDecoder =
    list string


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( model, getNicknames )

        DataReceived (Ok res) ->
            ( { model | nickNames = res }, Cmd.none )

        DataReceived (Err err) ->
            ( { model | error = Just <| buildErrorMessage err }, Cmd.none )


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl m ->
            m

        Http.Timeout ->
            "Taking too long, please try again"

        Http.NetworkError ->
            "Unable to reach server"

        Http.BadStatus stat ->
            "Request failed with status: " ++ String.fromInt stat

        Http.BadBody m ->
            m


getNames : String -> List String
getNames =
    String.split ","


init : () -> ( Model, Cmd Msg )
init _ =
    ( { nickNames = []
      , error = Nothing
      }
    , Cmd.none
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
