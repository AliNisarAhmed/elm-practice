port module PortExamples exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Json.Decode exposing (Error(..), Value, decodeValue, string)


type alias Model =
    { dataFromJS : String
    , jsonError : Maybe Error
    }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SendDataToJS ]
            [ text "Send data to JS" ]
        , viewDataFromJSOrError model
        ]


viewDataFromJSOrError : Model -> Html Msg
viewDataFromJSOrError model =
    case model.jsonError of
        Just error ->
            viewError error

        Nothing ->
            viewDataFromJS model.dataFromJS


viewError : Error -> Html Msg
viewError jsonError =
    let
        errorHeading =
            "Could not receive data from JS"

        errorMessage =
            case jsonError of
                Failure message _ ->
                    message

                _ ->
                    "Error: Invalid JSON"
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


viewDataFromJS : String -> Html Msg
viewDataFromJS data =
    div []
        [ br [] []
        , strong [] [ text "Data received from JS: " ]
        , text data
        ]


type Msg
    = SendDataToJS
    | ReceiveDataFromJS Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendDataToJS ->
            ( model, sendDataToJS "Hello JavaScript" )

        ReceiveDataFromJS value ->
            case decodeValue string value of
                Ok data ->
                    ( { model | dataFromJS = data }, Cmd.none )

                Err error ->
                    ( { model | jsonError = Just error }, Cmd.none )


port sendDataToJS : String -> Cmd msg


port receiveData : (Value -> msg) -> Sub msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { dataFromJS = ""
    , jsonError = Nothing
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    receiveData ReceiveDataFromJS


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
