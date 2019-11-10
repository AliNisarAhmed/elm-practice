module DecodingJson exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (optional, optionalAt, required, requiredAt)
import RemoteData as RD exposing (..)


type alias Post =
    { id : Int
    , title : String
    , authorName : String
    , authorUrl : String
    }


type alias Model =
    { posts : WebData (List Post)
    , errorMessage : Maybe String
    }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick FetchPosts ]
            [ text "Refresh posts" ]
        , viewPostOrError model
        ]


viewPostOrError : Model -> Html Msg
viewPostOrError model =
    case model.posts of
        RD.NotAsked ->
            text ""

        RD.Loading ->
            h3 [] [ text "Loading..." ]

        RD.Success posts ->
            viewPosts posts

        RD.Failure httpError ->
            viewError (buildErrorMessage httpError)


viewError : String -> Html Msg
viewError e =
    div []
        [ h3 [] [ text "Coudn't fetch data at this time" ]
        , text ("Error: " ++ e)
        ]


viewPosts : List Post -> Html Msg
viewPosts posts =
    div []
        [ h3 [] [ text "Posts" ]
        , table []
            (viewTableHeader :: List.map viewPost posts)
        ]


viewTableHeader : Html Msg
viewTableHeader =
    tr []
        [ th []
            [ text "ID" ]
        , th [] [ text "Title" ]
        , th [] [ text "Author" ]
        ]


viewPost : Post -> Html Msg
viewPost post =
    tr []
        [ td []
            [ text (String.fromInt post.id)
            , td [] [ text post.title ]
            , td []
                [ a [ href post.authorUrl ] [ text post.authorName ]
                ]
            ]
        ]


type Msg
    = FetchPosts
    | DataReceived (RD.WebData (List Post))


postDecoder : Decoder Post
postDecoder =
    Decode.succeed Post
        |> required "id" int
        |> required "title" string
        |> required "authorName" string
        |> required "authorUrl" string


fetchPosts : Cmd Msg
fetchPosts =
    Http.get
        { url = "http://localhost:5019/posts"
        , expect =
            Http.expectJson (RD.fromResult >> DataReceived) (list postDecoder)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchPosts ->
            ( { model | posts = RD.Loading }, fetchPosts )

        DataReceived res ->
            ( { model | posts = res }, Cmd.none )


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl m ->
            m

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network Unavailable"

        Http.BadStatus statusCode ->
            "Request failed with code: " ++ String.fromInt statusCode

        Http.BadBody m ->
            m


init : () -> ( Model, Cmd Msg )
init _ =
    ( { posts = RD.Loading, errorMessage = Nothing }, fetchPosts )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
