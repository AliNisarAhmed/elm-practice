module Page.EditPost exposing (..)

import Browser.Navigation as Nav
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Error exposing (buildErrorMessage, viewError)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Encode as Encode
import Post exposing (Post, PostId, postDecoder, postEncoder)
import RemoteData as RD
import Route


type alias Model =
    { navKey : Nav.Key
    , post : RD.WebData Post
    , saveError : Maybe String
    }


init : PostId -> Nav.Key -> ( Model, Cmd Msg )
init postId navKey =
    ( initialModel navKey, fetchPost postId )


initialModel : Nav.Key -> Model
initialModel navKey =
    { navKey = navKey
    , post = RD.Loading
    , saveError = Nothing
    }


fetchPost : PostId -> Cmd Msg
fetchPost postId =
    Http.get
        { url = "http://localhost:5019/posts/" ++ Post.idToString postId
        , expect = postDecoder |> Http.expectJson (RD.fromResult >> PostReceived)
        }


type Msg
    = PostReceived (RD.WebData Post)
    | UpdateTitle String
    | UpdateAuthorName String
    | UpdateAuthorUrl String
    | SavePost
    | PostSaved (Result Http.Error Post)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PostReceived post ->
            ( { model | post = post }, Cmd.none )

        UpdateTitle title ->
            let
                updatedTitle =
                    RD.map (\p -> { p | title = title }) model.post
            in
            ( { model | post = updatedTitle }, Cmd.none )

        UpdateAuthorName newName ->
            let
                updatedName =
                    RD.map (\p -> { p | authorName = newName }) model.post
            in
            ( { model | post = updatedName }, Cmd.none )

        UpdateAuthorUrl newUrl ->
            let
                updatedUrl =
                    RD.map (\p -> { p | authorUrl = newUrl }) model.post
            in
            ( { model | post = updatedUrl }, Cmd.none )

        SavePost ->
            ( model, savePost model.post )

        PostSaved (Ok postData) ->
            let
                post =
                    RD.succeed postData
            in
            ( { model | post = post }
            , Route.pushUrl Route.Posts model.navKey
            )

        PostSaved (Err err) ->
            ( { model | saveError = Just <| buildErrorMessage err }, Cmd.none )


savePost : RD.WebData Post -> Cmd Msg
savePost post =
    case post of
        RD.Success postData ->
            let
                postUrl =
                    "http://localhost:5019/posts/" ++ Post.idToString postData.id
            in
            Http.request
                { method = "POST"
                , headers = []
                , url = postUrl
                , body = Http.jsonBody (postEncoder postData)
                , expect = Http.expectJson PostSaved postDecoder
                , timeout = Nothing
                , tracker = Nothing
                }

        _ ->
            Cmd.none



-- VIEWS


view : Model -> Html Msg
view model =
    E.layout [] <|
        E.el [] <|
            column [] <|
                [ row [] [ E.text "Edit Post" ]
                , viewError model.saveError
                ]
                    ++ viewPost model.post


viewPost : RD.WebData Post -> List (E.Element Msg)
viewPost post =
    case post of
        RD.NotAsked ->
            [ row [] [ E.text "" ] ]

        RD.Loading ->
            [ row [] [ E.text "Loading Post..." ] ]

        RD.Success postData ->
            editForm postData

        RD.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)


viewFetchError : String -> List (Element Msg)
viewFetchError err =
    let
        errorHeading =
            "Could not fetch the post at this time"
    in
    [ E.text <| errorHeading
    , E.text <| "Error: " ++ err
    ]


editForm : Post -> List (E.Element Msg)
editForm post =
    [ row []
        [ Input.text []
            { onChange = UpdateTitle
            , text = post.title
            , placeholder = Just <| Input.placeholder [] <| E.text "Edit Title"
            , label = Input.labelAbove [] <| E.text "Edit Title"
            }
        ]
    , row []
        [ Input.text []
            { onChange = UpdateAuthorName
            , text = post.authorName
            , placeholder = Just <| Input.placeholder [] <| E.text "Edit Author Name"
            , label = Input.labelAbove [] <| E.text "Edit Author Name"
            }
        ]
    , row []
        [ Input.text []
            { onChange = UpdateAuthorUrl
            , text = post.authorUrl
            , placeholder = Just <| Input.placeholder [] <| E.text "Edit Author Url"
            , label = Input.labelAbove [] <| E.text "Edit Author Url"
            }
        ]
    , row []
        [ Input.button []
            { onPress = Just SavePost
            , label = E.text "Submit"
            }
        ]
    ]
