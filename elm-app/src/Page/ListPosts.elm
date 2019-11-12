module Page.ListPosts exposing (..)

import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Post exposing (..)
import RemoteData as RD exposing (..)


type alias Model =
    { posts : WebData (List Post)
    , deleteError : Maybe String
    }


type Msg
    = FetchPosts
    | PostsReceived (WebData (List Post))
    | DeletePost PostId
    | PostDeleted (Result Http.Error String)


init : ( Model, Cmd Msg )
init =
    ( { posts = RD.Loading
      , deleteError = Nothing
      }
    , fetchPosts
    )


fetchPosts : Cmd Msg
fetchPosts =
    Http.get
        { url = "http://localhost:5019/posts/"
        , expect =
            Http.expectJson (RD.fromResult >> PostsReceived) postsDecoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchPosts ->
            ( { model | posts = RD.Loading }, fetchPosts )

        PostsReceived posts ->
            ( { model | posts = posts }, Cmd.none )

        DeletePost postId ->
            ( model, deletePost postId )

        PostDeleted (Ok _) ->
            ( model, fetchPosts )

        PostDeleted (Err err) ->
            ( { model | deleteError = Just (buildErrorMessage err) }, Cmd.none )


deletePost : PostId -> Cmd Msg
deletePost postId =
    Http.request
        { method = "PATCH"
        , headers = []
        , url = "http://localhost:5019/posts/" ++ Post.idToString postId
        , body = Http.emptyBody
        , expect = Http.expectString PostDeleted
        , timeout = Nothing
        , tracker = Nothing
        }



-- VIEWS


view : Model -> Html Msg
view model =
    E.layout [] <|
        E.el [] <|
            column []
                [ row [ padding 30 ] [ E.text "Posts" ]
                , row [ padding 20 ] [ E.link [] { url = "/posts/new", label = E.text "Create a New post" } ]
                , row [ padding 20 ] [ Input.button [] { onPress = Just FetchPosts, label = E.text "Refresh" } ]
                , viewDeleteError model.deleteError
                , viewPosts model.posts
                ]


emptyTable =
    { data = []
    , columns = []
    }


viewPosts : WebData (List Post) -> Element Msg
viewPosts posts =
    case posts of
        RD.NotAsked ->
            E.table [] emptyTable

        RD.Loading ->
            E.text "Loading..."

        RD.Success actualPosts ->
            E.table [ spacing 30 ]
                { data = actualPosts
                , columns =
                    [ { header = E.text "ID"
                      , width = fill
                      , view =
                            \p -> E.text (idToString p.id)
                      }
                    , { header = E.text "Title"
                      , width = fill
                      , view =
                            \p -> E.text p.title
                      }
                    , { header = E.text "Author"
                      , width = fill
                      , view =
                            \p ->
                                E.link []
                                    { url = p.authorUrl
                                    , label = E.text p.authorName
                                    }
                      }
                    , { header = E.text ""
                      , width = fill
                      , view =
                            \p ->
                                E.link []
                                    { url = "posts/" ++ Post.idToString p.id
                                    , label = E.text "Edit"
                                    }
                      }
                    , { header = E.text ""
                      , width = fill
                      , view =
                            \p ->
                                Input.button []
                                    { onPress = Just <| DeletePost p.id
                                    , label = E.text "Delete"
                                    }
                      }
                    ]
                }

        RD.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)


viewFetchError : String -> Element Msg
viewFetchError err =
    E.text <| "Error: " ++ err ++ "\nplease try again..."


viewDeleteError : Maybe String -> Element Msg
viewDeleteError maybeErr =
    case maybeErr of
        Just err ->
            row [] [ E.text <| "Error deleting the post, Error: " ++ err ]

        Nothing ->
            E.text ""
