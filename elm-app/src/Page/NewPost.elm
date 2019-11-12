module Page.NewPost exposing (..)

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
import Post exposing (Post, PostId, newPostEncoder, postDecoder)
import RemoteData as RD
import Route


type alias Model =
    { navKey : Nav.Key
    , title : String
    , authorName : String
    , authorUrl : String
    , creationError : Maybe String
    }


type Msg
    = UpdateTitle String
    | UpdateAuthorName String
    | UpdateAuthorUrl String
    | SubmitNewPost
    | PostCreated (Result Http.Error Post)


init : Nav.Key -> ( Model, Cmd Msg )
init navKey =
    ( initialModel navKey, Cmd.none )


initialModel : Nav.Key -> Model
initialModel navKey =
    { navKey = navKey
    , title = ""
    , authorName = ""
    , authorUrl = ""
    , creationError = Nothing
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTitle title ->
            ( { model | title = title }
            , Cmd.none
            )

        UpdateAuthorName name ->
            ( { model | authorName = name }
            , Cmd.none
            )

        UpdateAuthorUrl url ->
            ( { model
                | authorUrl = url
              }
            , Cmd.none
            )

        SubmitNewPost ->
            ( model
            , submitNewPost model
            )

        PostCreated (Ok postData) ->
            ( model
            , Route.pushUrl Route.Posts model.navKey
            )

        PostCreated (Err err) ->
            ( { model | creationError = Just <| buildErrorMessage err }
            , Cmd.none
            )



-- COMMANDS


submitNewPost : Model -> Cmd Msg
submitNewPost model =
    Http.post
        { url = "http://localhost:5019/posts"
        , body = Http.jsonBody (newPostEncoder model)
        , expect = Http.expectJson PostCreated postDecoder
        }



-- VIEWS


view : Model -> Html Msg
view model =
    E.layout [ padding 40 ] <|
        E.column []
            [ row [] [ E.text "Create New Post" ]
            , row [] [ viewError model.creationError ]
            , newPostForm model
            ]


newPostForm : Model -> Element Msg
newPostForm model =
    column [ padding 20 ]
        [ row []
            [ Input.text [ spacing 20 ]
                { onChange = UpdateTitle
                , text = model.title
                , placeholder = Just <| Input.placeholder [] <| E.text "Enter a title..."
                , label = Input.labelAbove [] <| E.text "Title"
                }
            ]
        , row []
            [ Input.text [ spacing 20 ]
                { onChange = UpdateAuthorName
                , text = model.authorName
                , placeholder = Just <| Input.placeholder [] <| E.text "Enter author's name..."
                , label = Input.labelAbove [] <| E.text "Author Name"
                }
            ]
        , row []
            [ Input.text [ spacing 20 ]
                { onChange = UpdateAuthorUrl
                , text = model.authorUrl
                , placeholder = Just <| Input.placeholder [] <| E.text "Enter author's url..."
                , label = Input.labelAbove [] <| E.text "Author Url"
                }
            ]
        , row []
            [ Input.button []
                { onPress = Just SubmitNewPost
                , label = E.text "Submit"
                }
            ]
        ]
