module Post exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Url.Parser exposing (Parser, custom)


type alias Post =
    { id : PostId
    , title : String
    , authorName : String
    , authorUrl : String
    }


type PostId
    = PostId Int


postsDecoder : Decoder (List Post)
postsDecoder =
    list postDecoder


postDecoder : Decoder Post
postDecoder =
    Decode.succeed Post
        |> required "id" idDecoder
        |> required "title" string
        |> required "authorName" string
        |> required "authorUrl" string


idDecoder : Decoder PostId
idDecoder =
    Decode.map PostId int


idToString : PostId -> String
idToString (PostId id) =
    String.fromInt id


idParser : Parser (PostId -> a) a
idParser =
    custom "POSTID" <|
        \postId -> Maybe.map PostId (String.toInt postId)


postEncoder : Post -> Encode.Value
postEncoder post =
    Encode.object
        [ ( "id", encodeId post.id )
        , ( "title", Encode.string post.title )
        , ( "authorName", Encode.string post.authorName )
        , ( "authorUrl", Encode.string post.authorUrl )
        ]


newPostEncoder : { a | title : String, authorName : String, authorUrl : String } -> Encode.Value
newPostEncoder { title, authorName, authorUrl } =
    Encode.object
        [ ( "title", Encode.string title )
        , ( "authorName", Encode.string authorName )
        , ( "authorUrl", Encode.string authorUrl )
        ]


encodeId : PostId -> Encode.Value
encodeId (PostId id) =
    Encode.int id
