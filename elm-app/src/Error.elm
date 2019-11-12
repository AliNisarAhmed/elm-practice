module Error exposing (buildErrorMessage, viewError)

import Element exposing (Element, row, text)
import Http


viewError : Maybe String -> Element msg
viewError maybeError =
    case maybeError of
        Just err ->
            row [] [ text err ]

        Nothing ->
            text ""


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message
