module Signup exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Events


type alias User =
    { name : String
    , email : String
    , password : String
    , loggedIn : Bool
    }


initialModel : User
initialModel =
    { name = ""
    , email = ""
    , password = ""
    , loggedIn = False
    }


type Msg
    = UpdateText String
    | UpdateEmail String
    | UpdatePassword String
    | SubmitForm


update : Msg -> User -> User
update msg user =
    case msg of
        UpdateText str ->
            { user | name = str }

        UpdateEmail str ->
            { user | email = str }

        UpdatePassword p ->
            { user | password = p }

        SubmitForm ->
            Debug.log "Form Submitted" user


view : User -> Html Msg
view user =
    Element.layout [ padding 30, height fill ] <|
        el [ width fill, centerY ] <|
            column [ centerX, spacing 20, paddingXY 60 25, Border.color pink, Background.color bgPrimary ]
                [ row [ centerX ]
                    [ el [ Font.bold, Font.color pink, Font.size 30 ] <|
                        text "Sign up Form"
                    ]
                , row
                    []
                    [ nameInput user ]
                , row []
                    [ emailInput user ]
                , row []
                    [ passwordInput user ]
                , row [ centerX ]
                    [ submitButton ]
                ]


submitButton : Element Msg
submitButton =
    Input.button buttonStyles
        { onPress = Just SubmitForm
        , label = text "Submit Form"
        }


nameInput : User -> Element Msg
nameInput user =
    Input.text
        fieldStyles
        { onChange = UpdateText
        , text = user.name
        , placeholder =
            Just <|
                Input.placeholder [] (text "Your Name")
        , label = Input.labelAbove labelStyles (text "Name")
        }


emailInput : User -> Element Msg
emailInput user =
    Input.email fieldStyles
        { onChange = UpdateEmail
        , text = user.email
        , placeholder = Just <| Input.placeholder [] (text "Enter your Email")
        , label = Input.labelAbove labelStyles (text "Email")
        }


passwordInput : User -> Element Msg
passwordInput user =
    Input.newPassword fieldStyles
        { onChange = UpdatePassword
        , text = user.password
        , placeholder = Just <| Input.placeholder [] (text "Enter your Password")
        , label = Input.labelAbove labelStyles (text "Password")
        , show = False
        }


labelStyles : List (Attribute Msg)
labelStyles =
    [ Font.color pink ]


fieldStyles : List (Attribute Msg)
fieldStyles =
    [ centerX, alignTop, padding 20, Border.width 2, Border.rounded 10, Font.color pink ]


buttonStyles : List (Attribute Msg)
buttonStyles =
    [ centerX, padding 20, Border.width 2, Border.rounded 10, Background.color <| pink, Font.color white, Border.color pink, mouseOver [ Font.color pink, Background.color white ] ]


pink : Color
pink =
    rgb255 230 135 184


bgPrimary : Color
bgPrimary =
    rgba 0 0 0 0


white : Color
white =
    rgb 255 255 255


main : Program () User Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
