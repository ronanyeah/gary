module Main exposing (main)

import Browser
import Element exposing (centerX, centerY, el, fill, height, none, width)
import Helpers.View exposing (cappedWidth, style)
import Html exposing (Html)


type alias Model =
    {}


type Msg
    = Msg


main : Program () Model Msg
main =
    Browser.element
        { init =
            always
                ( {}
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


view : Model -> Html Msg
view _ =
    let
        attrs =
            [ "blue"
            , "light-blue"
            , "orange"
            , "yellow"
            , "black"
            , "pink"
            , "red"
            ]
                |> List.map
                    (\col ->
                        Element.image [ cappedWidth 500, centerX, centerY ]
                            { src = "/" ++ col ++ ".svg"
                            , description = ""
                            }
                            |> Element.inFront
                    )
    in
    none
        |> el (attrs ++ [ width fill, height fill ])
        |> Element.layoutWith
            { options =
                [ Element.focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            [ width fill
            , height fill
            , style "-webkit-tap-highlight-color" "transparent"
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )
