module Main exposing (main)

import Array exposing (Array)
import Browser
import Color
import Element exposing (Color, centerX, centerY, el, fill, height, none, padding, width)
import Element.Background as Background
import Helpers.View exposing (cappedHeight, cappedWidth, style)
import Html exposing (Html)
import Layer
import Random exposing (Generator)
import Time


type alias Model =
    { colors : Array Color }


type Msg
    = ColorsCb (List Color)
    | Go


main : Program () Model Msg
main =
    Browser.element
        { init =
            always
                ( { colors = Array.empty }
                , Cmd.none
                  --, Random.list 8 genColor
                  --|> Random.generate ColorsCb
                )
        , view = view
        , update = update
        , subscriptions =
            always
                (Time.every 10
                    (always Go)
                )
        }


genColor : Generator Color
genColor =
    Random.map3
        (\a b c ->
            Element.rgb255
                (round (255 * a))
                (round (255 * b))
                (round (255 * c))
        )
        (Random.float 0 1)
        (Random.float 0 1)
        (Random.float 0 1)


view : Model -> Html Msg
view model =
    let
        attrs =
            [ Layer.black
            , Layer.red
            , Layer.blue
            , Layer.orange
            , Layer.yellow
            , Layer.lightBlue
            , Layer.pink
            ]
                |> List.indexedMap
                    (\i ->
                        Element.html
                            >> el
                                [ centerX
                                , centerY
                                , model.colors
                                    |> Array.get i
                                    |> Maybe.map toHex
                                    |> Maybe.withDefault ""
                                    |> style "fill"
                                , cappedWidth 500
                                , cappedHeight 500
                                , Element.padding 50
                                ]
                            >> Element.inFront
                    )

        bg =
            model.colors
                |> Array.get 7
                |> Maybe.map
                    (Background.color
                        >> List.singleton
                    )
                |> Maybe.withDefault []
    in
    none
        |> el (attrs ++ [ width fill, height fill ] ++ bg)
        |> Element.layoutWith
            { options =
                [ Element.focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            []


toHex : Color -> String
toHex =
    Element.toRgb
        >> (\{ red, green, blue } ->
                Color.fromRGB ( red * 255, green * 255, blue * 255 )
           )
        >> Color.toHex


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ColorsCb data ->
            ( { model | colors = Array.fromList data }, Cmd.none )

        Go ->
            ( model
            , Random.list 8 genColor
                |> Random.generate ColorsCb
            )
