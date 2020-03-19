module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events
import Color
import Element exposing (Color, centerX, centerY, el, fill, height, none, padding, rgb255, width)
import Element.Background as Background
import Element.Events
import Helpers.View exposing (cappedHeight, cappedWidth, style)
import Html exposing (Html)
import Json.Decode
import Layer
import Random exposing (Generator)
import Time


type alias Model =
    { colors : Array Color
    , on : Bool
    }


type Msg
    = ColorsCb (Maybe (List Color))
    | Toggle


cols : Array Color
cols =
    [ rgb255 190 64 48
    , rgb255 111 130 186
    , rgb255 222 233 156
    , rgb255 241 113 62
    , rgb255 138 214 230
    , rgb255 249 208 203
    , rgb255 0 0 0
    , rgb255 255 255 255
    ]
        |> Array.fromList


main : Program () Model Msg
main =
    Browser.element
        { init =
            always
                ( { colors = cols
                  , on = False
                  }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ if model.on then
                        Time.every 10
                            (always <|
                                ColorsCb Nothing
                            )

                      else
                        Sub.none
                    , Browser.Events.onKeyDown (Json.Decode.succeed Toggle)
                    ]
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
        arr =
            if model.on then
                model.colors

            else
                cols

        attrs =
            [ Layer.red
            , Layer.blue
            , Layer.yellow
            , Layer.orange
            , Layer.lightBlue
            , Layer.pink
            , Layer.black
            ]
                |> List.indexedMap
                    (\i ->
                        Element.html
                            >> el
                                [ centerX
                                , centerY
                                , arr
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
            arr
                |> Array.get 7
                |> Maybe.map
                    (Background.color
                        >> List.singleton
                    )
                |> Maybe.withDefault []
    in
    none
        |> el
            (attrs
                ++ [ width fill
                   , height fill
                   , Element.Events.onClick Toggle
                   ]
                ++ bg
            )
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
        ColorsCb md ->
            ( { model | colors = md |> Maybe.map Array.fromList |> Maybe.withDefault model.colors }
            , if md == Nothing then
                Random.list 8 genColor
                    |> Random.generate (Just >> ColorsCb)

              else
                Cmd.none
            )

        Toggle ->
            ( { model | on = not model.on }
            , Cmd.none
            )
