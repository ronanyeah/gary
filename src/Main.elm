module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events
import Color
import Element exposing (Color, centerX, centerY, el, fill, height, none, padding, rgb255, width)
import Element.Background as Background
import Element.Events
import Element.Font as Font
import Helpers.View exposing (cappedHeight, cappedWidth, style, when)
import Html exposing (Html)
import Html.Attributes
import Json.Decode exposing (Decoder)
import Layer
import Random exposing (Generator)
import Time


type alias Model =
    { colors : Array Color
    , on : Bool
    , count : Int
    }


type Msg
    = ColorsCb (Maybe (List Color))
    | Toggle


cols : Array Color
cols =
    [ rgb255 190 64 48
    , rgb255 111 130 186
    , rgb255 222 233 156
    , orange
    , rgb255 138 214 230
    , rgb255 249 208 203
    , black
    , white
    ]
        |> Array.fromList


main : Program () Model Msg
main =
    Browser.element
        { init =
            always
                ( { colors = cols
                  , on = False
                  , count = 0
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
                    , Browser.Events.onKeyDown onPress
                    ]
        }


onPress : Decoder Msg
onPress =
    Json.Decode.field "keyCode" Json.Decode.int
        |> Json.Decode.andThen
            (\key ->
                if key == 13 || key == 32 then
                    Json.Decode.succeed Toggle

                else
                    Json.Decode.fail ""
            )


genColor : Generator Color
genColor =
    Random.map3
        Element.rgb255
        (Random.int 0 255)
        (Random.int 0 255)
        (Random.int 0 255)


white : Color
white =
    rgb255 255 255 255


black : Color
black =
    rgb255 0 0 0


orange : Color
orange =
    rgb255 241 113 62


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

        blk =
            arr
                |> Array.get 6
                |> Maybe.withDefault black

        bg =
            arr
                |> Array.get 7
                |> Maybe.withDefault white
                |> Background.color
    in
    none
        |> Element.layoutWith
            { options =
                [ Element.focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            (attrs
                ++ [ width fill
                   , height fill
                   , Element.Events.onClick Toggle
                   , bg
                   , Element.newTabLink
                        [ Element.alignRight
                        , Element.alignBottom
                        , padding 10
                        , Font.heavy
                        , Font.size 100
                        , Font.color blk
                        , style "user-select" "none"
                        , style "-webkit-tap-highlight-color" "transparent"
                        , Html.Attributes.class "woah"
                            |> Element.htmlAttribute
                        ]
                        { url = "https://tarbh.engineering/"
                        , label = Element.text "?"
                        }
                        |> when (model.count > 3 && model.count < 8)
                        |> Element.inFront
                   ]
            )


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
            ( { model | on = not model.on, count = model.count + 1 }
            , Cmd.none
            )
