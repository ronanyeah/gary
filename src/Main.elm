module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Events
import Element.Font as Font
import Helpers.View exposing (cappedHeight, cappedWidth, style, when, whenAttr)
import Html exposing (Html)
import Html.Attributes
import Json.Decode exposing (Decoder)
import Layer
import Process
import Random exposing (Generator)
import SolidColor
import Task
import Time


type alias Model =
    { colors : Array Color
    , on : Bool
    , count : Int
    }


type Msg
    = ColorsCb Int Color
    | Toggle


cols : List Color
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


main : Program () Model Msg
main =
    Browser.element
        { init =
            always
                ( { colors = Array.fromList cols
                  , on = False
                  , count = 0
                  }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions =
            \_ ->
                --Browser.Events.onKeyDown onPress
                Sub.none
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
        attrs =
            [ Layer.red
            , Layer.blue
            , Layer.yellow
            , Layer.orange
            , Layer.lightBlue
            , Layer.pink
            , Layer.black
            ]
                |> List.map2
                    (\c ->
                        Element.html
                            >> el
                                [ centerX
                                , centerY
                                , style "fill" (toHex c)
                                , cappedWidth 500
                                , cappedHeight 500
                                , Element.padding 50
                                , Element.Events.onClick Toggle
                                ]
                            >> Element.inFront
                    )
                    (Array.toList model.colors)

        blk =
            model.colors
                |> Array.get 6
                |> Maybe.withDefault black

        bg =
            model.colors
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
                   , Element.pointer
                   , bg
                   , image [ height <| px 100, centerX, centerY, moveDown 200 ]
                        { src = "/gary/hand.png"
                        , description = ""
                        }
                        |> inFront
                        |> whenAttr (model.count == 0)

                   --, newTabLink
                   --[ alignRight
                   --, alignBottom
                   --, padding 10
                   --, Font.heavy
                   --, Font.size 100
                   --, Font.color blk
                   --, style "user-select" "none"
                   --, style "-webkit-tap-highlight-color" "transparent"
                   --, Html.Attributes.class "woah"
                   --|> Element.htmlAttribute
                   --]
                   --{ url = "https://tarbh.engineering/"
                   --, label = text "?"
                   --}
                   --|> when (model.count > 3 && model.count < 8)
                   --|> inFront
                   , newTabLink
                        [ centerX
                        , padding 15
                        , Font.bold
                        , Font.size 35
                        , Font.color blk
                        , alignBottom
                        , Font.family [ Font.typeface "Courier New" ]

                        --, style "user-select" "none"
                        --, style "-webkit-tap-highlight-color" "transparent"
                        --, Html.Attributes.class "woah"
                        --|> Element.htmlAttribute
                        ]
                        { url = "https://tarbh.net/"
                        , label = text "tarbh.net"
                        }
                        |> inFront
                        |> whenAttr (model.count > 1 && not model.on)
                   ]
            )


toHex : Color -> String
toHex =
    Element.toRgb
        >> (\{ red, green, blue } ->
                SolidColor.fromRGB ( red * 255, green * 255, blue * 255 )
           )
        >> SolidColor.toHex


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ColorsCb n md ->
            let
                next =
                    if n >= 7 then
                        0

                    else
                        n + 1
            in
            if model.on then
                ( { model
                    | colors = Array.set n md model.colors
                  }
                , Process.sleep 5
                    |> Task.andThen
                        (\_ ->
                            Time.now
                        )
                    |> Task.map
                        (Time.posixToMillis
                            >> Random.initialSeed
                            >> Random.step genColor
                            >> Tuple.first
                        )
                    |> Task.perform (ColorsCb next)
                )

            else
                ( { model | colors = Array.fromList cols }, Cmd.none )

        Toggle ->
            if model.on then
                ( { model
                    | on = False
                    , colors = Array.fromList cols
                    , count = model.count + 1
                  }
                , Cmd.none
                )

            else
                ( { model
                    | on = True
                    , count = model.count + 1
                  }
                , genColor
                    |> Random.generate (ColorsCb 0)
                )
