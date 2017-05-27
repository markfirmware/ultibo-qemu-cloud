module Main exposing (..)

import Animation exposing (px)
import Date
import Date.Format exposing (format)
import Dict
import Formatting exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import Navigation
import Regex exposing (..)
import Task
import Time exposing (second)
import Window


t = Formatting.s


main : Program Never Model Msg
main =
    Navigation.program
        NewLocation
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { style : Animation.State
    , date : Date.Date
    , size : Window.Size
    , diagnostic : Bool
    , ip : String
    }


type alias Location =
    { name : String
    , ip : String
    }


allLocations =
    Dict.fromList
        [ ("108.61.117.135" , "Amsterdam")
        , ("45.32.223.179" , "Atlanta")
        , ("45.63.115.228" , "Paris")
        , ("104.156.232.107" , "Sydney")
        ]


locationName ip =
    case Dict.get ip allLocations of
        Nothing -> "Unrecognized Location"
        Just name -> name


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( { style =
            Animation.style
                [ Animation.opacity 1.0
                ]
      , date = Date.fromTime 0.0
      , size = { width = 0, height = 0 }
      , diagnostic = False
      , ip = location.host
      }
    , Task.perform Size Window.size
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Animation.subscription Animate [ model.style ]
        , Time.every (0.1 * second) Tick
        , Window.resizes Size
        ]


type Msg
    = Animate Animation.Msg
    | Tick Time.Time
    | Size Window.Size
    | ToggleDiagnostic
    | NewLocation Navigation.Location


animationOpacity duration ease opacity =
    Animation.toWith
        ( Animation.easing
            { duration = duration * second
            , ease = ease
            }
        )
        [ Animation.opacity opacity
        ]


justEntered f prev current n =
    f prev /= n && f current == n


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        NewLocation location ->
            ( model
            , Cmd.none
            )

        Tick t -> let now = Date.fromTime t in
            ( { model
                | date = now
                , style = if justEntered Date.second model.date now 59 then
                    Animation.interrupt
                        [ animationOpacity 0.80 (\x -> x * x) 0
                        , Animation.wait (0.25 * second)
                        , animationOpacity 0.10 (\x -> x * x * x * x) 1
                        ]
                        model.style
                  else
                    model.style
              }
            , Cmd.none
            )

        Animate animMsg ->
            ( { model
                | style = Animation.update animMsg model.style
              }
            , Cmd.none
            )

        ToggleDiagnostic ->
            ( { model
                | diagnostic = not model.diagnostic
              }
            , Cmd.none
            )

        Size s ->
            ( { model
                | size = s
              }
            , Cmd.none
            )

--      _ ->
--          ( model
--          , Cmd.none
--          )


format2 f date =
    let afternoon =
            if      Date.hour date < 12 then "Morning"
            else if Date.hour date < 18 then "Afternoon"
            else if Date.hour date < 21 then "Evening"
            else                             "Night"
        d =
            Date.day date
        th =
            if      d == 1 || d == 21 || d == 31 then "st"
            else if d == 2 || d == 22            then "nd"
            else if d == 3 || d == 23            then "rd"
            else                                      "th"
    in date
        |> format f
        |> replace All (regex "%r") (\_ -> afternoon)
        |> replace All (regex "%t") (\_ -> th)


opacity model b o =
    ( "opacity" , if (xor b model.diagnostic) then "1.0" else o)


rootDiv model content =
    div
        [ style
            [ ( "width", "1920px" )
            , ( "height", "1080px" )
            , ( "overflow", "hidden" )
            , ( "background-color", "black" )
            , ( "color", "orange" )
            ]
        , onClick ToggleDiagnostic
        ]
        (if model.date == Date.fromTime 0 then
            [ text "" ]
        else
            content)


view : Model -> Html Msg
view model =
    rootDiv
        model
        [ div
          [ style
            [ ( "position" , "fixed" )
            , opacity model False "0"
            , ( "font-family", "monospace" )
            , ( "font-size", "40px" )
            , ( "color", "white" )
            ]
          ]
          [ text <| "Window.size " ++ toString model.size.width ++ " " ++ toString model.size.height
          ]
        , div
          [ style
            [ ( "font-size", if model.size.width >= 1400 then "185px" else "150px" )
            , ( "line-height", "97%" )
            , opacity model True "0.4"
            ]
          ]
          (List.map (\(s, f) -> div s <| [ text <| format2 f model.date ])
            [ ( Animation.render model.style, (locationName model.ip) ++ " Time: %l:%M" )
            , ( [], "%A %r" )
            , ( [], "%B %e%t %Y" )
            ])
        ]


spaceInt = padLeft 2 ' ' int
zeroInt = padLeft 2 '0' int

correctTime =
    t "Correct Time: "
    <> spaceInt
    <> t ":"
    <> zeroInt
    <> t ":"
    <> zeroInt
