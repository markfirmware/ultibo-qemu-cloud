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


projects =
    [ { name = "WebStatusProgram", portDigit = 0, page = True }
    , { name = "Example 01-HelloWorld", portDigit = 5, page = False }
    , { name = "Example 05-TimeDate", portDigit = 1, page = False }
    , { name = "Example 09-LogOutput", portDigit = 2, page = False }
    , { name = "Example 17-TextEditor", portDigit = 4, page = False }
    ]


main : Program Never Model Msg
main =
    Navigation.program
        NewLocation
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


type alias Model =
    { ip : String
    }


type alias Project =
    { name : String
    , portDigit : Int
    , page : Bool
    }


webServerUrl p ip =
    "http://" ++ ip ++ ":557" ++ toString p.portDigit ++ "/status/about"


vncUrl p ip =
    "http://novnc.com/noVNC/vnc_auto.html?host=" ++
    ip ++ "&port=577" ++ toString p.portDigit ++
    "&reconnect=1&reconnect_delay=5000"


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
    ( { ip = location.host
      }
    , Cmd.none
    )


type Msg
    = NewLocation Navigation.Location


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        NewLocation location ->
            ( model
            , Cmd.none
            )


rootDiv content =
    div
        [ style
            [ ( "width", "1920px" )
            , ( "height", "1080px" )
--          , ( "overflow", "hidden" )
            , ( "background-color", "white" )
            , ( "color", "orange" )
            , ( "font-size", "40px" )
            , ( "font-family", "monospace" )
            ]
        ]
        content


nameLine model =
    div [] <| [ text <| locationName model.ip ]


vncLink project model =
    a [ href <| vncUrl project model.ip ] [ text project.name ]


webServerLink project model =
    a [ href <| webServerUrl project model.ip ] [ text "(built-in web server)" ]


maybeMore b x =
    if b then
        [ text " " 
        , x
        ]
    else
        []


view : Model -> Html Msg
view model =
    rootDiv <|
      [ nameLine model ] ++
      List.map (\project -> div [] <|
          [ vncLink project model ] ++
           maybeMore project.page (webServerLink project model)) projects
