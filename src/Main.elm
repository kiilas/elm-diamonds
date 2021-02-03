module Main exposing (..)

import Array
import Browser
import Html
import Html.Attributes
import Time

import Cell
import Game
import Map
import Thing

type alias Model = Game.Game

init : () -> (Model, Cmd Msg)
init = always <| (Game.init, Cmd.none)

type Msg
  = Tic

thingClass : Thing.Thing -> String
thingClass thing =
    case thing of
        Thing.Dirt -> "dirt"
        Thing.Space -> "space"
        Thing.Steel -> "steel"
        Thing.Wall -> "wall"
        Thing.Boulder -> "boulder"
        Thing.Diamond -> "diamond"
        Thing.Player -> "player"
        Thing.Exit -> "exit"
        Thing.Firefly _ -> "firefly"
        Thing.Butterfly _ -> "butterfly"

numFrames : Thing.Thing -> Int
numFrames thing =
  case thing of
    Thing.Diamond -> 2
    Thing.Firefly _ -> 2
    Thing.Butterfly _ -> 2
    _ -> 1

translationValue : Int -> Int -> String
translationValue x y = String.fromInt (x*100) ++ "% " ++ String.fromInt (y*100) ++ "%"

cellView : Int -> Int -> Int -> Cell.Cell -> Html.Html msg
cellView tic y x cell =
  Html.node
    "tile"
      [ Html.Attributes.class <| thingClass cell.thing
      , Html.Attributes.class <| "frame-" ++ String.fromInt(modBy (numFrames cell.thing) tic)
      , Html.Attributes.style "translate" <| translationValue x y]
    []

rowView : Int -> Int -> List Cell.Cell -> List (Html.Html msg)
rowView tic = List.indexedMap << cellView tic

mapView : Int -> Map.Map -> Html.Html msg
mapView tic =
  Html.node
    "div"
    [ Html.Attributes.id "map"]
  << List.concat
  << List.indexedMap (rowView tic)
  << Map.toList

view : Model -> Html.Html msg
view model = mapView model.tic model.map

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    tic -> (Game.update model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions = always <| Time.every 200 <| always Tic

main = Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
