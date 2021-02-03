module Main exposing (..)

import Array
import Browser
import Html
import Html.Attributes
import Time

import Cell
import Game
import Map

type alias Model = Game.Game

init : () -> (Model, Cmd Msg)
init = always <| (Game.initGame, Cmd.none)

type Msg
  = Tic

cellClass : Cell.Cell -> String
cellClass cell =
    case cell of
        Cell.Dirt -> "dirt"
        Cell.Space -> "space"
        Cell.Steel -> "steel"
        Cell.Wall -> "wall"
        Cell.Boulder -> "boulder"
        Cell.Diamond -> "diamond"
        Cell.Player -> "player"
        Cell.Exit -> "exit"
        Cell.Firefly -> "firefly"
        Cell.Butterfly -> "butterfly"

numFrames : Cell.Cell -> Int
numFrames cell =
  case cell of
    Cell.Diamond -> 2
    Cell.Firefly -> 2
    Cell.Butterfly -> 2
    _ -> 1

translationValue : Int -> Int -> String
translationValue x y = String.fromInt (x*100) ++ "% " ++ String.fromInt (y*100) ++ "%"

cellView : Int -> Int -> Int -> Cell.Cell -> Html.Html msg
cellView tic y x cell =
  Html.node
    "tile"
      [ Html.Attributes.class <| cellClass cell
      , Html.Attributes.class <| "frame-" ++ String.fromInt(modBy (numFrames cell) tic)
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
    tic -> ({model | tic = model.tic + 1}, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions = always <| Time.every 200 <| always Tic

main = Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
