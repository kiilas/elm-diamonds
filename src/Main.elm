module Main exposing (..)

import Array
import Browser
import Html
import Html.Attributes

import Cell
import Game
import Map

type alias Model = Game.Game

init : Model
init = Game.initGame

type alias Msg = ()

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

cellView : Int -> Int -> Cell.Cell -> Html.Html ()
cellView y x cell = Html.node "tile" [Html.Attributes.class <| cellClass cell] []

rowView : Int -> List Cell.Cell -> List (Html.Html ())
rowView = List.indexedMap << cellView

mapView : Map.Map -> Html.Html ()
mapView = Html.node "div" [Html.Attributes.id "map"] << List.concat << List.indexedMap rowView << Map.toList

view : Model -> Html.Html ()
view = mapView << .map

update : Msg -> Model -> Model
update _ = identity

main = Browser.sandbox
    { init = init
    , view = view
    , update = update
    }
