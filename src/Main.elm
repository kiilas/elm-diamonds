module Main exposing (..)

import Array
import Browser
import Html
import Html.Attributes

import Cell
import Game
import Map

type alias Model = Game.Game

init : () -> (Model, Cmd Msg)
init = always <| (Game.initGame, Cmd.none)

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

translationValue : Int -> Int -> String
translationValue x y = String.fromInt (x*100) ++ "% " ++ String.fromInt (y*100) ++ "%"

cellView : Int -> Int -> Cell.Cell -> Html.Html ()
cellView y x cell =
  Html.node
    "tile"
      [ Html.Attributes.class <| cellClass cell
      , Html.Attributes.style "translate" <| translationValue x y]
    []

rowView : Int -> List Cell.Cell -> List (Html.Html ())
rowView = List.indexedMap << cellView

mapView : Map.Map -> Html.Html ()
mapView = Html.node "div" [Html.Attributes.id "map"] << List.concat << List.indexedMap rowView << Map.toList

view : Model -> Html.Html ()
view = mapView << .map

update : Msg -> Model -> (Model, Cmd Msg)
update _ model = (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions = always Sub.none

main = Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
