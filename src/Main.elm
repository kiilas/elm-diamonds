module Main exposing (..)

import Array
import Browser
import Browser.Events
import Html
import Html.Attributes
import Json.Decode
import Time

import Cell
import Dir
import Explosion
import Game
import Key
import Map
import PlayerMove
import Thing

type alias Model =
  { game : Game.Game
  , keyState : KeyState
  }

type alias KeyState =
  { up : Bool
  , right : Bool
  , down : Bool
  , left : Bool
  }

setKeyState : Bool -> Key.Key -> KeyState -> KeyState
setKeyState pressed key keyState =
  case key of
    Key.Up -> {keyState | up = pressed}
    Key.Right -> {keyState | right = pressed}
    Key.Down -> {keyState | down = pressed}
    Key.Left -> {keyState | left = pressed}

init : () -> (Model, Cmd Msg)
init = always <| ({game = Game.init, keyState = KeyState False False False False}, Cmd.none)

type Msg
  = Tic
  | KeyDown Key.Key
  | KeyUp Key.Key
  | NoOp

thingClass : Thing.Thing -> String
thingClass thing =
    case thing of
        Thing.Dirt -> "dirt"
        Thing.Space -> "space"
        Thing.Steel -> "steel"
        Thing.Wall -> "wall"
        Thing.Boulder _ -> "boulder"
        Thing.Diamond _ -> "diamond"
        Thing.Player -> "player"
        Thing.Exit -> "exit"
        Thing.Firefly _ -> "firefly"
        Thing.Butterfly _ -> "butterfly"
        Thing.Explosion Explosion.Space Explosion.Stage1 -> "explosion-space-1"
        Thing.Explosion Explosion.Space Explosion.Stage2 -> "explosion-space-2"
        Thing.Explosion Explosion.Space Explosion.Stage3 -> "explosion-space-3"
        Thing.Explosion Explosion.Space Explosion.Stage4 -> "explosion-space-4"
        Thing.Explosion Explosion.Diamond Explosion.Stage1 -> "explosion-diamond-1"
        Thing.Explosion Explosion.Diamond Explosion.Stage2 -> "explosion-diamond-2"
        Thing.Explosion Explosion.Diamond Explosion.Stage3 -> "explosion-diamond-3"
        Thing.Explosion Explosion.Diamond Explosion.Stage4 -> "explosion-diamond-4"

numFrames : Thing.Thing -> Int
numFrames thing =
  case thing of
    Thing.Diamond _ -> 2
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
view model = mapView model.game.tic model.game.map

playerMove : KeyState -> PlayerMove.Move
playerMove keyState =
  if keyState.right
    then PlayerMove.Walk Dir.Right
  else if keyState.left
    then PlayerMove.Walk Dir.Left
  else if keyState.up
    then PlayerMove.Walk Dir.Up
  else if keyState.down
    then PlayerMove.Walk Dir.Down
  else
    PlayerMove.Stand

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tic -> ({model | game = Game.update (playerMove model.keyState) model.game}, Cmd.none)
    KeyDown key -> ({model | keyState = setKeyState True key model.keyState}, Cmd.none)
    KeyUp key -> ({model | keyState = setKeyState False key model.keyState}, Cmd.none)
    NoOp -> (model, Cmd.none)

decodeKey : String -> Maybe Key.Key
decodeKey string =
  case string of
    "ArrowUp" -> Just Key.Up
    "ArrowRight" -> Just Key.Right
    "ArrowDown" -> Just Key.Down
    "ArrowLeft" -> Just Key.Left
    _ -> Nothing

subscriptions : Model -> Sub Msg
subscriptions =
  always <|
    Sub.batch
    [ Time.every 200 <| always Tic
      , Browser.Events.onKeyDown
        <| Json.Decode.map (Maybe.withDefault NoOp << Maybe.map KeyDown << decodeKey)
        <| Json.Decode.field "key" Json.Decode.string
      , Browser.Events.onKeyUp
        <| Json.Decode.map (Maybe.withDefault NoOp << Maybe.map KeyUp << decodeKey)
        <| Json.Decode.field "key" Json.Decode.string]

main = Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
