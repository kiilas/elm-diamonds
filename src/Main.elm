module Main exposing (..)

import Array
import Browser
import Browser.Events
import Browser.Navigation
import Html
import Html.Attributes
import Json.Decode
import Time
import Url

import Cell
import Dir
import Explosion
import Game
import Key
import Level
import Map
import PlayerMove
import Thing

type alias Model =
  { game : Game.Game
  , keyState : KeyState
  , level : Level.Level
  }

type alias KeyState =
  { up : Bool
  , right : Bool
  , down : Bool
  , left : Bool
  , fire : Bool
  }

initKeyState : KeyState
initKeyState = KeyState False False False False False

setKeyState : Bool -> Key.Key -> KeyState -> KeyState
setKeyState pressed key keyState =
  case key of
    Key.Up -> {keyState | up = pressed}
    Key.Right -> {keyState | right = pressed}
    Key.Down -> {keyState | down = pressed}
    Key.Left -> {keyState | left = pressed}
    Key.Fire -> {keyState | fire = pressed}
    Key.Restart -> keyState

defaultLevel =
  """
  IIAJA_
  CCCCCCCC
  JBBIADBC
  CEEAADRC
  CGAAADAC
  CAAEGAAC
  CAAAAAAC
  CAAAMBGC
  CCCCCCCC
  """

init : () -> Url.Url -> Browser.Navigation.Key -> (Model, Cmd Msg)
init _ url _ =
  let
    levelString =
      case url.query of
        Nothing -> defaultLevel
        Just l -> l
    level = Level.decode levelString
  in
  (
    { game = Game.fromLevel level
    , keyState = initKeyState
    , level = level }
  , Cmd.none )

type Msg
  = Tic
  | KeyDown Key.Key
  | KeyUp Key.Key
  | VisibilityChange Browser.Events.Visibility
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
    Thing.Exit -> 2
    _ -> 1

translation : Int -> Int -> String
translation x y =
  "translate("
  ++ String.fromInt (x*100)
  ++ "%, "
  ++ String.fromInt (y*100)
  ++ "%)"

cellView : Int -> Int -> Int -> Cell.Cell -> Html.Html msg
cellView tic y x cell =
  Html.node
    "tile"
      [ Html.Attributes.class <| thingClass cell.thing
      , Html.Attributes.class <| "frame-" ++ String.fromInt(1 + modBy (numFrames cell.thing) tic)
      , Html.Attributes.style "transform" <| translation x y]
    []

rowView : Int -> Int -> List Cell.Cell -> List (Html.Html msg)
rowView tic = List.indexedMap << cellView tic

gameView : Game.Game -> Html.Html msg
gameView game =
  Html.node
    "div"
    [ Html.Attributes.id "map"
    , Html.Attributes.class
      <| if game.collected >= game.toCollect then
           "exit-open"
         else
           "exit-closed"]
  <| List.concat
  <| List.indexedMap (rowView game.tic)
  <| Map.toList game.map

view : Model -> Browser.Document msg
view model =
  { title = "elm-diamonds"
  , body = [gameView model.game]
  }

playerMove : KeyState -> PlayerMove.Move
playerMove keyState =
  let
    action =
      if keyState.fire then
        PlayerMove.Grab
      else
        PlayerMove.Walk
  in
  if keyState.right
    then action Dir.Right
  else if keyState.left
    then action Dir.Left
  else if keyState.up
    then action Dir.Up
  else if keyState.down
    then action Dir.Down
  else
    PlayerMove.Stand

restart : Model -> Model
restart model = {model | game = Game.fromLevel model.level}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tic ->
      ( { model | game = Game.update (playerMove model.keyState) model.game}
        |> \m ->
          if m.game.finished then
            restart model
          else
            m
        , Cmd.none)
    KeyDown key ->
      case key of
        Key.Restart -> (restart model, Cmd.none)
        _ -> ({model | keyState = setKeyState True key model.keyState}, Cmd.none)
    KeyUp key -> ({model | keyState = setKeyState False key model.keyState}, Cmd.none)
    NoOp -> (model, Cmd.none)
    VisibilityChange _ -> ({model | keyState = initKeyState}, Cmd.none)

decodeKey : String -> Maybe Key.Key
decodeKey string =
  case string of
    "ArrowUp" -> Just Key.Up
    "ArrowRight" -> Just Key.Right
    "ArrowDown" -> Just Key.Down
    "ArrowLeft" -> Just Key.Left
    "Shift" -> Just Key.Fire
    "r" -> Just Key.Restart
    "R" -> Just Key.Restart
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
        <| Json.Decode.field "key" Json.Decode.string
      , Browser.Events.onVisibilityChange VisibilityChange ]


main = Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = always NoOp
    , onUrlChange = always NoOp
    }
