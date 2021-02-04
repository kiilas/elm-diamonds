module Thing exposing (Thing(..), MoveType(..), dir, rotate, solid, moveType, passable, setFalling, round)

import Dir exposing (..)
import Rotation exposing (..)

type Thing
  = Dirt
  | Space
  | Steel
  | Wall
  | Boulder Bool
  | Diamond Bool
  | Player
  | Exit
  | Firefly Dir
  | Butterfly Dir

type MoveType
  = NoMove
  | PlayerMove
  | Gravity
  | Handed Rotation

dir : Thing -> Dir
dir thing =
  case thing of
    Firefly d -> d
    Butterfly d -> d
    _ -> None

rotate : Rotation -> Thing -> Thing
rotate rot thing =
  case thing of
    Firefly d -> Firefly <| Dir.rotate rot d
    Butterfly d -> Butterfly <| Dir.rotate rot d
    _ -> thing

solid : Thing -> Bool
solid thing =
  case thing of
    Space -> False
    _ -> True

moveType : Thing -> MoveType
moveType cell =
  case cell of
    Player -> PlayerMove
    Boulder _ -> Gravity
    Diamond _ -> Gravity
    Firefly _ -> Handed CCW
    Butterfly _ -> Handed CW
    _ -> NoMove

passable : Thing -> Bool
passable thing =
  case thing of
    Dirt -> True
    Space -> True
    Diamond False -> True
    _ -> False

setFalling : Bool -> Thing -> Thing
setFalling falling thing =
  case (falling, thing) of
    (True, Boulder False) -> Boulder True
    (True, Diamond False) -> Diamond True
    (False, Boulder True) -> Boulder False
    (False, Diamond True) -> Diamond False
    _ -> thing

round : Thing -> Bool
round thing =
  case thing of
    Wall -> True
    Boulder _ -> True
    Diamond _ -> True
    _ -> False
