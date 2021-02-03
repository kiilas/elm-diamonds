module Thing exposing (Thing(..), MoveType(..), dir, rotate, solid, moveType)

import Dir exposing (..)
import Rotation exposing (..)

type Thing
  = Dirt
  | Space
  | Steel
  | Wall
  | Boulder
  | Diamond
  | Player
  | Exit
  | Firefly Dir
  | Butterfly Dir

type MoveType
  = NoMove
  | Handed Rotation

dir : Thing -> Dir
dir thing =
  case thing of
    Firefly d -> d
    Butterfly d -> d
    _ -> NoDir

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
    Firefly _ -> Handed CCW
    Butterfly _ -> Handed CW
    _ -> NoMove
