module Thing exposing (Thing(..), MoveType(..), dir, rotate, solid, moveType, passable, setFalling, round, explosionType, crushable, isFalling, explosionKernel, destructible, isMonster, isFriendly, isExit)

import Dir exposing (..)
import Explosion
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
  | Explosion Explosion.Explosion Explosion.Stage

type MoveType
  = NoMove
  | PlayerMove
  | Gravity
  | Handed Rotation
  | Morph Thing

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
    Explosion e Explosion.Stage1 -> Morph (Explosion e Explosion.Stage2)
    Explosion e Explosion.Stage2 -> Morph (Explosion e Explosion.Stage3)
    Explosion e Explosion.Stage3 -> Morph (Explosion e Explosion.Stage4)
    Explosion Explosion.Space Explosion.Stage4 -> Morph Space
    Explosion Explosion.Diamond Explosion.Stage4 -> Morph (Diamond False)
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

explosionType : Thing -> Explosion.Explosion
explosionType thing =
  case thing of
    Butterfly _ -> Explosion.Diamond
    _ -> Explosion.Space

crushable : Thing -> Bool
crushable thing =
  case thing of
    Player -> True
    Firefly _ -> True
    Butterfly _ -> True
    _ -> False

isFalling : Thing -> Bool
isFalling thing =
  case thing of
    Boulder True -> True
    Diamond True -> True
    _ -> False

explosionKernel : Explosion.Explosion -> Thing
explosionKernel explosion = Explosion explosion Explosion.Stage1

destructible : Thing -> Bool
destructible thing =
  case thing of
    Steel -> False
    _ -> True

isMonster : Thing -> Bool
isMonster thing =
  case thing of
    Firefly _ -> True
    Butterfly _ -> True
    _ -> False

isFriendly : Thing -> Bool
isFriendly thing =
  case thing of
    Player -> True
    _ -> False

isExit : Thing -> Bool
isExit thing =
  case thing of
    Exit -> True
    _ -> False
