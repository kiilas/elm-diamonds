module Dir exposing (Dir(..), add, rotate, isHorizontal)

import Rotation exposing (..)

type Dir
  = Up
  | Right
  | Down
  | Left
  | None

add : Dir -> (Int, Int) -> (Int, Int)
add dir (x, y) =
  case dir of
    Up -> (x, y - 1)
    Right -> (x + 1, y)
    Down -> (x, y + 1)
    Left -> (x - 1, y)
    None -> (x, y)

rotate : Rotation -> Dir -> Dir
rotate rot dir =
  case rot of
    CW ->
      case dir of
        Up -> Right
        Right -> Down
        Down -> Left
        Left -> Up
        None -> None
    CCW ->
      case dir of
        Up -> Left
        Right -> Up
        Down -> Right
        Left -> Down
        None -> None

isHorizontal : Dir -> Bool
isHorizontal dir =
  case dir of
    Left -> True
    Right -> True
    _ -> False
