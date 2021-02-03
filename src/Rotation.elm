module Rotation exposing (Rotation(..), inverse)

type Rotation
  = CW
  | CCW

inverse : Rotation -> Rotation
inverse rot =
  case rot of
    CW -> CCW
    CCW -> CW
