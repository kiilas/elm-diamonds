module Cell exposing (..)

import Thing exposing (..)

type alias Cell =
  { thing : Thing
  , scanned : Bool
  }
