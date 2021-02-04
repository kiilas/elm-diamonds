module PlayerMove exposing (..)

import Dir exposing (..)

type Move
  = Stand
  | Walk Dir

dir : Move -> Dir
dir move =
  case move of
    Stand -> Dir.None
    Walk d -> d
