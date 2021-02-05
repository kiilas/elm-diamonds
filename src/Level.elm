module Level exposing (Level, decode)

import Dir
import Explosion
import Map
import Thing

import Array

type alias Level =
  { map : Map.Map
  , toCollect : Int
  , bgColor : Int
  , fgColor : Int
  }

b64Decode : Char -> Maybe Int
b64Decode c =
  case
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
    |> String.indices (String.fromChar c)
      of
        n :: _ -> Just n
        [] -> Nothing

at : Array.Array Int -> Int -> Int
at array n =
  Array.get n array
  |> Maybe.withDefault 0

decode : String -> Level
decode string =
  let
    array = Array.fromList <| List.filterMap b64Decode <| String.toList string
    width = at array 0
    height = at array 1
  in
  { map =
      List.range 0 (height - 1)
      |> List.map (\y -> List.range (6 + y * width) (5 + (y + 1) * width))
      |> List.map (List.map <| at array)
      |> List.map (List.map decodeThing)
      |> Map.fromList
  , toCollect = 64 * at array 2 + at array 3
  , bgColor = at array 4
  , fgColor = at array 5
  }

decodeThing : Int -> Thing.Thing
decodeThing n =
  case n of
    0 -> Thing.Dirt
    1 -> Thing.Space
    2 -> Thing.Steel
    3 -> Thing.Wall
    4 -> Thing.Boulder False
    5 -> Thing.Boulder True
    6 -> Thing.Diamond False
    7 -> Thing.Diamond True
    8 -> Thing.Player
    9 -> Thing.Exit
    10 -> Thing.Firefly Dir.Up
    11 -> Thing.Firefly Dir.Right
    12 -> Thing.Firefly Dir.Down
    13 -> Thing.Firefly Dir.Left
    14 -> Thing.Butterfly Dir.Up
    15 -> Thing.Butterfly Dir.Right
    16 -> Thing.Butterfly Dir.Down
    17 -> Thing.Butterfly Dir.Left
    18 -> Thing.Explosion Explosion.Space Explosion.Stage1
    19 -> Thing.Explosion Explosion.Space Explosion.Stage2
    20 -> Thing.Explosion Explosion.Space Explosion.Stage3
    21 -> Thing.Explosion Explosion.Space Explosion.Stage4
    22 -> Thing.Explosion Explosion.Diamond Explosion.Stage1
    23 -> Thing.Explosion Explosion.Diamond Explosion.Stage2
    24 -> Thing.Explosion Explosion.Diamond Explosion.Stage3
    25 -> Thing.Explosion Explosion.Diamond Explosion.Stage4
    _ -> Thing.Steel
