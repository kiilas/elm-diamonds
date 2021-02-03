module Map exposing (Map, emptyMap, exampleMap, toList)

import Array
import Cell exposing (..)

type alias Array2D a = Array.Array (Array.Array a)

type alias Map = Array2D Cell.Cell

emptyMap : Int -> Int -> Map
emptyMap w h = Array.repeat w <| Array.repeat h Cell.Space

exampleMap : Map
exampleMap =
  Array.fromList <| List.map Array.fromList
    [ [Steel,   Steel,   Steel,   Steel,   Steel, Steel,     Steel, Steel]
    , [ Exit,   Space,   Space,  Player,    Dirt,  Wall,     Space, Steel]
    , [Steel, Boulder, Boulder,    Dirt,    Dirt,  Wall, Butterfly, Steel]
    , [Steel, Diamond,    Dirt,    Dirt,    Dirt,  Wall,      Dirt, Steel]
    , [Steel,    Dirt,    Dirt, Boulder, Diamond,  Dirt,      Dirt, Steel]
    , [Steel,    Dirt,    Dirt,    Dirt,    Dirt,  Dirt,      Dirt, Steel]
    , [Steel,    Dirt,    Dirt,    Dirt, Firefly, Space,   Diamond, Steel]
    , [Steel,   Steel,   Steel,   Steel,   Steel, Steel,     Steel, Steel]]

toList : Map -> List (List Cell)
toList = Array.toList << Array.map Array.toList
