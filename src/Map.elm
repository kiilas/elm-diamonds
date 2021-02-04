module Map exposing (Map, exampleMap, toList, coords, at, cellAt, set, fromList, unscan)

import Array
import Cell exposing (..)
import Dir exposing (..)
import Maybe exposing (..)
import Thing exposing (..)

type alias Array2D a = Array.Array (Array.Array a)

type alias Map = Array2D Cell.Cell

fromList : List (List Thing) -> Map
fromList =
  let
    fromThing t = {thing = t, scanned = False}
  in
    Array.fromList << List.map (Array.fromList << List.map fromThing)

exampleMap : List (List Thing)
exampleMap =
  [ [Steel,   Steel,   Steel,   Steel,        Steel, Steel,          Steel, Steel]
  , [ Exit,   Space,   Space,  Player,         Dirt,  Wall,          Space, Steel]
  , [Steel, Boulder False, Boulder False,    Dirt,         Dirt,  Wall, Butterfly Left, Steel]
  , [Steel, Diamond False,    Dirt,    Dirt,         Dirt,  Wall,           Dirt, Steel]
  , [Steel,    Dirt,    Dirt, Boulder False,      Diamond False,  Dirt,           Dirt, Steel]
  , [Steel,    Dirt,    Dirt,    Dirt,         Dirt,  Dirt,           Dirt, Steel]
  , [Steel,    Dirt,    Dirt,    Dirt, Firefly Down, Space,        Diamond False, Steel]
  , [Steel,   Steel,   Steel,   Steel,        Steel, Steel,          Steel, Steel]]

toList : Map -> List (List Cell)
toList = Array.toList << Array.map Array.toList

coords : Map -> List (Int, Int)
coords = List.concatMap rowCoords << Array.toIndexedList

rowCoords : (Int, Array.Array Cell.Cell) -> List (Int, Int)
rowCoords (y, row) = List.map (\x -> (x, y)) <| List.range 0 <| (Array.length row - 1)

at : (Int, Int) -> Map -> Thing
at pos map = .thing <| cellAt pos map

cellAt : (Int, Int) -> Map -> Cell.Cell
cellAt (x, y) map =
  Array.get y map
  |> andThen (Array.get x)
  |> withDefault {thing = Steel, scanned = True}

set : Thing -> (Int, Int) -> Map -> Map
set thing (x, y) map =
  Array.get y map
  |> Maybe.map (Array.set x {thing = thing, scanned = True})
  |> Maybe.map (\row -> Array.set y row map)
  |> withDefault map

unscan : Map -> Map
unscan = Array.map <| Array.map <| (\cell -> {cell | scanned = False})
