module Game exposing (Game, init, update)

import Cell exposing (..)
import Dir
import Map
import Rotation exposing (..)
import Thing exposing (..)

import Array

type alias Game =
  { map : Map.Map
  , tic : Int
  }

init : Game
init =
  { map = Map.fromList Map.exampleMap
  , tic = 0
  }

update : Game -> Game
update game = updateMap {game | tic = game.tic + 1}

updateMap : Game -> Game
updateMap game =
  let
    unscanned = {game | map = Map.unscan game.map}
  in
  List.foldl updateCell unscanned <| Map.coords game.map

updateCell : (Int, Int) -> Game -> Game
updateCell pos game =
  let
    cell = Map.cellAt pos game.map
    thing = cell.thing
  in
  if cell.scanned
    then game
  else case moveType thing of
    NoMove -> game
    Handed rot ->
      let
        ahead = Dir.add (dir thing) pos
        rotAhead = Dir.add (Dir.rotate rot (dir thing)) pos
      in
      if not (solid (Map.at ahead game.map))
        then move pos ahead game
      else if not (solid (Map.at rotAhead game.map))
        then move pos rotAhead <| rotate rot pos game
      else
        rotate (inverse rot) pos game

rotate : Rotation -> (Int, Int) -> Game -> Game
rotate rot pos game =
  let
    thing = Map.at pos game.map
  in
  {game | map = Map.set (Thing.rotate rot thing) pos game.map}

move : (Int, Int) -> (Int, Int) -> Game -> Game
move from to game =
  let
    thing = Map.at from game.map
  in
  {game | map = Map.set Space from game.map |> Map.set thing to}
