module Game exposing (Game, init, update)

import Cell
import Dir
import Map
import PlayerMove
import Rotation
import Thing

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

update : PlayerMove.Move -> Game -> Game
update playerMove game =
  updateMap playerMove {game | tic = game.tic + 1}

updateMap : PlayerMove.Move -> Game -> Game
updateMap playerMove game =
  let
    unscanned = {game | map = Map.unscan game.map}
  in
  List.foldl (updateCell playerMove) unscanned <| Map.coords game.map

updateCell : PlayerMove.Move -> (Int, Int) -> Game -> Game
updateCell playerMove pos game =
  let
    cell = Map.cellAt pos game.map
    thing = cell.thing
  in
  if cell.scanned
    then game
  else case Thing.moveType thing of
    Thing.NoMove -> game
    Thing.PlayerMove ->
      let
        ahead = Dir.add (PlayerMove.dir playerMove) pos
      in
      if Thing.passable (Map.at ahead game.map)
        then move pos ahead game
      else
        game
    Thing.Handed rot ->
      let
        ahead = Dir.add (Thing.dir thing) pos
        rotAhead = Dir.add (Dir.rotate rot (Thing.dir thing)) pos
      in
      if not (Thing.solid (Map.at rotAhead game.map))
        then move pos rotAhead <| rotate rot pos game
      else if not (Thing.solid (Map.at ahead game.map))
        then move pos ahead game
      else
        rotate (Rotation.inverse rot) pos game

rotate : Rotation.Rotation -> (Int, Int) -> Game -> Game
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
  {game | map = Map.set Thing.Space from game.map |> Map.set thing to}
