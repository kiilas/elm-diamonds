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
        ahead = Map.at (Dir.add (PlayerMove.dir playerMove) pos) game.map
      in
      if Thing.passable ahead
        then move (PlayerMove.dir playerMove) pos game
      else
        game
    Thing.Handed rot ->
      let
        ahead = Map.at (Dir.add (Thing.dir thing) pos) game.map
        rotatedDir = Dir.rotate rot <| Thing.dir thing
        rotAhead = Map.at (Dir.add rotatedDir pos) game.map
      in
      if not (Thing.solid rotAhead)
        then move rotatedDir pos <| rotate rot pos game
      else if not (Thing.solid ahead)
        then move (Thing.dir thing) pos game
      else
        rotate (Rotation.inverse rot) pos game
    Thing.Gravity ->
      let
        under = Map.at (Dir.add Dir.Down pos) game.map
        left = Map.at (Dir.add Dir.Left pos) game.map
        leftUnder = Map.at (Dir.add Dir.Down <| Dir.add Dir.Left pos) game.map
        right = Map.at (Dir.add Dir.Right pos) game.map
        rightUnder = Map.at (Dir.add Dir.Down <| Dir.add Dir.Right pos) game.map
      in
        if not (Thing.solid under)
          then move Dir.Down pos <| change (Thing.setFalling True) pos game
        else
          if Thing.round under
          && not (Thing.solid left)
          && not (Thing.solid leftUnder)
            then move Dir.Left pos <| change (Thing.setFalling True) pos game
        else
          if Thing.round under
          && not (Thing.solid right)
          && not (Thing.solid rightUnder)
            then move Dir.Right pos <| change (Thing.setFalling True) pos game
        else
          change (Thing.setFalling False) pos game

rotate : Rotation.Rotation -> (Int, Int) -> Game -> Game
rotate rot pos game =
  let
    thing = Map.at pos game.map
  in
  {game | map = Map.set (Thing.rotate rot thing) pos game.map}

move : Dir.Dir -> (Int, Int) -> Game -> Game
move dir from game =
  let
    thing = Map.at from game.map
    to = Dir.add dir from
  in
  {game | map = Map.set Thing.Space from game.map |> Map.set thing to}

change : (Thing.Thing -> Thing.Thing) -> (Int, Int) -> Game -> Game
change f pos game =
  let
    thing = Map.at pos game.map
  in
  {game | map = Map.set (f thing) pos game.map}
