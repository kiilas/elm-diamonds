module Game exposing (Game, init, update)

import Cell
import Dir
import Explosion
import Map
import PlayerMove
import Rotation
import Thing

import Array

type alias Game =
  { map : Map.Map
  , tic : Int
  , collected : Int
  , toCollect : Int
  , pushAttempt : Maybe Dir.Dir
  }

init : Game
init =
  { map = Map.fromList Map.exampleMap
  , tic = 0
  , collected = 0
  , toCollect = 9
  , pushAttempt = Nothing
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
  else if Thing.isMonster thing && Map.adjacentTo Thing.isFriendly game.map pos
    then explode (Thing.explosionType thing) pos game
  else case Thing.moveType thing of
    Thing.NoMove -> game
    Thing.PlayerMove ->
      let
        pushAttempt = game.pushAttempt
        playerDir = PlayerMove.dir playerMove
        aheadPos = Dir.add playerDir pos
        ahead = Map.at game.map aheadPos
      in
      game
      |> always {game | pushAttempt = Nothing}
      |>
        if Thing.passable ahead then
          collect ahead
          >>
            case playerMove of
              PlayerMove.Stand -> identity
              PlayerMove.Walk d -> move d pos
              PlayerMove.Grab d -> change (always Thing.Space) aheadPos
        else
          if Dir.isHorizontal playerDir
          && Thing.isPushable ahead
          && not (Thing.solid <| Map.at game.map (Dir.add playerDir aheadPos))
            then
              if Just playerDir == game.pushAttempt then
                move playerDir aheadPos
                >> move playerDir pos
              else
                always {game | pushAttempt = Just playerDir}
        else if Thing.isExit ahead && game.collected >= game.toCollect then
          always init
        else
          identity
    Thing.Handed rot ->
      let
        ahead = Map.at game.map (Dir.add (Thing.dir thing) pos)
        rotatedDir = Dir.rotate rot <| Thing.dir thing
        rotAhead = Map.at game.map (Dir.add rotatedDir pos)
      in
      if not (Thing.solid rotAhead)
        then move rotatedDir pos <| rotate rot pos game
      else if not (Thing.solid ahead)
        then move (Thing.dir thing) pos game
      else
        rotate (Rotation.inverse rot) pos game
    Thing.Gravity ->
      let
        under = Map.at game.map (Dir.add Dir.Down pos)
        left = Map.at game.map (Dir.add Dir.Left pos)
        leftUnder = Map.at game.map (Dir.add Dir.Down <| Dir.add Dir.Left pos)
        right = Map.at game.map (Dir.add Dir.Right pos)
        rightUnder = Map.at game.map (Dir.add Dir.Down <| Dir.add Dir.Right pos)
      in
        if not (Thing.solid under)
          then move Dir.Down pos <| change (Thing.setFalling True) pos game
        else
          if Thing.isFalling thing && Thing.crushable under
            then explode (Thing.explosionType under) (Dir.add Dir.Down pos) game
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
    Thing.Morph t -> change (always t) pos game

rotate : Rotation.Rotation -> (Int, Int) -> Game -> Game
rotate rot pos game =
  let
    thing = Map.at game.map pos
  in
  {game | map = Map.set (Thing.rotate rot thing) pos game.map}

move : Dir.Dir -> (Int, Int) -> Game -> Game
move dir from game =
  let
    thing = Map.at game.map from
    to = Dir.add dir from
  in
  {game | map = Map.set Thing.Space from game.map |> Map.set thing to}

change : (Thing.Thing -> Thing.Thing) -> (Int, Int) -> Game -> Game
change f pos game =
  let
    thing = Map.at game.map pos
  in
  {game | map = Map.set (f thing) pos game.map}

explode : Explosion.Explosion -> (Int, Int) -> Game -> Game
explode explosion (x, y) game =
  let
    explosionCoords =
      [ (x-1, y-1), (x, y-1), (x+1, y-1)
      , (x-1, y), (x, y), (x+1, y)
      , (x-1, y+1), (x, y+1), (x+1, y+1) ]
    explosionChange thing =
      if Thing.destructible thing
        then Thing.explosionKernel explosion
      else
        thing
  in
  List.foldl (change explosionChange) game explosionCoords

collect : Thing.Thing -> Game -> Game
collect thing game =
  case thing of
    Thing.Diamond _ -> {game | collected = game.collected + 1}
    _ -> game
