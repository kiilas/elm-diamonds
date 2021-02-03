module Game exposing (Game, initGame)

import Cell
import Map

type alias Game =
  { map : Map.Map
  , tic : Int
  }

initGame : Game
initGame =
  { map = Map.exampleMap
  , tic = 0
  }
