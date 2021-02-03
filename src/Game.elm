module Game exposing (Game, initGame)

import Cell
import Map

type alias Game = {map : Map.Map}

initGame : Game
initGame = Game <| Map.exampleMap
