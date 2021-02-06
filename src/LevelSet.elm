module LevelSet exposing (LevelSet, get, getFirst, length, single, fromNonEmptyList)

import Level

import Array

type alias LevelSet =
  { head : Level.Level
  , tail : Array.Array Level.Level
  }

get : Int -> LevelSet -> Maybe Level.Level
get num levelSet =
  case num of
    0 -> Just levelSet.head
    n -> Array.get (n-1) levelSet.tail

getFirst : LevelSet -> Level.Level
getFirst = .head

length : LevelSet -> Int
length = (+) 1 << Array.length << .tail

single : Level.Level -> LevelSet
single level =
  { head = level
  , tail = Array.empty }

fromNonEmptyList : Level.Level -> List Level.Level -> LevelSet
fromNonEmptyList head tail =
  { head = head
  , tail = Array.fromList tail }
