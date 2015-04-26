module LinearOrder 
  ( LinearOrder
  , empty
  , insertTop, insertBottom, moveUp, moveDown
  ) where

import Dict exposing (Dict)
import Debug

type alias LinearOrder v = List v

empty : LinearOrder v
empty = []

insertTop : v -> List v -> List v
insertTop v vs = v::vs

insertBottom : v -> List v -> List v
insertBottom v vs = vs ++ [v]

{-allows dumb things like moving something
not in the list, moving something already 
at the top -}
moveUp : v -> List v -> List v
moveUp v vs =
  case vs of
    [] -> []
    _::[] -> vs
    h1::h2::t ->
      if | h1 == v -> vs
         | h2 == v -> h2::h1::t
         | otherwise -> h1:: moveUp v (h2::t) 
--backwards behavior if there are duplicate v
moveDown : v -> List v -> List v
moveDown v vs =
  List.reverse <| moveUp v <| List.reverse vs

--too complicated for now
{--
type alias LinearOrder v =
  { max : NodeID
  , min : NodeID
  , set : OrderedSet v
  }

type alias OrderedSet v = Dict.Dict NodeID (Element v)

type alias NodeID = Int

type alias Element v =
  { pred : Maybe NodeID
  , succ : Maybe NodeID
  , value : v
  }

empty : LinearOrder v
empty = { max = 0, min = 0, set = Dict.empty }

singleton : v -> LinearOrder v
singleton v =
  { max = 0, min = 0, set = Dict.singleton 0 v }

{-
addTopElement : v -> LinearOrder v -> LinearOrder v
addTopElement v ord = 
  let set = ord.set 
  in 
    if set = Dict.empty
    then singleton v
    else let max = ord.max
             maxElement = Dict.get max 
-}      
--}
