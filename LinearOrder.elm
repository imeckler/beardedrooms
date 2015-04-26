module LinearOrder 
  ( LinearOrder
  , empty
  ) where

import Dict exposing (Dict)
import Debug

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

