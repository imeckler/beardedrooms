module LinearOrder 
  ( LinearOrder
  , empty
  , insertTop, insertBottom, insertAbove, insertBelow
  , moveUp, moveDown
  , delete, moveAbove, moveBelow
  , atTop, atBottom, member
  , heads
  ) where

import Dict exposing (Dict)
import Debug

type alias LinearOrder v = List v

empty : LinearOrder v
empty = []

--allows repeated elements
insertTop : v -> LinearOrder v -> LinearOrder v
insertTop v vs = v::vs

insertBottom : v -> LinearOrder v -> LinearOrder v
insertBottom v vs = vs ++ [v]

insertAbove : v -> v -> LinearOrder v -> LinearOrder v
insertAbove new place vs =
  case vs of
    [] -> []
    h::t -> if h == place
               then new::h::t
               else h::(insertAbove new place t)

insertBelow : v -> v -> LinearOrder v -> LinearOrder v
insertBelow new place vs =
  List.reverse vs |> insertAbove new place |> List.reverse

delete : v -> LinearOrder v -> LinearOrder v
delete v vs = case vs of 
    []   -> []
    h::t -> if h == v 
               then t
               else h :: delete v t

moveAbove : v -> v -> LinearOrder v -> LinearOrder v
moveAbove v place vs =
  delete v vs |> insertAbove v place

moveBelow : v -> v -> LinearOrder v -> LinearOrder v
moveBelow v place vs =
  delete v vs |> insertBelow v place

{-allows dumb things like moving something
not in the list, moving something already 
at the top -}
moveUp : v -> LinearOrder v -> LinearOrder v
moveUp v vs =
  case vs of
    [] -> []
    _::[] -> vs
    h1::h2::t ->
      if | h1 == v -> vs
         | h2 == v -> h2::h1::t
         | otherwise -> h1:: moveUp v (h2::t) 

--backwards behavior if there are duplicate v
moveDown : v -> LinearOrder v -> LinearOrder v
moveDown v vs =
  List.reverse vs |> moveUp v |> List.reverse

atTop : v -> LinearOrder v -> Bool
atTop v vs =
  case vs of 
    [] -> False
    h::_ -> h == v

atBottom : v -> LinearOrder v -> Bool
atBottom v vs =
  List.reverse vs |> atTop v

member : v -> LinearOrder v -> Bool
member = List.member

heads : LinearOrder v -> Maybe (LinearOrder v)
heads vs = List.reverse vs |> List.tail


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
