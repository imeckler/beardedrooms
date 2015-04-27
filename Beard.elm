module Beard 
  ( Beard 
  ) where

{- for now, assume the display tree
is the same as tree that represents the semantics
of which objects descend from which for purposes
of moving whole subtrees around, folding whole subtrees
away, etc. But these might be separated if the user
wants to move a box around on the screen without
changing its parent and children. -}
import Object exposing (Object) 
import LinearOrder exposing (LinearOrder)
import Dict exposing (Dict)
import Debug

{- i changed my mind back to beards
being just a single DisplayTree. this 
avoids duplicating code and it provides a 
neat solution to rooms with multiple roots:
that room corresponds to a single db object
that displays invisibly and has as children
the would-be roots.-}
type alias Beard =
  { freshID : NodeID
  , tree : DisplayTree
--  , currentFocus : NodeID --or Location?
  }

--empty node carries something telling
--where a node used to be, maybe
type DisplayTree =
  Empty NodeID --or something. for leaving markers.
  | Node { data : NodeData
         , children : Children
         }

type alias NodeData =
  { value : Object.ObjectInContext
  , id : NodeID
  , location : Location
  }

type alias Children =
  { upNodes : Forest
  , downNodes : Forest
  , overNodes : Forest
  --maybe left and right nodes
  }   
 
type alias Forest = 
  { trees : Dict.Dict NodeID DisplayTree
  , order : LinearOrder.LinearOrder NodeID
  }

type alias NodeID = Int

type alias Location = List NodeID

empty : Beard
empty = { freshID = 0, 
        , forest = emptyTree }

emptyTree : DisplayTree
emptyTree = 
          
{ trees = Dict.empty
                   , order = LinearOrder.empty } }  

insert : Object.ObjectInContext -> Location -> Beard -> Beard
insert obj loc beard =
  case loc of
    [] -> let id = beard.freshID

          in { }


Debug.crash "Beard.insert: Location does not exist"

