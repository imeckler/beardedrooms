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

type alias Beard =
  { freshID : NodeID
  , forest : Forest
  , currentFocus : NodeID --or Location?
  }

type alias Forest = List DisplayTree

--empty node carries something telling
--where a node used to be, maybe
type DisplayTree =
  Empty NodeID --or something
  | Node { data : NodeData
         , children : Children
         }

type alias Children =
  { upNodes : Forest
  , downNodes : Forest
  , overNodes : Forest
  --maybe left and right nodes
  }   
 
type alias NodeData =
  { value : Object.ObjectInContext
  , id : NodeID
  , location : Location
  }

type alias NodeID = Int

type alias Location = List NodeID
