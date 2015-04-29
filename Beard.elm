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
the would-be roots. still have special functions
so that you don't generally add root nodes to beards.-}
type alias Beard =
  { freshID : NodeID
  , tree : DisplayTree
--  , currentFocus : NodeID --or Location?
  }

type DisplayTree =
  { nodeData : NodeData
  , children : Children
  }

type alias NodeData =
  { value : Object.ObjectInContext
  , location : Location
  }

getID : NodeData -> NodeID
getID nodeData =
  nodeData.location |> List.reverse |> List.head |> fst

type NodeKind = UpNode | DownNode | OverNode --more

type alias Children =
  { upNodes : Forest
  , downNodes : Forest
  , overNodes : Forest
  --maybe left, right and under nodes
  }   
 
type alias Forest = 
  Forest
    { trees : Dict.Dict NodeID DisplayTree
    , order : LinearOrder.LinearOrder NodeID
    }

type alias NodeID = Int

--location [(3,Up),(6,Down)] means i'm the 
--6th DownNode of the 3rd UpNode of the root
type alias Location = List (NodeID,NodeKind)

singleton : Object.ObjectInContex -> Beard
singleton obj =
  let nodeData =
        { value = obj, location = [] } 
  in
  { freshID = 1,
  , tree = singletonTree nodeData 
  } 

singletonTree : NodeData -> DisplayTree
singletonTree data =
 { nodeData = data
 , children = emptyChildren }

emptyChildren : Children
emptyChildren = 
  { upNodes = emptyForest
  , downNodes = emptyForest
  , overNodes = emptyForest
  }

emptyForest : Forest
emptyForest =
  Forest
    { trees = Dict.empty
    , order = LinearOrder.empty
    }  

{- this is not great because the type Children
and the type NodeKind are not coupled at all.
what is the right way to do this without writing a
new insert function for each kind of descendant 
we may want to add?-}
treeInsert : NodeData -> Location
             -> DisplayTree -> DisplayTree
treeInsert nodeData loc tree =
  case loc of
    [] -> Debug.crash "Beard.treeInsert: can't insert at root" 
    (id,kind)::loc' ->
      let new = forestInsertRec nodeData id loc'  
          children = tree.children
      in
      case kind of
        UpNode -> { tree | upNodes <- new children.upNodes }
        DownNode -> { tree | downNodes <- new children.downNodes }
        OverNode -> { tree | overNodes <- new children.overNode }

forestInsertRec : NodeData -> NodeID 
                  -> Location -> Forest -> Forest 
forestInsertRec nodeData id loc forest =
  case loc of 
    [] -> forestInsertNew nodeData id forest
    _ -> 
      case Dict.get id forest.trees of
        Nothing ->
         Debug.crash "Beard.forestInsertRec: no such location"
        Just tree -> 
          let newTree = treeInsert nodeData loc tree
              newTrees = 
                Dict.update id (always newTree) forest.trees
          in
          { forest | trees <- newTrees }
   
forestInsertNew nodeData id forest =
  if Dict.member id forest.trees 
  then Debug.crash "Bearh.forestInsertNew: id already exists"
  else 
    let newTree = { nodeData = nodeData
                  , children = emptyChildren }
        Dict.insert id newTree forest.trees



insert' : Object.ObjectInContext -> Location
          -> Beard -> Beard
insert' obj loc beard =
  case loc of
    [] -> let id = beard.freshID

          in { }


Debug.crash "Beard.insert: Location does not exist"

