module Beard 
  ( ..
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
  , treeBeard : DisplayTree --leave this, or the Ulairi will get you
--  , currentFocus : NodeID --or Location?
  }

type alias DisplayTree =
  { nodeData : NodeData
  , children : Children
  }

type alias NodeData =
  { value : Object.ObjectInContext
  , location : Location
  }
 
idFromData : NodeData -> NodeID
idFromData nodeData =
  case nodeData.location |> List.reverse |> List.head of
    Nothing -> Debug.crash "Beard.idFromData: node has no location"
    Just (id,_) -> id

kindFromData : NodeData -> NodeKind
kindFromData nodeData =
  case nodeData.location |> List.reverse |> List.head of
    Nothing -> Debug.crash "Beard.kindFromData: node has no location"
    Just (_,kind) -> kind

type NodeKind = UpNode | DownNode | OverNode --more

type alias NodeID = Int

{- location [(3,Up),(6,Down)] means i'm the 
DownNode labelled 6 of the UpNode labelled 3 
of the root -}
type alias Location = List (NodeID,NodeKind)

type alias Children =
  { upNodes : Forest
  , downNodes : Forest
  , overNodes : Forest
  --maybe left, right and under nodes
  }   
 
type Forest = 
  Forest
    { trees : Dict.Dict NodeID DisplayTree
    , order : Order
    }

type alias Order = LinearOrder.LinearOrder NodeID

singleton : Object.ObjectInContext -> Beard
singleton obj =
  let nodeData =
        { value = obj, location = [] } 
  in
  { freshID = 1
  , treeBeard = singletonTree nodeData 
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
          newchildren =
           case kind of
            UpNode ->
              { children
              | upNodes <- new children.upNodes }
            DownNode ->
              { children
              | downNodes <- new children.downNodes }
            OverNode ->
              { children
              | overNodes <- new children.overNodes }
      in 
      { tree | children <- newchildren }

forestInsertRec : NodeData -> NodeID 
                  -> Location -> Forest -> Forest 
forestInsertRec nodeData id loc (Forest forest) =
  case loc of 
    [] -> forestInsertNew nodeData id (Forest forest)
    _ -> 
      case Dict.get id forest.trees of
        Nothing ->
         Debug.crash "Beard.forestInsertRec: no such location"
        Just tree -> 
          let newTree = treeInsert nodeData loc tree
              newTrees = 
                Dict.update id (\_ -> Just newTree) forest.trees
          in
          Forest { forest | trees <- newTrees }
   
forestInsertNew : NodeData -> NodeID -> Forest -> Forest
forestInsertNew nodeData id (Forest forest) =
  if Dict.member id forest.trees 
  then Debug.crash "Bearh.forestInsertNew: id already exists"
  else 
    let newTree = singletonTree nodeData
        newTrees = Dict.insert id newTree forest.trees
        newOrder = orderInsert nodeData id forest.order
    in
    Forest { trees = newTrees, order = newOrder }

orderInsert : NodeData -> NodeID  -> Order -> Order 
orderInsert nodeData id order =  
  case kindFromData nodeData of
    DownNode -> LinearOrder.insertTop id order
    UpNode -> LinearOrder.insertBottom id order
    OverNode -> LinearOrder.insertBottom id order

{-
insert' : Object.ObjectInContext -> Location
          -> Beard -> Beard
insert' obj loc beard =
  case loc of
    [] -> let id = beard.freshID

          in { }
-}

