module Beard 
  ( ..
  ) where

import Object exposing (Object) 
import LinearOrder exposing (LinearOrder)
import Dict exposing (Dict)
import Debug

type alias Beard =
  { freshID : NodeID
  , treeBeard : DisplayTree 
  }
--  , currentFocus : NodeID --or Location?

type alias DisplayTree =
  { nodeData : NodeData
  , children : Children
  }

type alias NodeData =
  { value : Object.ObjectInContext
  , location : Location
  }

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
  { freshID = 0
  , treeBeard = singletonTree nodeData } 

insert : Object.ObjectInContext -> Location ->
         NodeKind -> Beard -> (Location,Beard)
insert obj loc kind beard =
  let id = beard.freshID 
      loc' = loc ++ [(id,kind)]
      newTree = singletonTree { value = obj, location = loc'}
      newTreeBeard = treeInsert newTree loc' beard.treeBeard 
  in 
  ( loc', { freshID = id + 1, treeBeard = newTreeBeard } )

{-
delete : Location -> Beard -> Beard
delete loc beard =
  { beard | treeBeard <- treeDelete loc beard.treeBeard }

move : Location -> Location -> Beard -> Beard
move oldLoc newLoc beard =
  { beard | treeBeard <- treeMove oldLoc newLoc beard.treeBeard }

treeMove : Location -> Location -> DisplayTree
treeMove oldLoc newLoc tree =
  let (beard',tree) = treeDelete oldLoc beard
  in 
  insert
-}

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

treeModifyAt : (Forest -> Forest) -> Location -> DisplayTree ->
  DisplayTree
treeModifyAt update loc oldTree =
  [] -> Debug.crash "Beard.treeInsert: can't modify at root" 
  (id,kind)::loc' ->
    let new = forestModifyAt update id loc'  
        children = oldTree.children
        newchildren =
          case kind of
            UpNode ->
              { children | upNodes <- new children.upNodes }
            DownNode ->
              { children | downNodes <- new children.downNodes }
            OverNode ->
              { children | overNodes <- new children.overNodes }
   in 
   { oldTree | children <- newchildren }

forestModifyAt : (Forest -> Forest) -> NodeID -> Location -> Forest -> Forest 
forestModifyAt update id loc (Forest forest) =
  case loc of 
    [] -> update (Forest forest)
    _ -> case Dict.get id forest.trees of
       Nothing ->
         Debug.crash "Beard.forestInsertRec: no such location"
       Just tree -> 
         let updatedTree = treeModifyAt update loc tree
             newTrees = Dict.update id (\_ -> Just updatedTree) forest.trees
         in
         Forest { forest | trees <- newTrees }
 





treeInsert : DisplayTree -> Location -> DisplayTree -> DisplayTree
treeInsert newTree loc oldTree = case loc of
  [] -> Debug.crash "Beard.treeInsert: can't insert at root" 
  (id,kind)::loc' ->
    let new = forestInsertRec newTree id kind loc'  
        children = oldTree.children
        newchildren =
          case kind of
            UpNode ->
              { children | upNodes <- new children.upNodes }
            DownNode ->
              { children | downNodes <- new children.downNodes }
            OverNode ->
              { children | overNodes <- new children.overNodes }
   in 
   { oldTree | children <- newchildren }

forestInsertRec : DisplayTree -> NodeID -> NodeKind -> Location -> Forest -> Forest 
forestInsertRec newTree id kind loc (Forest forest) =
  case loc of 
    [] -> forestInsertNew newTree id kind (Forest forest)
    _ -> case Dict.get id forest.trees of
       Nothing ->
         Debug.crash "Beard.forestInsertRec: no such location"
       Just tree -> 
         let updatedTree = treeInsert newTree loc tree
             newTrees = Dict.update id (\_ -> Just updatedTree) forest.trees
         in
         Forest { forest | trees <- newTrees }
   
forestInsertNew : DisplayTree -> NodeID -> NodeKind -> Forest -> Forest
forestInsertNew newTree id kind (Forest forest) =
  if Dict.member id forest.trees 
  then Debug.crash "Bearh.forestInsertNew: id already exists"
  else 
    let newTrees = Dict.insert id newTree forest.trees
        newOrder = orderInsert id kind forest.order
    in
    Forest { trees = newTrees, order = newOrder }

orderInsert : NodeID -> NodeKind -> Order -> Order 
orderInsert id kind order =  
  case kind of
    DownNode -> LinearOrder.insertTop id order
    UpNode -> LinearOrder.insertBottom id order
    OverNode -> LinearOrder.insertBottom id order
 
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


