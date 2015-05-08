module Beard 
  ( ..
  ) where

import Object exposing (Object) 
import LinearOrder exposing (LinearOrder)
import Dict exposing (Dict)
import Debug
import Json.Decode as Json exposing ((:=))

type alias Beard =
  { freshID : NodeID
  , treeBeard : DisplayTree 
  }
--  , currentFocus : NodeID --or Location?

ofJson : Json.Decoder Beard
ofJson = Json.object2 Beard
  ("freshID"   := Json.int)
  ("treeBeard" := displayTreeOfJson)

type alias DisplayTree =
  { nodeData : NodeData
  , children : Children
  }

displayTreeOfJson : Json.Decoder DisplayTree
displayTreeOfJson = Json.object2 DisplayTree
  ("nodeData" := nodeDataOfJson)
  ("children" := childrenOfJson)

type alias NodeData =
  { value : Object.ObjectInContext
  , location : Location
  }

nodeDataOfJson : Json.Decoder NodeData
nodeDataOfJson = Json.object2 NodeData
  ("value"    := Json.string)
  ("location" := locationOfJson)

type NodeKind = UpNode | DownNode | OverNode --more

nodeKindOfJson : Json.Decoder NodeKind
nodeKindOfJson = \kind -> case kind of 
    UpNode   -> Json.string "UpNode"
    DownNode -> Json.string "DownNode"
    OverNode -> Json.string "OverNode"

type alias NodeID = Int

{- location [(3,Up),(6,Down)] means i'm the 
DownNode labelled 6 of the UpNode labelled 3 
of the root -}
type alias Location = List (NodeID,NodeKind)

locationOfJson : Json.Decoder Location
locationOfJson = Json.list <| Json.object2 (,)
  ("id"   := Json.int)
  ("kind" := nodeKindOfJson)

type alias Children =
  { upNodes   : Forest
  , downNodes : Forest
  , overNodes : Forest
  --maybe left, right and under nodes
  }   
 
childrenOfJson : Json.Decoder Children 
childrenOfJson = Json.object3 Children
  ("upNodes"   := forestOfJson)
  ("downNodes" := forestOfJson)
  ("overNodes" := forestOfJson)

--because can't have totally openended recursive records
type Forest = Forest ActualForest

forestOfJson : Json.Decoder Forest
forestOfJson = \(Forest forest) -> actualForestOfJson forest

type alias ActualForest =
  { trees : Dict.Dict NodeID DisplayTree
  , order : Order
  }

actualForestOfJson : Json.Decoder ActualForest
actualForestOfJson = Json.object2 ActualForest
  ("trees" := Json.null)
  ("order" := orderOfJson)

type alias Order = LinearOrder.LinearOrder NodeID

orderOfJson : Json.Decoder Order
orderOfJson = Json.list Json.int

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
treeModifyAt update loc oldTree = case loc of 
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
treeInsert addTree loc oldTree = case loc of
  [] -> Debug.crash "Beard.treeInsert: can't insert at root" 
  _  ->
    let 
      newTree = pushLocation loc addTree
      updateForest = treeIntoForest newTree loc
    in 
    treeModifyAt updateForest loc oldTree

treeIntoForest : DisplayTree -> Location -> Forest -> Forest
treeIntoForest tree loc (Forest forest) =
  case List.head <| List.reverse loc of 
    Nothing -> Debug.crash "Beard.treeIntoForest was passed an empty location"
    Just (id,kind) -> 
      if Dict.member id forest.trees 
      then Debug.crash "Beard.treeIntoForest: id already exists"
      else 
        let newTrees = Dict.insert id tree forest.trees
            newOrder = orderInsert id kind forest.order
        in
        Forest { trees = newTrees, order = newOrder }

{-
treeReorderAbove : Location -> NodeID -> DisplayTree -> DisplayTree
treeReorderAbove loc id oldTree = 
  treeModifyAt fun loc oldTree
  fun stuff (Forest forest) = 
    { forest | trees <- forest.trees
-}  

orderInsert : NodeID -> NodeKind -> Order -> Order 
orderInsert id kind order =  
  case kind of
    DownNode -> LinearOrder.insertTop id order
    UpNode -> LinearOrder.insertBottom id order
    OverNode -> LinearOrder.insertBottom id order
 
nodeModifyLocation : (Location -> Location) -> NodeData -> NodeData
nodeModifyLocation update node = 
  { node | location <- update node.location }

treeMap : (NodeData -> NodeData) -> DisplayTree -> DisplayTree
treeMap update tree =
  let newNodeData = update tree.nodeData
      forestUpdate = forestMap update 
      oldChildren = tree.children 
      newChildren = 
        { oldChildren
            | upNodes   <- forestUpdate tree.children.upNodes
            , downNodes <- forestUpdate tree.children.downNodes
            , overNodes <- forestUpdate tree.children.overNodes
        }
  in 
  { tree | nodeData <- newNodeData, children <- newChildren }

forestMap : (NodeData -> NodeData) -> Forest -> Forest
forestMap update (Forest forest) =
  Forest { forest | trees <- Dict.map (\_ -> treeMap update) forest.trees }

popLocation : Location -> DisplayTree -> DisplayTree
popLocation loc tree =
  let 
    munchLoc popped loc' = case (popped, loc') of 
      ([],_) -> loc' 
      (_,[]) -> Debug.crash "Beard.popLocation: can't pop from empty location"
      (pop::ps, loc'::ls) ->
        if pop == loc'
           then munchLoc ps ls
           else Debug.crash "Beard.popLocation: can't pop a non-initial-segment"
  in 
  treeMap (nodeModifyLocation <| munchLoc loc) tree

pushLocation : Location -> DisplayTree -> DisplayTree
pushLocation loc tree =
  treeMap (nodeModifyLocation <| (++) loc) tree

{-
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
  then Debug.crash "Beard.forestInsertNew: id already exists"
  else 
    let newTrees = Dict.insert id newTree forest.trees
        newOrder = orderInsert id kind forest.order
    in
    Forest { trees = newTrees, order = newOrder }
-}

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


