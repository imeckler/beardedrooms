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
  { upNodes   : Forest
  , downNodes : Forest
  , overNodes : Forest
  --maybe left, right and under nodes
  }   

--because can't have totally openended recursive records
type Forest = Forest ActualForest

type alias ActualForest =
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
      newTreeBeard = treeInsert (emptyTree obj) loc' beard.treeBeard 
  in 
  ( loc', { freshID = id + 1, treeBeard = newTreeBeard } )

delete : Location -> Beard -> Beard
delete loc beard =
  { beard | treeBeard <- treeDelete loc beard.treeBeard }

move : Location -> Location -> Beard -> Beard
move oldLoc newLoc beard =
  { beard | treeBeard <- treeMove oldLoc newLoc beard.treeBeard }

emptyTree : NodeData -> DisplayTree
emptyTree obj =
 { nodeData = { value = obj, location = [] }
 , children = emptyChildren }

emptyChildren : Children
emptyChildren = 
  { upNodes   = emptyForest
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
    _  -> case Dict.get id forest.trees of
       Nothing ->
         Debug.crash "Beard.forestInsertRec: no such location"
       Just tree -> 
         let updatedTree = treeModifyAt update loc tree
             newTrees = Dict.update id (\_ -> Just updatedTree) forest.trees
         in
         Forest { forest | trees <- newTrees }
 
treeGet : Location -> DisplayTree -> DisplayTree
treeGet loc tree = case loc of
  []              -> tree
  (id,kind)::loc' ->
    let get = forestGet id loc'  
        children = tree.children
    in 
    case kind of
      UpNode   -> get children.upNodes
      DownNode -> get children.downNodes
      OverNode -> get children.overNodes

forestGet : NodeID -> Location -> Forest -> DisplayTree
forestGet id loc (Forest forest) =
  case Dict.get id forest.trees of
    Nothing   -> Debug.get "Beard.forestGet: bad location, no such id"
    Just tree -> treeGet loc tree

treeDelete : Location -> DisplayTree -> (DisplayTree, DisplayTree)
treeDelete loc tree =
  let deleted = treeGet loc tree
      updated = treeModifyAt (treeFromForest loc) loc tree
  in
  (deleted,updated)

treeFromForest : Location -> Forest -> Forest
treeFromForest loc (Forest forest) =
  let (id,_) = infoFromLoc loc
      newWorldOrder = LinearOrder.delete (idFromLoc loc) forest.order

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
  let (id,kind) = infoFromLoc loc
  in
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

infoFromLoc : Location -> (NodeID,NodeKind) 
infoFromLoc loc =
  case List.head <| List.reverse loc of 
    Nothing -> Debug.crash "Beard.infoFromLoc was passed an empty location"
    Just (id,kind) -> (id,kind)

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


{-

ofJson : Json.Decoder Beard
ofJson = Json.object2 Beard
  ("freshID"   := Json.int)
  ("treeBeard" := displayTreeOfJson)

displayTreeOfJson : Json.Decoder DisplayTree
displayTreeOfJson = Json.object2 DisplayTree
  ("nodeData" := nodeDataOfJson)
  ("children" := childrenOfJson)

nodeDataOfJson : Json.Decoder NodeData
nodeDataOfJson = Json.object2 NodeData
  ("value"    := Json.string)
  ("location" := locationOfJson)

nodeKindOfJson : Json.Decoder NodeKind
nodeKindOfJson = Json.null UpNode
{-
nodeKindOfJson = Json.customDecoder 
  (\kind -> case kind of 
  UpNode   -> Json.string "UpNode"
  DownNode -> Json.string "DownNode"
  OverNode -> Json.string "OverNode"
  )
-}

locationOfJson : Json.Decoder Location
locationOfJson = Json.list <| Json.object2 (,)
  ("id"   := Json.int)
  ("kind" := nodeKindOfJson)

childrenOfJson : Json.Decoder Children 
childrenOfJson = Json.object3 Children
  ("upNodes"   := forestOfJson)
  ("downNodes" := forestOfJson)
  ("overNodes" := forestOfJson)

forestOfJson : Json.Decoder Forest
forestOfJson = Json.map Forest actualForestOfJson

actualForestOfJson : Json.Decoder ActualForest
actualForestOfJson = Json.object2 ActualForest
  ("trees" := Json.null Dict.empty ) --TODO
  ("order" := orderOfJson)

orderOfJson : Json.Decoder Order
orderOfJson = Json.list Json.int

-}

