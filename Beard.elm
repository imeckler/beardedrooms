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

--Beard funs

singleton : Object.ObjectInContext -> Beard
singleton obj =
  { freshID = 0
  , treeBeard = singletonTree obj } 

--ideally location shouldn't really be returned here
insert : Object.ObjectInContext -> Location ->
         NodeKind -> Beard -> (Location,Beard)
insert obj loc kind beard =
  let id = beard.freshID 
      loc' = loc ++ [(id,kind)]
      newTreeBeard = treeInsert (singletonTree obj) loc' beard.treeBeard 
  in 
  ( loc', { freshID = id + 1, treeBeard = newTreeBeard } )

delete : Location -> Beard -> Beard
delete loc beard =
  let (_,updated) = treeDelete loc beard.treeBeard
  in
  { beard | treeBeard <- updated }

move : Location -> Location -> NodeKind -> Beard -> Beard
move oldLoc newLoc kind beard =
  let (_,newBeard) = treeMove oldLoc newLoc kind beard.treeBeard
  in
  { beard | treeBeard <- newBeard }

--emptys

singletonTree : Object.ObjectInContext -> DisplayTree
singletonTree obj =
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

--basic tree operations

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
    Nothing   -> Debug.crash "Beard.forestGet: bad location, no such id"
    Just tree -> treeGet loc tree

treeDelete : Location -> DisplayTree -> (DisplayTree, DisplayTree)
treeDelete loc tree =
  let deleted = treeGet loc tree |> popLocation loc
      updated = treeModifyAt (deleteFromForest loc) loc tree
  in
  (deleted,updated)

deleteFromForest : Location -> Forest -> Forest
deleteFromForest loc (Forest forest) =
  let (id,_) = infoFromLoc loc
      newWorldOrder = LinearOrder.delete id forest.order
      newTrees = Dict.remove id forest.trees
  in
  Forest { forest | trees <- newTrees, order <- newWorldOrder }

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

--composed operations

treeMove : Location -> Location -> NodeKind -> DisplayTree -> (Location,DisplayTree)
treeMove oldPlace newParent kind tree =
  let (deleted,updated) = treeDelete oldPlace tree
      (id,_) = infoFromLoc oldPlace
      newPlace = newParent ++ [(id,kind)]
  in
  (newPlace, treeInsert deleted newPlace updated)

treeReorderAbove : Location -> NodeID -> DisplayTree -> DisplayTree
treeReorderAbove loc newPlace tree = 
  let (id,_) = infoFromLoc loc
      reorder : Forest -> Forest
      reorder (Forest forest) = 
        let newWorldOrder = LinearOrder.moveAbove id newPlace forest.order
        in
        Forest { forest | order <- newWorldOrder }
  in
  treeModifyAt reorder loc tree

treeReorderBelow : Location -> NodeID -> DisplayTree -> DisplayTree
treeReorderBelow loc newPlace tree = 
  let (id,_) = infoFromLoc loc
      reorder : Forest -> Forest
      reorder (Forest forest) = 
        let newWorldOrder = LinearOrder.moveBelow id newPlace forest.order
        in
        Forest { forest | order <- newWorldOrder }
  in
  treeModifyAt reorder loc tree

--if move to mulitdimensional, then parameterize this and the reordering funs
--by direction (reorder : ... -> Directino = Nodekind -> ...)
treeInsertAbove : DisplayTree -> Location -> NodeID -> DisplayTree -> DisplayTree
treeInsertAbove addTree loc id oldTree = 
  treeInsert addTree loc oldTree
  |> treeReorderAbove loc id 

treeInsertBelow : DisplayTree -> Location -> NodeID -> DisplayTree -> DisplayTree
treeInsertBelow addTree loc id oldTree = 
  treeInsert addTree loc oldTree
  |> treeReorderBelow loc id 

treeMoveAbove : Location -> Location -> DisplayTree -> DisplayTree
treeMoveAbove oldPlace newSibling tree =
  let (newParent,(id,kind)) = splitLoc newSibling
      (newPlace,updated) = treeMove oldPlace newParent kind tree
  in
  treeReorderAbove newPlace id tree

treeMoveBelow : Location -> Location -> DisplayTree -> DisplayTree
treeMoveBelow oldPlace newSibling tree =
  let (newParent,(id,kind)) = splitLoc newSibling
      (newPlace,updated) = treeMove oldPlace newParent kind tree
  in
  treeReorderBelow newPlace id tree

--probably more efficient to use modifyAt, but complexer
--need to return list of new children locations?
disownChildren : Location -> DisplayTree -> DisplayTree
disownChildren loc tree =
  let      
      disownNode : NodeKind -> NodeID -> DisplayTree -> DisplayTree
      disownNode kind id tree' = 
        let childLocation = loc ++ [(id,kind)]
            moveKind = case kind of
              UpNode   -> treeMoveAbove
              DownNode -> treeMoveBelow
              OverNode -> treeMoveAbove --shouldn't actually be used 
        in
        moveKind childLocation loc tree'
      children  = .children <| treeGet loc tree
      upOrder   = .order <| forestDrill children.upNodes 
      downOrder = .order <| forestDrill children.downNodes 
      --NOTE foldl vs foldr
      upsMoved      = List.foldl (disownNode UpNode)   tree     upOrder 
      upsDownsMoved = List.foldr (disownNode DownNode) upsMoved downOrder
  in
  upsDownsMoved

treeDeleteSingle : Location -> DisplayTree -> (DisplayTree,DisplayTree)
treeDeleteSingle loc tree =
  disownChildren loc tree 
  |> treeDelete loc

--not actually sure if this and other possible things like
--treeSingleMoveAbove are useful
treeMoveSingle : Location -> Location -> NodeKind -> DisplayTree -> (Location,DisplayTree)
treeMoveSingle oldPlace newParent kind tree =
  disownChildren oldPlace tree 
  |> treeMove oldPlace newParent kind 


--helper funs

forestDrill : Forest -> ActualForest
forestDrill (Forest forest) = forest

{-
orderInsert : NodeID -> NodeKind -> Order -> Order 
orderInsert id kind order =  
  case kind of
    DownNode -> LinearOrder.insertTop id order
    UpNode -> LinearOrder.insertBottom id order
    OverNode -> LinearOrder.insertBottom id order
-}

--check later if this factorization is helpful
--prob not, original motive was for disownChildren
outFromMiddle : NodeKind -> (Order -> v) -> Order -> v
outFromMiddle kind f order =
  case kind of
    DownNode -> List.reverse order |> f 
    UpNode -> f order
    OverNode -> f order

orderOutFromMiddle : NodeKind -> (Order -> Order) -> (Order -> Order)
orderOutFromMiddle kind f order =
  outFromMiddle kind f order
  |> outFromMiddle kind identity 
 
orderInsert : NodeID -> NodeKind -> Order -> Order 
orderInsert id kind order =  
  orderOutFromMiddle kind (LinearOrder.insertBottom id) order  

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

--these are just list funs... 

infoFromLoc : Location -> (NodeID,NodeKind) 
infoFromLoc loc =
  case List.head <| List.reverse loc of 
    Nothing -> Debug.crash "Beard.infoFromLoc was passed an empty location"
    Just (id,kind) -> (id,kind)

parentOfLoc : Location -> Location
parentOfLoc loc =
  case LinearOrder.heads loc of
    Nothing     -> Debug.crash "Beard.parentOfLoc: loc has no parent"
    Just parent -> parent

splitLoc : Location -> (Location,(NodeID,NodeKind))
splitLoc loc =
  (parentOfLoc loc, infoFromLoc loc)

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


