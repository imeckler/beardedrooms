module Digraph
  ( Digraph, empty, addVertex, addEdge, setValue, setValueUnchecked
  , getData, getValue, getIncoming, getOutgoing
  , getValueExn
  , NodeID
  ) where

import Dict exposing (Dict)
import Set exposing (Set)
import Debug

{- a Digraph is a dictionary of indexed vertices.
    the leading index records the maximum index, 
    so all the indices are unique. this is annoying
   but Dict does not expose the max
  function. v is the type
    of values stored in each vertex, and e
    is the type of edge labels. 
-}
type alias Digraph v e =
  { freshID : NodeID
  , graph   : Dict NodeID (VertexData v e)
  }
 
type alias NodeID = Int

type alias VertexData v e = 
  { incoming : Dict NodeID e
  , outgoing : Dict NodeID e
  , value    : v
  }

empty : Digraph v e
empty = { freshID = 0, graph = Dict.empty }

addVertex : v -> Digraph v e -> (NodeID, Digraph v e)
addVertex v g =
  let vId = g.freshID
      gr' = Dict.insert vId { incoming = Dict.empty, outgoing = Dict.empty, value = v } g.graph
  in
  (vId, { freshID = vId + 1, graph = gr' })

{- edge data e means stuff like, "the target was actually
created by pushing down from the source", "the source was
the most recent pushdown to target", "the author of this document
designated the target as the primary result for pushing down
from the source", and so on -}
addEdge : NodeID -> NodeID -> e -> Digraph v e -> Digraph v e
addEdge from to e g =
  if Dict.member from g.graph && Dict.member to g.graph
  then
    let gr' =
          Dict.update from (Maybe.map (addOutgoing to e)) g.graph
          |> Dict.update to (Maybe.map (addIncoming from e))
    in
    { g | graph <- gr' }
  else Debug.crash "Digraph.addEdge: Nodes not in graph"

addOutgoing : NodeID -> e -> VertexData v e -> VertexData v e
addOutgoing suc e vd = { vd | outgoing <- Dict.update suc (\_ -> Just e) vd.outgoing }

addIncoming : NodeID -> e -> VertexData v e -> VertexData v e
addIncoming pred e vd = { vd | incoming <- Dict.update pred (\_ -> Just e) vd.incoming }

getData : NodeID -> Digraph v e -> Maybe (VertexData v e)
getData v g = Dict.get v g.graph

setValue : NodeID -> v -> Digraph v e -> Maybe (Digraph v e)
setValue v x g = case Dict.get v g.graph of
  Just vd -> Just {g | graph <- Dict.insert v {vd | value <- x} g.graph}
  Nothing -> Nothing

-- Set the value for the given NodeID, returning the original graph if it
-- wasn't present.
setValueUnchecked : NodeID -> v -> Digraph v e -> Digraph v e
setValueUnchecked v x g = {g | graph <- Dict.update v (Maybe.map (\vd -> {vd | value <- x})) g.graph}

getValue : NodeID -> Digraph v e -> Maybe v
getValue v = Maybe.map .value << getData v

getValueExn : NodeID -> Digraph v e -> v
getValueExn v g = case getValue v g of
  Just x  -> x
  Nothing -> Debug.crash "valueOfNodeExn: Node not in graph"

getOutgoing : NodeID -> Digraph v e -> Maybe (Dict NodeID e)
getOutgoing v = Maybe.map .outgoing << getData v

getIncoming : NodeID -> Digraph v e -> Maybe (Dict NodeID e)
getIncoming v = Maybe.map .incoming << getData v

