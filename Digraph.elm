module Digraph
  ( Digraph, empty, addVertex, addEdge
  {- TODO: addNode, listNodes, addEdge, getEdges, etc. -}
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

-- a vertex has a value, a set of labeled incoming 
-- edges, and a set of outgoing edges
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
          Dict.update from (Maybe.map (addSuccessor to e)) g.graph
          |> Dict.update to (Maybe.map (addPredecessor from e))
    in
    { g | graph <- gr' }
  else Debug.crash "Digraph.addEdge: Nodes not in graph"

addSuccessor : NodeID -> e -> VertexData v e -> VertexData v e
addSuccessor suc e vd = { vd | outgoing <- Dict.update suc (\_ -> Just e) vd.outgoing }

addPredecessor : NodeID -> e -> VertexData v e -> VertexData v e
addPredecessor pred e vd = { vd | incoming <- Dict.update pred (\_ -> Just e) vd.incoming }


