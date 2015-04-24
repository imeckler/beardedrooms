module Digraph
    ( Digraph, empty
    {- TODO: addNode, listNodes, addEdge, getEdges, etc. -}
    ) where

import Dict as D
import Set as S

{- a Digraph is a dictionary of indexed vertices.
    the leading index records the maximum index, 
    so all the indices are unique. this is annoying
   but Dict does not expose the max
  function. v is the type
    of values stored in each vertex, and e
    is the type of edge labels. 
-}
type Digraph v comparable
    = Digraph Index (D.Dict Index (Vertex v comparable))

type alias Index = Int

--a vertex has a value, a set of labeled incoming 
--edges, and a set of outgoing edges
type Vertex v comparable
    = Vertex v (EdgeList comparable) (EdgeList comparable)

type alias EdgeList comparable
    = S.Set (Index, comparable) 

empty : Digraph v comparable
empty = Digraph 0 D.empty

{--}
addVertex : v -> Digraph v comparable ->  Digraph v comparable
addVertex v (Digraph m d) 
    = let d' = D.insert (m+1) (Vertex v S.empty S.empty) d 
      in
        Digraph (m+1) d' 
--}
