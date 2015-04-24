module Digraph
    ( Digraph, empty, addVertex, addEdge
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

--index points to/from the vertex, comparable labels that edge
type alias EdgeList comparable
    = S.Set (comparable, Index) 

empty : Digraph v comparable
empty = Digraph 0 D.empty

addVertex : v -> Digraph v comparable ->  Digraph v comparable
addVertex v (Digraph m d) 
    = let d' = D.insert m (Vertex v S.empty S.empty) d 
      in
        Digraph (m+1) d' 

{- this error thing doesn't work, but can't find
good documentation on error handling. can't let it slide
or you get e.g. edges pointing to nonexistent vertices. -}
addEdge : comparable -> (Index,Index) -> Digraph v comparable ->  Digraph v comparable
addEdge comparable (s,t) (Digraph m d) 
    = if D.member s d && D.member t d
         then let d' = D.update s (addOutEdge comparable t) d
                  d'' = D.update t (addInEdge comparable s) d'
              in   
                 Digraph m d''
         else Native.Debug.crash "can't add edge to/from nowhere"

addOutEdge : comparable -> Index -> Maybe (Vertex v comparable) -> Maybe (Vertex v comparable)
addOutEdge comparable t mv
    = case mv of 
        Nothing -> Nothing
        (Just (Vertex v ins outs)) -> Just (Vertex v ins (S.insert (comparable, t) outs))

addInEdge : comparable -> Index -> Maybe (Vertex v comparable) -> Maybe (Vertex v comparable)
addInEdge comparable s mv
    = case mv of  
        Nothing -> Nothing
        (Just (Vertex v ins outs)) -> Just (Vertex v (S.insert (comparable, s) ins) outs)
