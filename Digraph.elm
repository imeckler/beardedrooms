module Digraph
    ( Digraph
    {- TODO: addNode, listNodes, addEdge, getEdges, etc. -}
    ) where


type Digraph val edge
    = Array (val,Array edge)
