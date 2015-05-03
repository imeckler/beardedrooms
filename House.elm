module House where

import Digraph exposing (Digraph)
import Beard exposing (Beard)

type Direction
  = Up   | Down
  | In   | Out
  | Left | Right

-- TODO: Add data to edge data saying why edge exists (parent, etc.)
type alias House = Digraph Beard Direction

