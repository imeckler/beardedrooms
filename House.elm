module House where

import Digraph (Digraph)
import Bead (Beard)

type Direction
  = Up   | Down
  | In   | Out
  | Left | Right

type alias House = Digraph Beard Direction

