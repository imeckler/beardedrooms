module Beard 
  ( Beard 
  ) where

import Display exposing (Display)
import DB exposing (DBID)

type alias Beard =
  { freshID  : DBID
  , display  : Display
  }

