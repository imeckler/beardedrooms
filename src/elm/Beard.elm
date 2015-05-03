module Beard 
  ( Beard 
  , ofJson
  ) where

import Display exposing (Display)
import DB exposing (DBID)
import Json.Decode as Json exposing ((:=))

type alias Beard =
  { objectID : DBID
  , display  : Display
  }

ofJson : Json.Decoder Beard
ofJson = Json.object2 Beard
  ("objectID" := Json.string)
  ("display"  := Display.ofJson)

