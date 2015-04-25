module Room
  ( Room
  ) where

import Beard exposing (Beard)
import Object exposing (Object)

{- the type variable du(display unit) currently means 
a piece of html plus links, but might be different later.
this stuff is so factored because there will be
more stuff here, e.g. current focus (which might 
go in the beard itself), metadata for the room,
other metrics, etc.-}
type alias Room = 
  { beard : Beard.Beard
  , topLevel : List Object.ObjectInContext
  }
{- topLevel tracks the roots of the beard's forest,
for purposes of identifying the room -}

