module Room
  ( Room
  ) where

{- the type variable du(display unit) currently means 
a piece of html plus links, but might be different later.
this stuff is so factored because there might be
more stuff here, e.g. current focus (which might 
go in the beard itself), metadata for the room,
other metrics, etc.-}
type alias Room du = 
  { beard : Beard du }

