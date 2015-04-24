module Beard 
  ( Beard 
  ) where

type alias Beard du =
  List (DisplayTree du)

{- not sure if the children should be in
a record like this; will be nicer than
having a 6-ary constructor, but maybe it is unsafe because
extending the record would break the show beard function? -}
type DisplayTree du =
  Empty
  | Node du { upNodes : List DisplayTree du
            , downNodes : List DisplayTree du
            , overNodes : List DisplayTree du
            --maybe left and right nodes
            }   


