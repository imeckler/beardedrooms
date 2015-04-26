module Bijection
  ( Bijection
  , empty, insert
  ) where

import Dict exposing (Dict)

type alias Bijection comparablel comparabler =
  { fromLeft : Dict.Dict comparablel comparabler
  , fromRight : Dict.Dict comparabler comparablel
  }

empty : Bijection comparablel comparabler
empty = { fromLeft = Dict.empty
        , fromRight = Dict.empty
        }

--does nothing if that edge conflicts on either side
insert : comparablel -> comparabler -> 
  Bijection comparablel comparabler -> Bijection comparablel comparabler  
insert l r bij = 
  if Dict.member l bij.fromLeft || Dict.member r bij.fromRight
  then bij    
  else let left = Dict.insert l r bij.fromLeft
           right = Dict.insert r l bij.fromRight
        in
           { fromLeft = left
           , fromRight = right
           }

--TODO: move over stuff from Dict
