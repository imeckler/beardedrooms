module Protocol where

import DB exposing (DBID)

type FromServer
  = Object Beard
  | Ok

type ToServer
  = GetObject DBID

