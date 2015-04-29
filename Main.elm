module Main where

import House exposing (..)
import Display exposing (Display, displayUpdates)

type Update
  = UpdateDisplay (Display -> Display)
  | AddNeighbor 

type alias State = House

