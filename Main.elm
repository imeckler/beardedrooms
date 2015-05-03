module Main where

import House exposing (..)
import Dict exposing (..)
import Display exposing (Display, displayUpdates)
import Http
import Digraph

type Update
  = UpdateDisplay (Display -> Display)
  | NavigateTo DBID
  | CreateAndNavigateTo Beard

updates : Signal Update
updates =
  Signal.mergeMany
  [ Signal.map UpdateDisplay displayUpdates
  , Signal.map NavigateTo dbLinkClicks
  ,
  ]

type alias State =
  { house     : House
  , current   : { beard : Beard, node : NodeID }
  , instances : Dict DBID (List NodeID)
  }

update : Update -> State -> (Maybe (Task e ()), State)
update u s = case u of
  UpdateDisplay f ->
    let curr      = s.current
        currBeard = curr.beard
    in
    ( Nothing
    , {s | current <- {curr | beard <- {currBeard | display <- f currBeard.display }}}
    )

  CreateAndNavigateTo beard
    let s'             = saveCurrent s
        (node, house') = Digraph.addVertex beard s'.house
        instances'     = Dict.update beard.id (\ns -> Just (node :: Maybe.withDefault [] ns))
    in
    ( Nothing
    , { s' | house <- house', instances <- instances', current <- beard }
    )

  NavigateTo id ->
    case Dict.get id s.instances of
      Just (node :: _) ->
        let s' = saveCurrent s in
        ( Nothing
        , { s' | current <- {node = node, beard = Digraph.valueOfNodeExn node s.house} }
        )

      _ ->
        ( Just Http.getString (urlForId id)
        , s 
        )

saveCurrent : State -> State
saveCurrent s =
  let house' = Digraph.setValueUnchecked s.current.node s.current.beard s.house in
  { s | house <- house' }

-- outgoing
port viewUpdates : Signal ()
port viewUpdates = Signal.map (\_ -> ()) displayUpdates

port dbLinkClicks : Signal DBID

urlForId id = "http://localhost:3333/" ++ toString id

port responses
