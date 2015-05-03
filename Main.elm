module Main where

import House exposing (..)
import Dict exposing (..)
import Display exposing (Display(..), toggles, dbLinkClicks, Path)
import Http
import Digraph exposing (NodeID)
import Beard exposing (Beard)
import DB exposing (DBID)
import Task exposing (Task)
import Set
import Char
import Keyboard

type UserAction
  = NavigateToDBID DBID
  | NavigateToNode NodeID
  | ExpandHere DBID Path

type Update
  = UserAction UserAction
  | CreateAndNavigateTo Beard
  | ChangeDisplay (Display -> Display)
  | NoOp

updateBox : Signal.Mailbox Update
updateBox = Signal.mailbox NoOp

updates : Signal Update
updates =
  let aIsDown = Signal.map (Set.member (Char.toCode 'a')) Keyboard.keysDown

      beardAdds : Signal Update
      beardAdds = 
        keepWhen NoOp aIsDown
          (Signal.map (\mx -> case mx of
            Just (id,_) -> UserAction (NavigateToDBID id)
            _           -> NoOp)
            dbLinkClicks)

      beardGrowings =
        keepWhen NoOp (Signal.map not aIsDown)
          (Signal.map (\mx -> case mx of
            Just (id,path) -> UserAction (ExpandHere id path)
            _              -> NoOp)
            dbLinkClicks)
  in
  Signal.mergeMany
  [ Signal.map ChangeDisplay Display.toggles
  , Signal.map (\mx -> case mx of
      Just (dbID,path) -> UserAction (ExpandHere dbID path)
      _                -> NoOp)
      dbLinkClicks
  , beardAdds
  , beardGrowings
  , updateBox.signal
  ]


keepWhen : a -> Signal Bool -> Signal a -> Signal a
keepWhen x cond sx =
  Signal.map2 (,) cond sx
  |> Signal.filter (\(b, _) -> b) (True, x)
  |> Signal.map snd

type alias State =
  { house     : House
  , current   : { beard : Beard, node : NodeID }
  , instances : Dict DBID (List NodeID)
  }

update : Update -> State -> (Maybe (Task Http.Error ()), State)
update u s = case u of
  ChangeDisplay f ->
    let curr      = s.current
        currBeard = curr.beard
    in
    ( Nothing
    , {s | current <- {curr | beard <- {currBeard | display <- f currBeard.display }}}
    )

  CreateAndNavigateTo beard ->
    let s'             = saveCurrent s
        (node, house') = Digraph.addVertex beard s'.house
        instances'     =
          Dict.update beard.objectID
            (\ns -> Just (node :: Maybe.withDefault [] ns))
            s'.instances
    in
    ( Nothing
    , { s' | house <- house', instances <- instances', current <- {node = node, beard = beard} }
    )

  UserAction (NavigateToDBID id) ->
    case Dict.get id s.instances of
      Just (node :: _) ->
        let s' = saveCurrent s in
        ( Nothing
        , { s' | current <- {node = node, beard = Digraph.getValueExn node s'.house} }
        )

      _ ->
        ( Just <|
            Http.get Beard.ofJson (urlForId id) `Task.andThen` \beard ->
              Signal.send updateBox.address (CreateAndNavigateTo beard)
        , s 
        )

  UserAction (ExpandHere id path) ->
    let task =
          Http.get Beard.ofJson (urlForId id) `Task.andThen` \beard ->
            Signal.send updateBox.address <| ChangeDisplay <|
              Display.modifyAt (\(Display d) -> Display {d | children <- d.children ++ [beard.display]})
                path
    in
    (Just task, s)

saveCurrent : State -> State
saveCurrent s =
  let house' = Digraph.setValueUnchecked s.current.node s.current.beard s.house in
  { s | house <- house' }

urlForId id = "http://localhost:3333/" ++ toString id

