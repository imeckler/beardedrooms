module Display where

import Html exposing (..)
import Html.Events exposing (onClick)
import Debug

type Display
  = Section
    { heading  : String
    , children : List Display
    , open     : Bool
    }
  | Html Html

type alias Path = List Int

slideTo : Int -> List a -> Maybe (List a, a, List a)
slideTo =
  let go pre n xs = case xs of
        []       -> Nothing
        x :: xs' ->
          if n == 0
          then Just (List.reverse pre, x, xs)
          else go (x::pre) (n - 1) xs
  in
  go []
  
toggleAt : Path -> Display -> Display
toggleAt p d = case (p, d) of
  (_, Html)          -> Debug.crash "Display.toggleAt: Invalid path. Ends at Html."
  ([], Section r)    -> Section {r | open <- not r.open }
  (i::p', Section r) -> case slideTo i r.children of
    Nothing             -> Debug.crash "Display.toggleAt: Invalid path. Not enough children."
    Just (pre, c, post) -> Section {r | children <- pre ++ (toggleAt p' c) :: post }

displayUpdateMailbox : Signal.Mailbox (Display -> Display)
displayUpdateMailbox = Signal.mailbox identity

displayUpdates : Signal (Display -> Display)
displayUpdates = displayUpdateMailbox.signal

toHtml : Display -> Html
toHtml =
  let addr = displayUpdateMailbox.address
      go p depth d = case d of
        Section {heading, children, open} ->
          div []
          ( node ("h" ++ show depth)
            [ onClick addr (toggleAt (List.reverse p)) ] -- TODO: Something to optimize if need be
            [ Html.text heading ]
          ::
          if open
          then [div [] (List.indexedMap (\i -> go (i::p) (depth + 1)) children)]
          else [])

        Html h -> h
  in
  go [] 1

