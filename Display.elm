module Display where

import Html exposing (..)
import Html.Events exposing (onClick)
import Debug
import Json.Decode as Json exposing ((:=))
import Html.Attributes
import Dict exposing (Dict)
import DB exposing (DBID)
import Regex
import Util exposing (..)

type PreHtml
  = Node { tag : String, attributes : Dict String String, children : List PreHtml }
  | Text String

type Display
  = Display
    { preview  : PreHtml
    , content  : PreHtml
    , children : List Display
    , folded   : Bool
    }

-- For now, we represent HTML as Json in the following way.
-- { "tag" : String, "attributes": {String: String}, "children" : Array HTML }

preHtmlOfJson : Json.Decoder PreHtml
preHtmlOfJson =
  Json.oneOf
  [ Json.map Text Json.string
  , Json.object3 (\tag attrs cs -> Node { tag=tag, attributes=attrs, children=cs })
      ("tag"        := Json.string)
      ("attributes" := Json.dict Json.string)
      ("children"   := Json.list (recJson (\() -> preHtmlOfJson)))
  ]

-- hack to get around elm's inability to handle rec. values
recJson thunk = Json.customDecoder Json.value (\v -> Json.decodeValue (thunk ()) v)

ofJson : Json.Decoder Display
ofJson =
  Json.object4 (\p c cs f -> Display { preview = p, content = c, children = cs, folded = f })
    ("preview"  := preHtmlOfJson)
    ("content"  := preHtmlOfJson)
    ("children" := Json.list (recJson (\() -> ofJson)))
    ("folded"   := Json.bool)

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
toggleAt = modifyAt (\(Display d) -> Display {d | folded <- not d.folded})

modifyAt : (Display -> Display) -> Path -> Display -> Display
modifyAt f p (Display d) = case p of
  []      -> f (Display d)
  i :: p' -> case slideTo i d.children of
    Nothing             -> Debug.crash "Display.modifyAt: Invalid path. Not enough children"
    Just (pre, c, post) -> Display { d | children <- pre ++ modifyAt f p' c :: post }

displayUpdateMailbox : Signal.Mailbox (Display -> Display)
displayUpdateMailbox = Signal.mailbox identity

dbLinkClicksBox : Signal.Mailbox (Maybe (DBID, Path))
dbLinkClicksBox = Signal.mailbox Nothing

dbLinkClicks = dbLinkClicksBox.signal

toggles : Signal (Display -> Display)
toggles = displayUpdateMailbox.signal

toHtml : Display -> Html
toHtml =
  let addr = displayUpdateMailbox.address
      dictToAttrList = List.map (\(k,v) -> Html.Attributes.attribute k v) << Dict.toList
      matchVelUrl =
        let velUrlRegex = Regex.regex "vel://(.*)" in
        \u -> case Regex.find (Regex.AtMost 1) velUrlRegex u of
          m :: _ -> nth 1 m.submatches `Maybe.andThen` identity
          []     -> Nothing

      preHtmlToHtml path =
        let go pre = case pre of
          Text s -> Html.text s
          Node d ->
            if | d.tag == "a" -> 
                case Dict.get "href" d.attributes of
                  Nothing  -> Html.node "a" (dictToAttrList d.attributes) (List.map go d.children)
                  Just url -> 
                    let onClickAttr =
                          Maybe.map (\dbID -> onClick dbLinkClicksBox.address (Just (dbID, path)))
                            (matchVelUrl url)
                    in
                    Html.node "a" (maybeCons onClickAttr <| dictToAttrList d.attributes)
                      (List.map go d.children)
               | otherwise ->
                Html.node d.tag (dictToAttrList d.attributes) (List.map go d.children)
        in
        go

      go p depth (Display d) =
        if not d.folded
        then div [] [preHtmlToHtml p d.content, div [] (List.indexedMap (\i -> go (i::p) (depth + 1)) d.children)]
        else preHtmlToHtml p d.preview
  in
  go [] 1

