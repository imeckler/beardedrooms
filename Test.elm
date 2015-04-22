import Graphics.Element exposing (..)
import Mouse

-- TODO: Lookup elm-html
main : Signal Element
main =
      Signal.map show Mouse.position
