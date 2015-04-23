import Graphics.Element exposing (..)
import Mouse

-- TODO: Lookup elm-html and stuff
main : Signal Element
main =
      Signal.map show Mouse.position
