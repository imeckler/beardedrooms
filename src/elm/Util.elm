module Util where

nth : Int -> List a -> Maybe a
nth n xs = case xs of
  []       -> Nothing
  x :: xs' -> if n == 0 then Just x else nth (n - 1) xs'

maybeCons : Maybe a -> List a -> List a
maybeCons mx xs = case mx of
  Nothing -> xs
  Just x  -> x :: xs

