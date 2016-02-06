module Utilities (if', Vec, dist1, toFloatVec, mapWithDefault, firstKey) where

import Dict exposing (Dict)


-- General


if' : Bool -> a -> a -> a
if' x y z =
  if x then
    y
  else
    z


type alias Vec number =
  ( number, number )


dist1 : Vec number -> Vec number -> number
dist1 ( x, y ) ( x', y' ) =
  abs (x - x') + abs (y - y')


toFloatVec : Vec Int -> Vec Float
toFloatVec ( x, y ) =
  ( toFloat x, toFloat y )



-- Maybe


mapWithDefault : b -> (a -> b) -> Maybe a -> b
mapWithDefault b f =
  Maybe.withDefault b << Maybe.map f



-- Dict


firstKey : Dict comparable a -> Maybe comparable
firstKey =
  List.head << Dict.keys
