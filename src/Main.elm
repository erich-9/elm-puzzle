module Main (main) where

import Array exposing (Array)
import Color exposing (white)
import Dict exposing (Dict)
import Drag exposing (Action(..), trackMany)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input exposing (hoverable)
import Random exposing (generate, initialSeed)
import Random.Array exposing (shuffle)
import Set exposing (Set)
import Signal exposing (Signal, Mailbox)
import Utilities exposing (..)


config =
  { imgSrc = "img/elm.png"
  , imgShrinkFactor = 1
  , imgOffsetX = 0
  , imgOffsetY = 0
  , cols = 3
  , rows = 3
  , tileWidth = 93
  , tileHeight = 93
  , canvasWidth = 420
  , canvasHeight = 420
  , joinDist = 10
  }


type alias Puzzle =
  Dict Index { pos : Vec Float, level : Int, piece : Piece }


type alias Piece =
  Set (Vec Int)


type alias Index =
  Int


width : Int
width =
  config.cols * config.tileWidth


height : Int
height =
  config.rows * config.tileHeight


hover : Mailbox (Maybe Index)
hover =
  Signal.mailbox Nothing


main : Signal Element
main =
  let
    update m puzzle =
      case m of
        Just ( i, Lift ) ->
          toTopLevel i puzzle

        Just ( i, MoveBy dxy ) ->
          movePiece i (toFloatVec dxy) puzzle

        Just ( i, Release ) ->
          tryToJoin i puzzle

        _ ->
          puzzle

    puzzleSignal =
      Signal.foldp update initPuzzle (trackMany Nothing hover.signal)
  in
    Signal.map puzzleToElement puzzleSignal


toTopLevel : Index -> Puzzle -> Puzzle
toTopLevel i puzzle =
  let
    topLevel =
      Dict.foldl (always (max << .level)) 0 puzzle
  in
    Dict.update i (Maybe.map (\r -> { r | level = topLevel + 1 })) puzzle


movePiece : Index -> Vec Float -> Puzzle -> Puzzle
movePiece i ( dx, dy ) =
  let
    movePos ( x, y ) =
      ( x + dx, y - dy )
  in
    Dict.update i (Maybe.map (\r -> { r | pos = movePos r.pos }))


tryToJoin : Index -> Puzzle -> Puzzle
tryToJoin i puzzle =
  let
    mj =
      Dict.get i puzzle `Maybe.andThen` indexOfJoinCandidate

    indexOfJoinCandidate pp =
      (firstKey << Dict.filter (g (neighbors pp.piece)) << f) pp.pos

    f pos =
      Dict.map (\_ r -> { r | pos = dist1 pos r.pos }) puzzle

    g nbsI j { pos, piece } =
      (j /= i && pos < config.joinDist)
        && not (Set.isEmpty (Set.intersect nbsI piece))
  in
    (mapWithDefault identity (join i) mj) puzzle


neighbors : Set (Vec Int) -> Set (Vec Int)
neighbors pos =
  let
    posNeighbors ( x, y ) =
      Set.fromList [ ( x + 1, y ), ( x - 1, y ), ( x, y + 1 ), ( x, y - 1 ) ]
  in
    Set.foldl (\v a -> Set.union a (posNeighbors v)) Set.empty pos


join : Index -> Index -> Puzzle -> Puzzle
join i j =
  let
    join' puzzle =
      case ( Dict.get i puzzle, Dict.get j puzzle ) of
        ( Just r, Just s ) ->
          (Dict.insert j (union s r) << Dict.remove i) puzzle

        _ ->
          puzzle

    union r s =
      { r | level = max r.level s.level, piece = Set.union r.piece s.piece }
  in
    if' (i == j) identity join'


initPuzzle : Puzzle
initPuzzle =
  let
    initTile ( i, i' ) =
      let
        ( cr, cr' ) =
          ( i2cr i, i2cr i' )

        i2cr i =
          ( i % config.cols, i // config.cols )
      in
        ( i', { pos = initPos cr' cr, level = i', piece = Set.singleton cr } )
  in
    (Dict.fromList << List.map initTile << Array.toIndexedList) shuffledIndices


initPos : Vec Int -> Vec Int -> Vec Float
initPos cr' cr =
  let
    ( ( c, r ), ( c', r' ) ) =
      ( toFloatVec cr, toFloatVec cr' )

    ( dx, dy ) =
      ( mx / (rows + 1), my / (cols + 1) )

    ( mx, my ) =
      toFloatVec ( config.canvasWidth - width, config.canvasHeight - height )

    ( w, h ) =
      toFloatVec ( config.tileWidth, config.tileHeight )

    ( cols, rows ) =
      toFloatVec ( config.cols, config.rows )
  in
    ( (c' - c) * w + (c' - (cols - 1) / 2) * dx
    , (r' - r) * h + (r' - (rows - 1) / 2) * dy
    )


puzzleToElement : Puzzle -> Element
puzzleToElement puzzle =
  let
    puzzleToForms =
      List.concat << Dict.values << Dict.foldl addPiece Dict.empty

    addPiece i { pos, level, piece } a =
      Dict.insert level (pieceToForms i pos piece) a

    ( w, h ) =
      ( config.canvasWidth, config.canvasHeight )
  in
    collage w h (puzzleToForms puzzle)


pieceToForms : Index -> Vec Float -> Piece -> List Form
pieceToForms i ( x, y ) piece =
  let
    f cr a =
      let
        pos =
          ( (c + 0.5) * tw + x - w / 2, (r + 0.5) * th + y - h / 2 )

        ( c, r ) =
          toFloatVec cr
      in
        move pos (toForm (tile i cr)) :: a

    ( tw, th ) =
      toFloatVec ( config.tileWidth, config.tileHeight )

    ( w, h ) =
      toFloatVec ( width, height )
  in
    Set.foldl f [] piece


tile : Index -> Vec Int -> Element
tile i cr =
  let
    f hovering =
      if' hovering (Just i) Nothing

    border =
      outlined (solid white) (rect (toFloat w) (toFloat h))

    ( w, h ) =
      ( config.tileWidth, config.tileHeight )
  in
    collage (w - 1) (h - 1) [ imgTile cr, border ]
      |> hoverable (Signal.message hover.address << f)


imgTile : Vec Int -> Form
imgTile ( c, r ) =
  let
    ( ws, hs ) =
      ( s * config.tileWidth, s * config.tileHeight )

    ( xs, ys ) =
      ( s * config.imgOffsetX, s * config.imgOffsetY )

    s =
      config.imgShrinkFactor
  in
    config.imgSrc
      |> croppedImage ( c * ws + xs, (config.rows - r - 1) * hs + ys ) ws hs
      |> toForm
      |> scale (1 / toFloat s)


shuffledIndices : Array Index
shuffledIndices =
  let
    arrGen =
      shuffle (Array.initialize (config.cols * config.rows) identity)
  in
    fst (Random.generate arrGen (initialSeed randomInt))


port randomInt : Int
