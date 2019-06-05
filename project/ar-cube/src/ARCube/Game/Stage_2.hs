{-# LANGUAGE OverloadedStrings #-}
module ARCube.Game.Stage_2 where

import           Data.Function ((&))
import           Data.List     (nub)
import           Miso

import           ARCube.Utils

-- NOTE: remove _exercise and fix all type holes
-- to complete this stage
_exercise :: a
_exercise = undefined

-- | Model of the game state (empty for now).
data Game = Game [Coords]
  deriving (Eq, Show)

-- | Possible in-game actions (empty for now).
data GameAction
  = SetMark Coords
  deriving (Eq, Show)

-- | Coordinates of 3x3 cube cells.
type Coords = (Int, Int, Int)

-- | Initialise game state.
initGame :: Game
initGame = Game []

-- | Game event handler.
handleGame :: GameAction -> Game -> Game
handleGame (SetMark coords) (Game marked) = _exercise

-- | How to render game in a VR/AR scene.
renderGame :: Game -> [View GameAction]
renderGame (Game marked) = cube3x3 marked
  & scaled 0.3 0.3 0.3
  & rotated 30 60 0

-- Exercise 2.
-- Make cube cells clickable and change their color
-- from orange to red.

cube3x3 :: [Coords] -> [View GameAction]
cube3x3 marked = concat
  [ translated x y z (cell isMarked coords)
  | i <- [-1, 0, 1]
  , j <- [-1, 0, 1]
  , k <- [-1, 0, 1]
  , let coords = (i, j, k)
        [x, y, z] = map fromIntegral [i, j, k]
        isMarked = _exercise
  ]

cell :: Bool -> Coords -> [View GameAction]
cell isMarked coords = scaled 0.9 0.9 0.9 (box
  [ prop_ "color"   _exercise -- Hint: this one depends on isMarked
  , prop_ "opacity" "0.5"
  , _exercise -- Hint: see Miso.onClick and SetMark action
  ])
