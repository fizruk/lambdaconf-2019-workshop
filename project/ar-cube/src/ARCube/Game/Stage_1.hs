{-# LANGUAGE EmptyCase          #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
module ARCube.Game.Stage_1 where

import           Data.Function ((&))
import           Miso

import           ARCube.Utils

-- NOTE: remove _exercise and fix all type holes
-- to complete this stage
_exercise :: a
_exercise = error "Exercise in Stage 1 is not implemented!"

-- | Model of the game state (empty for now).
data Game = Game
  deriving (Eq, Show, Read)

-- | Possible in-game actions (empty for now).
data GameAction
deriving instance Show GameAction
deriving instance Read GameAction

-- | Initialise game state.
initGame :: Game
initGame = Game

-- | Game event handler.
handleGame :: GameAction -> Game -> Game
handleGame = \case {}

-- | How to render game in a VR/AR scene.
renderGame :: Game -> [View GameAction]
renderGame _ = cube3x3
  & rotated 30 60 0     -- initial rotation just for fun
  & translated 0 2.6 0  -- lift the cube so that it won't touch the surface when rotating
  & scaled 0.3 0.3 0.3  -- scale to make entire cube about the size of the marker

-- Exercise 1. Render a 3x3 cube using 'cell's.

-- | A cell is simply a translucent box.
cell :: [View action]
cell = scaled 0.9 0.9 0.9 (box
  [ prop_ "color"   "orange"
  , prop_ "opacity" "0.5"
  ])

-- | A 3x3 cube.
cube3x3 :: [View action]
cube3x3 = _exercise  -- Hint: what are the coordinates of cells?

