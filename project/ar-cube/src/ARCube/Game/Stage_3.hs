{-# LANGUAGE OverloadedStrings #-}
module ARCube.Game.Stage_3 where

import           Data.Function ((&))
import           Data.List     (nub)
import           Miso

import           ARCube.Utils

-- NOTE: remove _exercise and fix all type holes
-- to complete this stage
_exercise :: a
_exercise = undefined

-- | Model of the game state (empty for now).
data Game = Game
  { gMarked :: [Coords]     -- ^ Marked cells.
  , gFocus  :: Maybe Coords -- ^ Cell in focus.
  } deriving (Eq, Show)

-- | Possible in-game actions (empty for now).
data GameAction
  = SetMark Coords
  | SetFocus Coords
  | ResetFocus Coords
  deriving (Eq, Show)

-- | Coordinates of 3x3 cube cells.
type Coords = (Int, Int, Int)

-- | Initialise game state.
initGame :: Game
initGame = Game [] Nothing

-- | Game event handler.
handleGame :: GameAction -> Game -> Game
handleGame (ResetFocus coords) game = _exercise
handleGame (SetFocus coords) game = _exercise
handleGame (SetMark coords) game = game
  { gMarked = coords : gMarked game }

-- | How to render game in a VR/AR scene.
renderGame :: Game -> [View GameAction]
renderGame game = cube3x3 game
  & scaled 0.3 0.3 0.3
  & rotated 30 60 0

cube3x3 :: Game -> [View GameAction]
cube3x3 game = concat
  [ translated x y z (cell isMarked isFocus coords)
  | i <- [-1, 0, 1]
  , j <- [-1, 0, 1]
  , k <- [-1, 0, 1]
  , let coords = (i, j, k)
        [x, y, z] = map fromIntegral [i, j, k]
        isMarked = coords `elem` gMarked game
        isFocus  = _exercise
  ]

cell :: Bool -> Bool -> Coords -> [View GameAction]
cell isMarked isFocus coords = scaled 0.9 0.9 0.9 (box
  [ prop_ "color"   color
  , prop_ "opacity" "0.5"
  , onClick (SetMark coords)
  , onMouseEnter _exercise
  , onMouseLeave _exercise
  ])
  where
    color
      | isMarked  = "red"
      | isFocus   = "orange"
      | otherwise = "yellow"

