{-# LANGUAGE OverloadedStrings #-}
module ARCube.Game.Solution.Stage_3 where

import           Data.Function ((&))
import           Data.List     (nub)
import           Miso

import           ARCube.Utils

-- | Model of the game state (empty for now).
data Game = Game
  { gMarked :: [Coords]     -- ^ Marked cells.
  , gFocus  :: Maybe Coords -- ^ Cell in focus.
  } deriving (Eq, Show, Read)

-- | Possible in-game actions (empty for now).
data GameAction
  = SetMark Coords
  | SetFocus Coords
  | ResetFocus Coords
  deriving (Eq, Show, Read)

-- | Coordinates of 3x3 cube cells.
type Coords = (Int, Int, Int)

-- | Initialise game state.
initGame :: Game
initGame = Game [] Nothing

-- | Game event handler.
handleGame :: GameAction -> Game -> Game
handleGame (ResetFocus coords) game
  | gFocus game == Just coords = game { gFocus = Nothing }
  | otherwise = game
handleGame (SetFocus coords) game = game
  { gFocus = Just coords }
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
        isFocus  = Just coords == gFocus game
  ]

cell :: Bool -> Bool -> Coords -> [View GameAction]
cell isMarked isFocus coords = scaled 0.9 0.9 0.9 (box
  [ prop_ "color"   color
  , prop_ "opacity" "0.5"
  , onClick (SetMark coords)
  , onMouseEnter (SetFocus coords)
  , onMouseLeave (ResetFocus coords)
  ])
  where
    color
      | isMarked  = "red"
      | isFocus   = "orange"
      | otherwise = "yellow"

