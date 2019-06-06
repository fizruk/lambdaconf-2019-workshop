{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module ARCube.Game.Solution.Stage_4 where

import           Data.Function ((&))
import           Data.Maybe    (maybe)
import           Miso
import           Miso.String   (ms)

import           ARCube.Utils

-- | Model of the game state (empty for now).
data Game = Game
  { gMarked        :: [Coords]     -- ^ Marked cells.
  , gFocus         :: Maybe Coords -- ^ Cell in focus.
  , gSliceRotation :: Maybe SliceRotation
  , gRotationCount :: Int
  } deriving (Eq, Show, Read)

-- | Possible in-game actions (empty for now).
data GameAction
  = SetMark Coords
  | SetFocus Coords
  | ResetFocus Coords
  | RotateSlice (Maybe SliceRotation)
  deriving (Eq, Show, Read)

-- | Coordinates of 3x3 cube cells.
type Coords = (Int, Int, Int)

data Axis = AxisX | AxisY | AxisZ
  deriving (Eq, Show, Read)

data SliceRotation = SliceRotation Axis Int
  deriving (Eq, Show, Read)

type Rotation = (Float, Float, Float)

-- | Initialise game state.
initGame :: Game
initGame = Game [] Nothing Nothing 0

-- | Game event handler.
handleGame :: GameAction -> Game -> Game
handleGame (ResetFocus coords) game
  | gFocus game == Just coords = game { gFocus = Nothing }
  | otherwise = game
handleGame (SetFocus coords) game = game
  { gFocus = Just coords }
handleGame (SetMark coords) game
  | coords `elem` gMarked game = handleGame
      (RotateSlice (toSliceRotation coords)) game
  | otherwise = game
      { gMarked = coords : gMarked game
      , gSliceRotation = Nothing
      }
handleGame (RotateSlice sr) game = game
  { gMarked = maybe id (map . rotateCoords) sr (gMarked game)
  , gSliceRotation = sr
  , gRotationCount = 1 + gRotationCount game
  }

toSliceRotation :: Coords -> Maybe SliceRotation
toSliceRotation (i, j, k)
  | j == 0 && k == 0 = Just (SliceRotation AxisX i)
  | i == 0 && k == 0 = Just (SliceRotation AxisY j)
  | i == 0 && j == 0 = Just (SliceRotation AxisZ k)
  | i == 0 = Just (SliceRotation AxisX 0)
  | j == 0 = Just (SliceRotation AxisY 0)
  | k == 0 = Just (SliceRotation AxisZ 0)
  | otherwise = Nothing

inSliceRotation :: Coords -> SliceRotation -> Bool
inSliceRotation (i, j, k) = \case
  SliceRotation AxisX x -> i == x
  SliceRotation AxisY y -> j == y
  SliceRotation AxisZ z -> k == z

axisRotation :: Axis -> Rotation
axisRotation = \case
  AxisX -> (90, 0, 0)
  AxisY -> (0, 90, 0)
  AxisZ -> (0, 0, 90)

rotateCoords :: SliceRotation -> Coords -> Coords
rotateCoords (SliceRotation axis d) (i, j, k) =
  case axis of
    AxisX | i == d -> (i, k, -j)
    AxisY | j == d -> (-k, j, i)
    AxisZ | k == d -> (j, -i, k)
    _              -> (i, j, k)

increaseRotation
  :: (Float, Float, Float)
  -> (Float, Float, Float)
  -> (Float, Float, Float)
increaseRotation (a, b, c) (x, y, z) = (a + x, b + y, c + z)

-- | How to render game in a VR/AR scene.
renderGame :: Game -> [View GameAction]
renderGame game = cube3x3 game
  & scaled 0.3 0.3 0.3

cube3x3 :: Game -> [View GameAction]
cube3x3 game = concat
  [ cell isMarked isFocus coords
    & translated x y z
    & animateRotation coords
  | i <- [-1, 0, 1]
  , j <- [-1, 0, 1]
  , k <- [-1, 0, 1]
  , let coords = (i, j, k)
        [x, y, z] = map fromIntegral [i, j, k]
        isMarked = coords `elem` gMarked game
        isFocus  = Just coords == gFocus game
  ]
  where
    animateRotation coords =
      case gSliceRotation game of
        Just sr@(SliceRotation axis _) | inSliceRotation coords sr
          -> rotatedAnim n (axisRotation axis) (0, 0, 0)
        _ -> rotatedAnim n (0, 0, 0) (0, 0, 0) -- NOTE: simply removing animation does not work!
    n = ms (show (gRotationCount game))

cell :: Bool -> Bool -> Coords -> [View GameAction]
cell isMarked isFocus coords = scaled 0.9 0.9 0.9 (box
  [ prop_ "color"   color
  , prop_ "opacity" "0.5"
  , onClick (SetMark coords)
  , onDoubleClick (RotateSlice (toSliceRotation coords))
  , onMouseEnter (SetFocus coords)
  , onMouseLeave (ResetFocus coords)
  ])
  where
    color
      | isMarked  = "red"
      | isFocus   = "orange"
      | otherwise = "yellow"