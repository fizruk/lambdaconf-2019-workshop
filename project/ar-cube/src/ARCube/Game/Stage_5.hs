{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module ARCube.Game.Stage_5 where

import           Data.Function ((&))
import           Data.Maybe    (maybe)
import           Data.Monoid   ((<>))
import           Miso
import           Miso.String   (ms)

import           ARCube.Utils

-- NOTE: remove _exercise and fix all type holes
-- to complete this stage
_exercise :: a
_exercise = error "Exercise in Stage 5 is not implemented!"

-- | Model of the game state (empty for now).
data Game = Game
  { gMarked1       :: [Coords]            -- ^ Marked cells by Player 1.
  , gMarked2       :: [Coords]            -- ^ Marked cells by Player 2.
  , gFocus         :: Maybe Coords        -- ^ Current player's focus.
  , gCurrentPlayer :: Player              -- ^ Current player.
  , gSliceRotation :: Maybe SliceRotation -- ^ Latest rotated slice.
  , gRotationCount :: Int                 -- ^ Counter for slice rotation animations.
  } deriving (Eq, Show, Read)

-- | A player is one of two.
data Player = Player1 | Player2
  deriving (Eq, Show, Read)

switchPlayer :: Player -> Player
switchPlayer Player1 = Player2
switchPlayer Player2 = Player1

gMarked :: Game -> [Coords]
gMarked = gMarked1 <> gMarked2

unsafeMarkCell :: Coords -> Game -> Game
unsafeMarkCell coords game =
  case gCurrentPlayer game of
    Player1 -> game { gMarked1 = coords : gMarked1 game }
    Player2 -> game { gMarked2 = coords : gMarked2 game }

-- | Possible in-game actions (empty for now).
data GameAction
  = SetMark Coords                    -- ^ Mark a cell in a cube.
  | SetFocus Coords                   -- ^ Focus on a cell in a cube.
  | ResetFocus Coords                 -- ^ Reset focus.
  | RotateSlice (Maybe SliceRotation) -- ^ Rotate cube slice.
  deriving (Eq, Show, Read)

-- | Coordinates of 3x3 cube cells.
type Coords = (Int, Int, Int)

-- | Axis of rotation.
data Axis = AxisX | AxisY | AxisZ
  deriving (Eq, Show, Read)

-- | Slice rotation determined by axis and slice index along that axis.
data SliceRotation = SliceRotation Axis Int
  deriving (Eq, Show, Read)

-- | Rotation as combination of 3 angles (one per axis).
type Rotation = (Float, Float, Float)

-- | Initialise game state.
initGame :: Game
initGame = Game [] [] Nothing Player1 Nothing 0

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
  | otherwise = (unsafeMarkCell coords game)
      { gSliceRotation = Nothing
      , gCurrentPlayer = _exercise
      }
handleGame (RotateSlice sr) game = game
  { gMarked1 = _exercise
  , gMarked2 = _exercise
  , gSliceRotation = sr
  , gRotationCount = 1 + gRotationCount game
  , gCurrentPlayer = _exercise
  }

-- | Determine which slice to rotate and how based on a cell.
toSliceRotation :: Coords -> Maybe SliceRotation
toSliceRotation (i, j, k)
  | j == 0 && k == 0 = Just (SliceRotation AxisX i)
  | i == 0 && k == 0 = Just (SliceRotation AxisY j)
  | i == 0 && j == 0 = Just (SliceRotation AxisZ k)
  | i == 0 = Just (SliceRotation AxisX 0)
  | j == 0 = Just (SliceRotation AxisY 0)
  | k == 0 = Just (SliceRotation AxisZ 0)
  | otherwise = Nothing

-- | Check if a given cell is involved in slice rotation.
inSliceRotation :: Coords -> SliceRotation -> Bool
inSliceRotation (i, j, k) = \case
  SliceRotation AxisX x -> i == x
  SliceRotation AxisY y -> j == y
  SliceRotation AxisZ z -> k == z

-- | Rotation angles for an axis.
axisRotation :: Axis -> Rotation
axisRotation = \case
  AxisX -> (90, 0, 0)
  AxisY -> (0, 90, 0)
  AxisZ -> (0, 0, 90)

-- | Instantly rotate cell to a new place in a cube (if in the slice).
rotateCoords :: SliceRotation -> Coords -> Coords
rotateCoords (SliceRotation axis d) (i, j, k) =
  case axis of
    AxisX | i == d -> (i, k, -j)
    AxisY | j == d -> (-k, j, i)
    AxisZ | k == d -> (j, -i, k)
    _              -> (i, j, k)

-- | How to render game in a VR/AR scene.
renderGame :: Game -> [View GameAction]
renderGame game = cube3x3 game
  & translated 0 2.6 0  -- lift the cube so that it won't touch the surface when rotating
  & scaled 0.3 0.3 0.3  -- scale to make entire cube about the size of the marker

-- | Render 3x3 cube with interactive cells.
cube3x3 :: Game -> [View GameAction]
cube3x3 game = concat
  [ cell game coords
    & translated x y z
    & animateRotation coords
  | i <- [-1, 0, 1]
  , j <- [-1, 0, 1]
  , k <- [-1, 0, 1]
  , (i, j, k) /= (0, 0, 0)  -- don't render internal cell
  , let coords = (i, j, k)
        [x, y, z] = map fromIntegral [i, j, k]
  ]
  where
    animateRotation coords =
      case gSliceRotation game of
        Just sr@(SliceRotation axis _) | inSliceRotation coords sr
          -> rotatedAnim n (axisRotation axis) (0, 0, 0)
        _ -> rotatedAnim n (0, 0, 0) (0, 0, 0) -- NOTE: simply removing animation does not work!
    n = ms (show (gRotationCount game))

-- | Render an interactive cube cell.
cell :: Game -> Coords -> [View GameAction]
cell game coords = scaled 0.9 0.9 0.9 (box
  [ prop_ "color"   color
  , prop_ "opacity" "0.5"
  , onClick (SetMark coords)
  , onMouseEnter (SetFocus coords)
  , onMouseLeave (ResetFocus coords)
  ])
  where
    isMarked1 = coords `elem` gMarked1 game
    isMarked2 = coords `elem` gMarked2 game
    isFocus   = Just coords == gFocus game
    color
      | isMarked1 = "orange"
      | isMarked2 = "blue"
      | isFocus = case gCurrentPlayer game of
                    Player1 -> "yellow"
                    Player2 -> "cyan"
      | otherwise = "white"
