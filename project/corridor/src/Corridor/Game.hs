{-# LANGUAGE EmptyCase  #-}
{-# LANGUAGE LambdaCase #-}
module Corridor.Game where

import           Data.List  (transpose)
import           Data.Maybe (listToMaybe)

-- import           Miso

-- | Model of the game state.
data Game = Game
  { gameBoard :: Board
  } deriving (Eq)

-- | Coordinates on a board.
type Coords = (Int, Int)

-- | Possible in-game actions.
data GameAction
  = SetMark Mark Coords   -- ^ Try put a mark on a board cell.
  deriving (Show)

-- | Initialise game state.
initGame :: Game
initGame = Game { gameBoard = sampleBoard }

-- | A mark for tic-tac-toe.
data Mark = X | O
  deriving (Eq, Show)

-- | A cell is either empty or has a mark.
type Cell = Maybe Mark

-- | A board is a 2D grid of cells.
type Board = [[Cell]]

-- | Get all consequent streaks ignoring 'Nothing'.
streaks :: Eq a => [(Maybe a, b)] -> [(Int, (a, [b]))]
streaks [] = []
streaks ((Nothing, _) : xs) = streaks xs
streaks ((Just x, b) : xs)
  = (1 + length ys, (x, b : map snd ys)) : streaks zs
  where
    (ys, zs) = span ((== Just x) . fst) xs

-- | Determine is a streak is long enough to be a winning streak.
isLongStreak :: (Int, a) -> Bool
isLongStreak (i, _) = i >= 3

-- | Get a winning mark (if exists).
getWinner :: [(Int, a)] -> Maybe a
getWinner = listToMaybe . map snd

-- | Determine a winner in a game of tic-tac-toe (if exists).
winner :: Board -> Maybe (Mark, [(Int, Int)])
winner board = getWinner (filter isLongStreak (concatMap streaks allLines))
  where
    allLines = allLinesOf $
      zipWith (\j -> zipWith (\i m -> (m, (i, j))) [0..]) [0..] board

allLinesOf :: [[a]] -> [[a]]
allLinesOf board = rows ++ columns ++ diagonals
  where
    rows = board
    columns = transpose rows
    diagonals = leftDiagonals ++ rightDiagonals

    leftDiagonals = leftDiagonalsOf rows
    rightDiagonals = leftDiagonalsOf (reverse rows)

    leftDiagonalsOf b = leftTopDiagonalsOf b ++ leftBottomDiagonalsOf b
    leftTopDiagonalsOf = transpose . zipWith drop [0..]
    leftBottomDiagonalsOf = drop 1 . leftTopDiagonalsOf . transpose

-- | A sample 5x4 board.
sampleBoard :: Board
sampleBoard = reverse
  [ [ x, o, n, o, n ]
  , [ n, o, n, x, o ]
  , [ x, n, x, n, n ]
  , [ n, o, n, x, x ] ]
  where
    o = Just O
    x = Just X
    n = Nothing

-- | Initialise an empty NxM board.
initBoard :: (Int, Int) -> Board
initBoard (n, m) = replicate m (replicate n (Just O))

isGameOver :: Board -> Bool
isGameOver board =
  case winner board of
    Nothing -> False
    _       -> True

boardPlayer :: Board -> Maybe Mark
boardPlayer board
  | isGameOver board  = Nothing
  | crosses > noughts = Just O
  | otherwise         = Just X
  where
    crosses = length (filter (== Just X) (concat board))
    noughts = length (filter (== Just O) (concat board))

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt i f xs
  | i < 0 = xs
  | otherwise =
      case splitAt i xs of
        (ys, z:zs) -> ys ++ f z : zs
        _          -> xs

updateBoardAt :: (Int, Int) -> (Maybe Mark -> Maybe Mark) -> Board -> Board
updateBoardAt (i, j) f = updateAt j (updateAt i f)

pointToCoords :: (Double, Double) -> (Int, Int)
pointToCoords (x, y) = (round x, round y)

setMarkAt :: Mark -> (Int, Int) -> Board -> Board
setMarkAt mark coords board =
  case boardPlayer board of
    Just currentPlayerMark | currentPlayerMark == mark
      -> updateBoardAt coords setMark board
    _ -> board
  where
    setMark Nothing = Just mark
    setMark cell    = cell

updateGame :: GameAction -> Game -> Game
updateGame (SetMark mark coords) g = g
  { gameBoard = gameBoard' }
  where
    gameBoard' = setMarkAt mark coords (gameBoard g)
--
-- drawMark :: Mark -> View action
-- drawMark X = _
-- drawMark O = _
--
-- drawCellAt :: Int -> Int -> Cell -> View GameAction
-- drawCellAt i j cell = _
--
-- drawBoard :: Board -> View GameAction
-- drawBoard = pictures . zipWith drawRow [0..]
--   where
--     drawRow :: Int -> [Cell] -> View GameAction
--     drawRow j = pictures . zipWith (\i -> drawCellAt i j) [0..]
--
-- drawWinner :: Maybe (Mark, [(Int, Int)]) -> View GameAction
-- drawWinner Nothing            = blank
-- drawWinner (Just (X, coords)) = colored red $ pictures
--   (map (\(i, j) -> drawCellAt i j (Just X)) coords)
-- drawWinner (Just (O, coords)) = colored red $ pictures
--   (map (\(i, j) -> drawCellAt i j (Just O)) coords)
--
-- drawTicTacToe :: Board -> View GameAction
-- drawTicTacToe board = drawWinner (winner board) <> drawBoard board
