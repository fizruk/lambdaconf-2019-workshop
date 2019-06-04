{-# LANGUAGE OverloadedStrings #-}
module Corridor.Client where

import           Corridor.Game
import           Data.Function ((&))
import           Data.List     (intercalate)
import           Data.Monoid
import           Miso
import           Miso.String   (MisoString, ms)

-- | Run client application.
run :: IO ()
run = startApp App
  { initialAction   = initAction
  , model           = initialModel
  , update          = updateModel
  , view            = viewModel
  , subs            = [ ]
  , events          = defaultEvents
  , mountPoint      = Nothing
  }

-- * Model

-- | Client's state model.
data Model = Model Game Int
  deriving (Eq)

-- | Initialise model.
initialModel :: Model
initialModel = Model initGame 0

-- * Actions

-- | Client's possible actions.
data Action
  = InGameAction GameAction   -- ^ An in-game action.
  | NoOp                      -- ^ A special do-nothing action.

-- | Initial action to perfom on application start.
initAction :: Action
initAction = NoOp

-- * Handling model

-- | Client action handlers.
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp (Model game n) = pure (Model game (n + 1))
updateModel (InGameAction gameAction) (Model game n)
  = pure (Model (updateGame gameAction game) n)

-- * Rendering

-- | Model rendering (in HTML).
viewModel :: Model -> View Action
viewModel (Model game _)
--  = sceneVR markerScene
  = sceneAR markerScene
  where
    -- markerScene = asciiToVoxels haskellLogo
    markerScene = drawTicTacToe (gameBoard game)

sceneVR :: [View action] -> View action
sceneVR wrapped = nodeHtml "a-scene" []
  [ nodeHtml "a-entity"
    [ prop_ "camera" ""
    , prop_ "look-controls" ""
    , prop_ "wasd-controls" ""
    ] [ nodeHtml "a-cursor" [] [] ]
  , nodeHtml "a-sky" [ prop_ "color" "#CCDCEC" ] []
  , nodeHtml "a-entity"
      [ prop_ "position" "0 0 -1.5"
      ] wrapped
  ]

sceneAR :: [View action] -> View action
sceneAR wrapped = nodeHtml "a-scene" [ prop_ "embedded" "", prop_ "arjs" "" ]
  [ nodeHtml "a-marker"
      [ prop_ "preset" "custom"
      , prop_ "type" "pattern"
      , prop_ "url" "assets/markers/lc-2019-marker.patt"
      ]
      [ nodeHtml "a-entity"
          [ prop_ "rotation" "-90 0 0" ]
          wrapped
      , box' 0.1 0.1 0.1 [ prop_ "color" "blue" ]
      ]
  , nodeHtml "a-entity" [ prop_ "camera" "" ] []
  ]

prop_ :: MisoString -> MisoString -> Attribute action
prop_ = prop

-- * Haskell voxels logo (demo)

type AsciiImage = [[Char]]

-- | Width-to-height aspect ratio for monofont used in ASCII images.
asciiRatio :: Float
asciiRatio = 0.7

voxelColor :: Char -> Maybe String
voxelColor 'o' = Just "#FF5900"
voxelColor 'y' = Just "#FFB106"
voxelColor _   = Nothing

-- | Haskell logo.
--
-- @o@ — orange.
-- @y@ — yellow.
haskellLogo :: AsciiImage
haskellLogo =
  [ "oooooo   yyyyyyy                       "
  , "  oooooo   yyyyyy                      "
  , "   oooooo   yyyyyyy                    "
  , "    oooooo   yyyyyyy                   "
  , "      oooooo   yyyyyy   ooooooooooooooo"
  , "       oooooo   yyyyyyy  oooooooooooooo"
  , "        oooooo   yyyyyyy               "
  , "        oooooo   yyyyyyyy              "
  , "       oooooo   yyyyyyyyyyy   ooooooooo"
  , "      oooooo   yyyyyyyyyyyyy   oooooooo"
  , "    oooooo   yyyyyyy   yyyyyyy         "
  , "   oooooo   yyyyyyy     yyyyyyy        "
  , "  oooooo   yyyyyy        yyyyyyy       "
  , "oooooo   yyyyyyy           yyyyyyy     "
  ]

voxel :: Int -> Int -> Int -> String -> View action
voxel i j k c =
  nodeHtml "a-box"
    [ prop_ "color" (ms c)
    , prop_ "width" (ms (0.95 * s * asciiRatio))
    , prop_ "height" (ms (10 * s))
    , prop_ "depth" (ms (0.95 * s))
    , prop_ "position" (ms (intercalate " " (map show [x, y, z])))
    ] []
  where
    s = 0.05
    x = s * fromIntegral i * asciiRatio
    y = s * fromIntegral j
    z = s * fromIntegral k

asciiToVoxels :: AsciiImage -> [View action]
asciiToVoxels ascii =
  [ voxel (i - w) 1 (j - h) c
  | (j, line) <- zip [0..] ascii
  , (i, char) <- zip [0..] line
  , Just c <- [voxelColor char]
  ]
  where
    h = length ascii `div` 2
    w = maximum (map length ascii) `div` 2

cellPosition :: Int -> Int -> MisoString
cellPosition i j = ms (intercalate " " (map show [x, y, z]))
  where
    x = fromIntegral i * s
    y = 0
    z = fromIntegral j * s
    s = 0.05 :: Float

box' :: Float -> Float -> Float -> [Attribute action] -> View action
box' w h d attrs = nodeHtml "a-box"
  ([ prop_ "width"  (ms w)
   , prop_ "height" (ms h)
   , prop_ "depth"  (ms d)
   ] ++ attrs) []

box :: Float -> Float -> Float -> View action
box w h d = box' w h d []

cylinder :: Float -> Float -> View action
cylinder r h = nodeHtml "a-cylinder"
  [ prop_ "radius" (ms r)
  , prop_ "height" (ms h)
  ] []

sphere :: Float -> View action
sphere r = nodeHtml "a-sphere" [ prop_ "radius" (ms r) ] []

translated :: Float -> Float -> Float -> [View action] -> [View action]
translated x y z = pure . nodeHtml "a-entity" [ prop_ "position" str ]
  where
    str = ms x <> " " <> ms y <> " " <> ms z

rotated :: Float -> Float -> Float -> [View action] -> [View action]
rotated x y z = pure . nodeHtml "a-entity" [ prop_ "rotation" str ]
  where
    str = ms x <> " " <> ms y <> " " <> ms z

scaled :: Float -> Float -> Float -> [View action] -> [View action]
scaled x y z = pure . nodeHtml "a-entity" [ prop_ "scale" str ]
  where
    str = ms x <> " " <> ms y <> " " <> ms z

colored :: MisoString -> [View action] -> [View action]
colored c = pure . nodeHtml "a-entity" [ prop_ "color" c ]

drawMark :: Mark -> [View action]
drawMark X = [ box 0.5 2 0.5, box 2 0.5 0.5, box 0.5 0.5 2 ]
  & rotated 0 45 45
  & scaled 0.4 0.4 0.4
drawMark O = [ sphere 0.3 ]
  & rotated 90 0 0
  & colored "red"

cellBoxAt :: Int -> Int -> [View Action]
cellBoxAt i j = [ box' 0.95 0.95 0.95
  [ prop_ "color" "blue"
  , prop_ "opacity" "0.1"
  , onClick (InGameAction (SetMark O (i, j)))
  , onMouseEnter (InGameAction (SetMark X (i, j)))
  ] ]

winningCellBoxAt :: Int -> Int -> [View Action]
winningCellBoxAt i j = [ box' 0.95 0.95 0.95
  [ prop_ "color" "red"
  , prop_ "opacity" "0.1"
  ] ]

drawCellAt :: Int -> Int -> Cell -> [View Action]
drawCellAt i j cell = translated x y 0
  (cellPicture <> cellBoxAt i j)
  where
    x = fromIntegral i
    y = fromIntegral j
    cellPicture = foldMap drawMark cell

drawWinningCellAt :: Int -> Int -> Cell -> [View Action]
drawWinningCellAt i j cell = translated x y 0
  (cellPicture <> winningCellBoxAt i j)
  where
    x = fromIntegral i
    y = fromIntegral j
    cellPicture = foldMap drawMark cell

drawBoard :: Maybe (Mark, [(Int, Int)]) -> Board -> [View Action]
drawBoard win = mconcat . zipWith drawRow [0..]
  where
    drawRow :: Int -> [Cell] -> [View Action]
    drawRow j = mconcat . zipWith (\i -> drawCellAt' i j) [0..]

    drawCellAt' i j
      | (i, j) `elem` foldMap snd win = drawWinningCellAt i j
      | otherwise = drawCellAt i j

drawWinner :: Maybe (Mark, [(Int, Int)]) -> [View Action]
drawWinner Nothing            = mempty
drawWinner (Just (X, coords)) = mempty
drawWinner (Just (O, coords)) = mempty

drawTicTacToe :: Board -> [View Action]
drawTicTacToe board = drawBoard (winner board) board
  & translated (- w / 2) (- h / 2) 0  -- center board
  & scaled 0.2 0.2 0.2
  where
    h = fromIntegral (length board)
    w = fromIntegral (maximum (map length board))
