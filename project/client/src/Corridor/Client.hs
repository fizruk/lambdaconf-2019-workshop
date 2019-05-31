{-# LANGUAGE OverloadedStrings #-}
module Corridor.Client where

import           Corridor.Game
import           Data.List     (intercalate)
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
type Model = Game

-- | Initialise model.
initialModel :: Model
initialModel = initGame

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
updateModel NoOp (Game n) = pure (Game (n + 1))
updateModel (InGameAction gameAction) model
  = pure (updateGame gameAction model)

-- * Rendering

-- | Model rendering (in HTML).
viewModel :: Model -> View Action
viewModel _model =
  nodeHtml "a-scene" [ prop_ "embedded" "", prop_ "arjs" "" ]
    [ nodeHtml "a-marker"
        [ prop_ "preset" "custom"
        , prop_ "type" "pattern"
        , prop_ "url" "assets/markers/lc-2019-marker.patt"
        ]
        markerScene
    , nodeHtml "a-entity" [ prop_ "camera" "" ] []
    ]
  where
    markerScene = asciiToVoxels haskellLogo

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
