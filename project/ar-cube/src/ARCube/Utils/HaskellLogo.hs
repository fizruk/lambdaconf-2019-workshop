{-# LANGUAGE OverloadedStrings #-}
module ARCube.Utils.HaskellLogo where

import           Data.Monoid       ((<>))
import           Miso
import           Miso.String       (MisoString, ms)

import           ARCube.Utils.Miso

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
    , prop_ "width" (ms (show (0.95 * s * asciiRatio)))
    , prop_ "depth" (ms (show (0.95 * s)))
    , prop_ "height" (ms (show (10 * s)))
    , prop_ "position" pos
    ] []
  where
    pos = ms (show x) <> " " <> ms (show y) <> " " <> ms (show z)
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

