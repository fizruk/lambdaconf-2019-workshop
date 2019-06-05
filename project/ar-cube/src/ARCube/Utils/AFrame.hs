{-# LANGUAGE OverloadedStrings #-}
module ARCube.Utils.AFrame where

import qualified Data.List         as List
import           Miso
import           Miso.String       (MisoString, ToMisoString (..), ms)

import           ARCube.Utils.Miso


-- | Set up a VR scene.
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
      , prop_ "rotation" "90 0 0"
      ] wrapped
  ]

-- | Set up an AR scene.
sceneAR :: [View action] -> View action
sceneAR wrapped = nodeHtml "a-scene"
  [ prop_ "embedded" ""
  , prop_ "arjs" "sourceType: webcam; debugUIEnabled: false;"
  , prop_ "vr-mode-ui" "enabled: false"
  ]
  [ nodeHtml "a-marker"
      [ prop_ "preset" "custom"
      , prop_ "type" "pattern"
      , prop_ "url" "assets/markers/lc-2019-marker.patt"
      , prop_ "emitevents" "true"
      , prop_ "cursor" "rayOrigin: mouse"
      ]
      wrapped
  , nodeHtml "a-entity" [ prop_ "camera" "" ] []
  ]

-- * Primitives

box :: [Attribute action] -> [View action]
box attrs = wrapTag "a-box" attrs []

box_ :: [View action]
box_ = box []

sphere :: [Attribute action] -> [View action]
sphere attrs = wrapTag "a-sphere" attrs []

sphere_ :: [View action]
sphere_ = sphere []

-- * Relative positioning, orientation and scaling

translated :: Float -> Float -> Float -> [View action] -> [View action]
translated x y z = wrapEntity [ prop_ "position" (msListOf show [x, y, z]) ]

rotated :: Float -> Float -> Float -> [View action] -> [View action]
rotated x y z = wrapEntity [ prop_ "rotation" (msListOf show [x, y, z]) ]

scaled :: Float -> Float -> Float -> [View action] -> [View action]
scaled x y z = wrapEntity [ prop_ "scale" (msListOf show [x, y, z]) ]

-- * Helpers

wrapTag :: MisoString -> [Attribute action] -> [View action] -> [View action]
wrapTag name attrs contents =
  [ nodeHtml name attrs contents ]

wrapEntity :: [Attribute action] -> [View action] -> [View action]
wrapEntity = wrapTag "a-entity"

msListOf :: ToMisoString s => (a -> s) -> [a] -> MisoString
msListOf f = mconcat . List.intersperse " " . map (ms . f)
