{-# LANGUAGE OverloadedStrings #-}
module ARCube.Utils.AFrame where

import           Miso

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
sceneAR wrapped = nodeHtml "a-scene" [ prop_ "embedded" "", prop_ "arjs" "" ]
  [ nodeHtml "a-marker"
      [ prop_ "preset" "custom"
      , prop_ "type" "pattern"
      , prop_ "url" "assets/markers/lc-2019-marker.patt"
      ]
      wrapped
  , nodeHtml "a-entity" [ prop_ "camera" "" ] []
  ]

