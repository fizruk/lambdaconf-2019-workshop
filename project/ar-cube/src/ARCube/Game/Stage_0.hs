{-# LANGUAGE EmptyCase          #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
module ARCube.Game.Stage_0 where

import           Miso

import           ARCube.Utils

-- | Model of the game state (empty for now).
data Game = Game
  deriving (Eq, Show, Read)

-- | Possible in-game actions (empty for now).
data GameAction
deriving instance Show GameAction
deriving instance Read GameAction

-- | Initialise game state.
initGame :: Game
initGame = Game

-- | Game event handler.
handleGame :: GameAction -> Game -> Game
handleGame = \case {}

-- | How to render game in a VR/AR scene.
renderGame :: Game -> [View GameAction]
renderGame _ = asciiToVoxels haskellLogo
