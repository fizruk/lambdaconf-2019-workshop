{-# LANGUAGE EmptyCase  #-}
{-# LANGUAGE LambdaCase #-}
module Corridor.Game where

-- | Model of the game state.
data Game = Game Int
  deriving (Eq)

-- | Possible in-game actions.
data GameAction

-- | Initialise game state.
initGame :: Game
initGame = Game 0

-- | Handle in-game actions.
updateGame :: GameAction -> Game -> Game
updateGame = \case {}
