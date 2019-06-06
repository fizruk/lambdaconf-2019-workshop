{-# LANGUAGE OverloadedStrings #-}
module ARCube.Standalone where

import           Miso         (App (App), Effect, View)
import qualified Miso

import           ARCube.Game
import           ARCube.Utils (sceneAR, sceneVR)

-- | Run client application.
run :: Mode -> IO ()
run mode = Miso.startApp App
  { Miso.initialAction   = initAction
  , Miso.model           = initialModel
  , Miso.update          = updateModel
  , Miso.view            = viewModel mode
  , Miso.subs            = [ ]
  , Miso.events          = Miso.defaultEvents
  , Miso.mountPoint      = Nothing
  }

-- | Mode to run.
data Mode = VR | AR

-- * Model

-- | Client's state model.
data Model
  = Loading
  | InGame Game
  deriving (Eq)

-- | Initialise model.
initialModel :: Model
initialModel = Loading

-- * Actions

-- | Client's possible actions.
data Action
  = NoOp                      -- ^ A special do-nothing action.
  | StartGame                 -- ^ Start a game.
  | InGameAction GameAction   -- ^ An in-game action.

-- | Initial action to perfom on application start.
initAction :: Action
initAction = StartGame

-- * Handling model

-- | Client action handlers.
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp model       = pure model
updateModel StartGame _model = pure (InGame initGame)
updateModel (InGameAction gameAction) (InGame game)
  = pure (InGame (handleGame gameAction game))
updateModel (InGameAction _) model = pure model

-- | Render model in a given setting.
viewModel :: Mode -> Model -> View Action
viewModel mode model = scene mode views
  where
    scene AR = sceneAR
    scene VR = sceneVR

    views =
      case model of
        Loading     -> []
        InGame game -> fmap InGameAction <$> renderGame game
