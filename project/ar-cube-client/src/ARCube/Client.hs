{-# LANGUAGE OverloadedStrings #-}
module ARCube.Client where

import           Control.Concurrent          (forkIO)
import           Control.Concurrent.STM
import           Control.Monad               (forever)
import           Data.Monoid                 ((<>))
import qualified JavaScript.Web.MessageEvent as MessageEvent
import           JavaScript.Web.WebSocket    (WebSocket, WebSocketRequest (WebSocketRequest))
import qualified JavaScript.Web.WebSocket    as WebSocket
import           Miso                        (App (App), Effect, View, (<#))
import qualified Miso
import           Miso.String                 (MisoString, fromMisoString, ms)
import           Text.Read                   (readMaybe)

import           ARCube.Game
import           ARCube.Utils                (sceneAR, sceneVR)

foreign import javascript unsafe
  "document.location.hostname"
  getDocumentLocationHostname :: IO MisoString

foreign import javascript unsafe
  "document.location.port"
  getDocumentLocationPort :: IO MisoString

-- | Run client application.
run :: Mode -> IO ()
run mode = do
  messageQueue <- newTQueueIO
  hostname <- getDocumentLocationHostname
  port <- getDocumentLocationPort
  websocket <- WebSocket.connect WebSocketRequest
    { WebSocket.url = "wss://" <> hostname <> ":" <> port <> "/connect"
    , WebSocket.protocols = ["wss"]
    , WebSocket.onClose = Nothing
    , WebSocket.onMessage = Just $ \event -> do
        putStrLn "Received new message event from server!"
        case MessageEvent.getData event of
          MessageEvent.StringData str -> do
            case readMaybe (fromMisoString str) of
              Nothing -> do
                putStrLn "Could not parse message event:"
                putStrLn (fromMisoString str)
              Just game -> atomically $
                writeTQueue messageQueue (game :: Game)
          _ -> return ()
    }
  Miso.startApp App
    { Miso.initialAction   = initAction
    , Miso.model           = initialModel websocket
    , Miso.update          = updateModel
    , Miso.view            = viewModel mode
    , Miso.subs            = [ syncWithServer messageQueue ]
    , Miso.events          = Miso.defaultEvents
    , Miso.mountPoint      = Nothing
    }
  where
    syncWithServer messageQueue handleSync = do
      _threadId <- forkIO $ forever $ do
        game <- atomically $ readTQueue messageQueue
        putStrLn "New game update from server!"
        handleSync (Sync game)
      return ()

-- | Mode to run.
data Mode = VR | AR

-- * Model

-- | Client's state model.
data Model = Model
  { modelWebSocket :: WebSocket
  , modelState     :: ModelState
  }

instance Eq Model where
  m1 == m2 = modelState m1 == modelState m2

data ModelState
  = Loading
  | InGame Game
  deriving (Eq)

-- | Initialise model.
initialModel :: WebSocket -> Model
initialModel websocket = Model websocket Loading

-- * Actions

-- | Client's possible actions.
data Action
  = NoOp                      -- ^ A special do-nothing action.
  | StartGame                 -- ^ Start a game.
  | InGameAction GameAction   -- ^ An in-game action.
  | Sync Game

-- | Initial action to perfom on application start.
initAction :: Action
initAction = StartGame

-- * Handling model

-- | Client action handlers.
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp model      = pure model
updateModel StartGame model = pure model
  { modelState = InGame initGame }
updateModel (InGameAction gameAction) model@Model{modelState = InGame game}
  = model { modelState = InGame (handleGame gameAction game) } <# do
      WebSocket.send (ms (show gameAction)) (modelWebSocket model)
      putStrLn "Sent an action to the server!"
      return NoOp
updateModel (InGameAction _) model = pure model
updateModel (Sync game) model = pure model { modelState = InGame game }

-- | Render model in a given setting.
viewModel :: Mode -> Model -> View Action
viewModel mode model = scene mode views
  where
    scene AR = sceneAR
    scene VR = sceneVR

    views =
      case modelState model of
        Loading     -> []
        InGame game -> fmap InGameAction <$> renderGame game
