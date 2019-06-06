{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
module ARCube.Server where

import           Control.Concurrent             (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Exception              (catch)
import           Control.Monad                  (forever, when)
import qualified Data.ByteString.Lazy.Char8     as BSL8
import           Data.Map                       (Map)
import qualified Data.Map                       as Map

import           Network.HTTP.Types             (status400)
import           Network.Wai                    (responseLBS)
import           Network.Wai.Handler.WebSockets (websocketsOr)
import           Network.WebSockets
import           Servant


import           ARCube.Game

-- | AR Cube server API specification.
type API
    = "vr" :> Raw
 :<|> "ar" :> Raw
 :<|> GameAPI

-- | Term-level representation of 'API'.
api :: Proxy API
api = Proxy

-- | Server implementing 'API'.
server
  :: FilePath   -- ^ Where to serve VR static files from?
  -> FilePath   -- ^ Where to serve AR static files from?
  -> Config
  -> Server API
server vrDir arDir config
    = Servant.serveDirectory vrDir
 :<|> Servant.serveDirectory arDir
 :<|> gameServer config

type GameAPI = "connect" :> Raw

-- | Server config.
data Config = Config
  { configGame     :: TVar Game
  , configGamePrev :: TVar (Maybe Game)
  , configClients  :: TVar (Map PlayerName Client)
  , configNames    :: TVar [PlayerName]
  }

type PlayerName = String

-- | A client is represented by its websocket 'Connection'.
type Client = Connection

-- | Default server config with initial game and no clients.
mkDefaultConfig :: IO Config
mkDefaultConfig = do
  cfg <- atomically $ Config
          <$> newTVar initGame
          <*> newTVar Nothing
          <*> newTVar Map.empty
          <*> newTVar (map show [1..])
  return cfg

-- | The Game of Snakes server 'Application'.
gameServer :: Config -> Server GameAPI
gameServer config = websocketsOr defaultConnectionOptions wsApp backupApp
  where
    wsApp :: ServerApp
    wsApp pending_conn = do
        conn <- acceptRequest pending_conn
        name <- addClient conn config
        putStrLn $ name ++ " joined!"
        handleActions name conn config

    -- this application will be used for non-websocket requests
    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

-- | Add a new client to the server state.
-- This will update 'configClients' and add
-- a new player to the 'configGame'.
addClient :: Client -> Config -> IO PlayerName
addClient client Config{..} = do
  atomically $ do
    name:names <- readTVar configNames
    writeTVar configNames names
    modifyTVar configClients (Map.insert name client)
    return name

-- | An infinite loop, receiving data from the 'Client'
-- and handling its actions via 'handlePlayerAction'.
handleActions :: PlayerName -> Connection -> Config -> IO ()
handleActions name conn cfg@Config{..} = forever $ do
  action <- receiveData conn
  putStrLn ("Received action from client " ++ name ++ ": " ++ show action)
  atomically $ do
    game <- readTVar configGame
    writeTVar configGame (handleGame action game)
    writeTVar configGamePrev (Just game)

-- | Periodically update the 'Game' and send updates to all the clients.
periodicUpdates :: Int -> Config -> IO ()
periodicUpdates ms cfg@Config{..} = forever $ do
  threadDelay ms -- wait ms milliseconds
  gamePrev <- readTVarIO configGamePrev
  game <- readTVarIO configGame
  when (Just game /= gamePrev) $ do
    broadcastUpdate game cfg
    atomically $ writeTVar configGamePrev (Just game)
  where
    -- FIXME: (ms / 10^6) is not the actual time that has passed since the previous update
    -- we should use getCurrentTime to more accurately keep track of time deltas
    secs = fromIntegral ms / 1000000

-- | Send every 'Client' updated 'Game' concurrently.
broadcastUpdate :: Game -> Config -> IO ()
broadcastUpdate game cfg@Config{..} = do
  clients <- readTVarIO configClients
  mapM_ (forkIO . sendUpdate) (Map.toList clients)
  where
    sendUpdate (name, conn) = send name conn `catch` handleClosedConnection name
      where
        send name conn = do
          sendTextData conn game
          putStrLn ("Sent update to client " ++ name)

    handleClosedConnection :: PlayerName -> ConnectionException -> IO ()
    handleClosedConnection name _ = do
      putStrLn (name ++ " disconnected.")
      atomically $ do
        modifyTVar configClients  (Map.delete name)

-- =====================================
-- WebSockersData instances are needed
-- to send/receive Haskell structures
-- over websockets
-- =====================================

instance WebSocketsData Game where
  fromLazyByteString = read . BSL8.unpack
  toLazyByteString   = BSL8.pack . show

instance WebSocketsData GameAction where
  fromLazyByteString = read . BSL8.unpack
  toLazyByteString   = BSL8.pack . show

