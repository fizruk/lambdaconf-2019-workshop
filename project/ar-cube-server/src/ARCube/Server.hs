{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module ARCube.Server where

import           Servant

import           ARCube.Game

-- | AR Cube server API specification.
type API = "play" :> Raw

-- | Term-level representation of 'API'.
api :: Proxy API
api = Proxy

-- | Server implementing 'API'.
server
  :: FilePath   -- ^ Where to serve static files from?
  -> Server API
server staticDir = Servant.serveDirectory staticDir
