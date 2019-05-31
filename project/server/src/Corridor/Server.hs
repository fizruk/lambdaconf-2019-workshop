{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Corridor.Server where

import           Servant

import           Corridor.Game

-- | Corridor server API specification.
type API = "play" :> Raw

-- | Term-level representation of 'API'.
api :: Proxy API
api = Proxy

-- | Server implementing 'API'.
server
  :: FilePath   -- ^ Where to serve static files from?
  -> Server API
server staticDir = Servant.serveDirectory staticDir
