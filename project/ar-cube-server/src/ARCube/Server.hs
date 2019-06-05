{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module ARCube.Server where

import           Servant

import           ARCube.Game

-- | AR Cube server API specification.
type API
    = "vr" :> Raw
 :<|> "ar" :> Raw

-- | Term-level representation of 'API'.
api :: Proxy API
api = Proxy

-- | Server implementing 'API'.
server
  :: FilePath   -- ^ Where to serve VR static files from?
  -> FilePath   -- ^ Where to serve AR static files from?
  -> Server API
server vrDir arDir
    = Servant.serveDirectory vrDir
 :<|> Servant.serveDirectory arDir
