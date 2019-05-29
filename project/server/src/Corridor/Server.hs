{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Corridor.Server where

import           Servant

import           Corridor

type API = "static" :> Raw

api :: Proxy API
api = Proxy

server :: FilePath -> Server Raw
server staticDir = Servant.serveDirectory staticDir
