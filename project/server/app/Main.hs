module Main where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant
import           System.Environment       (getArgs)

import           Corridor.Server          (api, server)

main :: IO ()
main = do
  args <- getArgs
  case args of
    staticDir : _ -> do
      putStrLn $ "Static files served from " ++ staticDir
      putStrLn $ "Starting corridor-server at " ++ serverUrl
      putStrLn $ "Main application at " ++ mainUrl
      Warp.run port $ Servant.serve api (server staticDir)
    _ -> do
      putStrLn "Directory with static files not provided!"
  where
    port = 8019
    serverUrl = "http://localhost:" ++ show port
    mainUrl = serverUrl ++ "/static/index.html"
