module ARCube.Server.Main where

import           Data.Function               ((&))
import           Data.Maybe                  (fromMaybe)
import           Network.Socket              (HostName)
import qualified Network.Wai.Handler.Warp    as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import qualified Servant
import qualified System.Environment          as System
import qualified System.Process              as System

import           ARCube.Server               (api, server)

-- | Entry point parametrised by some options.
--
-- For now we only extract directory with static files
-- from the first argument.
mainWith :: String -> [String] -> IO ()
mainWith hostname args =
  case args of
    staticDir : _ -> do
      putStrLn "============================================================"
      putStrLn $ "Static files served from " ++ staticDir
      putStrLn $ "Starting ar-cube-server at " ++ serverUrl
      putStrLn "------------------------------------------------------------"
      putStrLn $ "Open VR application (on a desktop) at\n" ++ desktopUrl
      putStrLn $ "Open AR application (on a smartphone) at " ++ mainUrl
      putStrLn $ "Open AR marker (on a desktop) at\n" ++ markerUrl
      putStrLn "------------------------------------------------------------"
      Warp.runTLS Warp.defaultTlsSettings settings $
        Servant.serve api (server staticDir)
    _ -> do
      putStrLn "Directory with static files not provided!"
  where
    port = 8019
    serverUrl = "https://" ++ hostname ++ ":" ++ show port
    localhostUrl = "https://localhost:" ++ show port
    mainUrl = serverUrl ++ "/play/" -- NOTE: trailing slash is important!
    markerUrl = localhostUrl ++ "/play/assets/markers/lc-2019-marker.png"
    desktopUrl = localhostUrl ++ "/play/desktop.html"
    settings = Warp.defaultSettings
      & Warp.setPort port

-- | Default entry point for running production application.
defaultMain :: IO ()
defaultMain = do
  args <- System.getArgs
  hostname <- fromMaybe "localhost" <$> localHostName
  mainWith hostname args

-- | Default entry point for running from GHCi.
devMain :: IO ()
devMain = do
  dir <- devStaticDir
  hostname <- fromMaybe "localhost" <$> localHostName
  mainWith hostname [dir]

-- | Figure out where are static files for running during development.
-- Useful in GHCi.
devStaticDir :: IO FilePath
devStaticDir = do
  localInstallRoot <- concat . lines <$> System.readProcess "stack"
    ["path", "--stack-yaml=stack-ghcjs.yaml", "--local-install-root"] ""
  return (localInstallRoot ++ "/bin/ar-cube-client.jsexe/")

-- | Try extracting local host name from @LOCAL_IP_ADDRESS@ variable.
localHostName :: IO (Maybe HostName)
localHostName = System.lookupEnv "LOCAL_IP_ADDRESS"
