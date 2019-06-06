module ARCube.Server.Main where

import           Control.Concurrent          (forkIO)
import           Data.Function               ((&))
import           Data.Maybe                  (fromMaybe)
import           Network.Socket              (HostName)
import qualified Network.Wai.Handler.Warp    as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import qualified Servant
import qualified System.Environment          as System
import qualified System.Process              as System

import           ARCube.Server               (api, mkDefaultConfig,
                                              periodicUpdates, server)

-- | Entry point parametrised by some options.
--
-- For now we only extract directory with static files
-- from the first argument.
mainWith :: String -> [String] -> IO ()
mainWith hostname args =
  case args of
    vrDir : arDir : _ -> do
      putStrLn "============================================================"
      putStrLn $ "Static files (VR) served from " ++ vrDir
      putStrLn $ "Static files (AR) served from " ++ arDir
      putStrLn $ "Starting ar-cube-server at " ++ serverUrl
      putStrLn "------------------------------------------------------------"
      putStrLn $ "Local VR at " ++ desktopUrl
      putStrLn $ "Open VR at " ++ vrUrl
      putStrLn $ "Open AR at " ++ arUrl
      putStrLn $ "AR marker (open on desktop) at\n" ++ markerUrl
      putStrLn "------------------------------------------------------------"
      config <- mkDefaultConfig
      forkIO $ periodicUpdates 10000 config   -- update Universe every 10 milliseconds
      Warp.runTLS Warp.defaultTlsSettings settings $
        Servant.serve api (server vrDir arDir config)
    _ -> do
      putStrLn "Directory with static files not provided!"
  where
    port = 8019
    serverUrl = "https://" ++ hostname ++ ":" ++ show port
    localhostUrl = "https://localhost:" ++ show port
    vrUrl = serverUrl ++ "/vr/vr.html" -- NOTE: trailing slash is important!
    arUrl = serverUrl ++ "/ar/ar.html" -- NOTE: trailing slash is important!
    markerUrl = localhostUrl ++ "/ar/assets/markers/lc-2019-marker.png"
    desktopUrl = localhostUrl ++ "/vr/vr.html"
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
  dirs <- devStaticDirs
  hostname <- fromMaybe "localhost" <$> localHostName
  mainWith hostname dirs

-- | Figure out where are static files for running during development.
-- Useful in GHCi.
devStaticDirs :: IO [FilePath]
devStaticDirs = do
  localInstallRoot <- concat . lines <$> System.readProcess "stack"
    ["path", "--stack-yaml=stack-ghcjs.yaml", "--local-install-root"] ""
  return
    [ localInstallRoot ++ "/bin/ar-cube-client-vr.jsexe/"
    , localInstallRoot ++ "/bin/ar-cube-client-ar.jsexe/" ]

-- | Try extracting local host name from @LOCAL_IP_ADDRESS@ variable.
localHostName :: IO (Maybe HostName)
localHostName = System.lookupEnv "LOCAL_IP_ADDRESS"
