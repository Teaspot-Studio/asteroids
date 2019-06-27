module Main where

import Asteroids.Back
import Data.Semigroup ((<>))
import Network.Wai.Handler.Warp
import Options.Applicative

data Options = Options {
  staticsPath :: FilePath
, listenPort  :: Port
}

options :: Parser Options
options = Options
  <$> strOption
      (  long "statics"
      <> metavar "STATICS_PATH"
      <> help "Folder with static HTML/CSS/JS to serve."
      <> value "../front/statics"
      <> showDefault )
  <*> option auto
      (  long "port"
      <> metavar "PORT_NUMBER"
      <> help "Which port the server listens."
      <> value 8080
      <> showDefault  )

main :: IO ()
main = app =<< execParser opts
  where
    opts = info (options <**> helper)
       ( fullDesc
      <> progDesc "Server for asteroids game"
      <> header "asteroids-back - stub server to serve statics for asteroids game" )

app :: Options -> IO ()
app Options{..} = do
  putStrLn $ "Started listening on 127.0.0.1:" ++ show listenPort
  run listenPort $ staticsApp staticsPath
