module Main where

import BasePrelude hiding ((&))
import Control.Lens
import Network.Wreq
import Options.Applicative

data Input = Input { inputServer :: String, inputPort :: Int }
             deriving (Show, Eq)

linklater :: Input -> IO ()
linklater (Input server port) = do
  let opts = defaults & (param "user_name" .~ ["richard"]) & (param "command" .~ ["/jpeg"]) & (param "channel_name" .~ ["somechannel"]) & (param "text" .~ ["kittens"])
  response <- getWith opts ("http://" <> server <> ":" <> show port <> "/")
  print response

main :: IO ()
main =
  execParser parser >>= linklater
  where
    parser =
      info (helper <*> innerParser) (fullDesc <> progDesc "throw strings at your Linklater Slackbots")
    innerParser =
      Input <$> serverOption <*> (read <$> portOption)
    serverOption =
      strOption (long "server" <> metavar "SERVER" <> value "localhost" <> help "an IP address or a domain of the HTTP server")
    portOption =
      strOption (long "port" <> metavar "PORT" <> value "80" <> help "the port number of the HTTP server")
