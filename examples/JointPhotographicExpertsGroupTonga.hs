{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

module JointPhotographicExpertsGroupTonga where

import BasePrelude hiding (words, intercalate)
import Control.Lens ((^.))
import Data.Attoparsec.Text.Lazy
import Data.Text.Lazy hiding (filter)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.HTTP.Types (status200)
import Network.Linklater (say, slash, Channel(..), Command(..), User(..), Config(..), Message(..), Icon(..))
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wreq hiding (params)

findUrl :: Text -> Maybe Text
findUrl = fmap fromStrict . maybeResult . parse (manyTill (notChar '\n') (string "src=\"") *> takeTill (== '"'))

messageOf :: User -> Channel -> Text -> Text -> Message
messageOf (User u) c search = Message (EmojiIcon "gift") c . mappend (mconcat ["@", u, " Hello, wanderer. I found you this for \"", search, "\": "])

jpgto :: Maybe Command -> Application
jpgto (Just (Command user channel (Just text))) _ respond = do
  message <- get url >>= (return . fmap (messageOf user channel text) . findUrl . decodeUtf8 . flip (^.) responseBody)
  case (debug, message) of
    (True, _) -> putStrLn ("+ Pretending to post " <> show message) >> respondWith ""
    (False, Just m) -> config' >>= say m >> respondWith ""
    (False, Nothing) -> respondWith "Something went wrong!"
  where ourHeaders = [("Content-Type", "text/plain")]
        respondWith = respond . responseLBS status200 ourHeaders
        config' = (Config "trello.slack.com" . pack . filter (/= '\n')) <$> readFile "token"
        url = "http://" <> (unpack . intercalate "." . words $ text) <> ".jpg.to/"
        debug = True

main :: IO ()
main = let port = 3000 in putStrLn ("+ Listening on port " <> show port) >> run port (slash jpgto)
