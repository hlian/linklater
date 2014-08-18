{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

-- Full package here: https://github.com/hlian/jpgtobot/
module JointPhotographicExpertsGroupTonga where

import BasePrelude hiding (words, intercalate, filter)
import Control.Lens ((^.))
import Data.Aeson (encode)
import Data.Attoparsec.Text.Lazy
import Data.Char (isLetter, isAscii)
import Data.Text.Lazy
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.Linklater (say, slashSimple, Command(..), Config(..), Message(..), Icon(..), Format(..))
import Network.Wai.Handler.Warp (run)
import Network.Wreq hiding (params)

findUrl :: Text -> Maybe Text
findUrl = fmap fromStrict . maybeResult . parse (manyTill (notChar '\n') (string "src=\"") *> takeTill (== '"'))

jpgto :: Maybe Command -> IO Text
jpgto (Just (Command commandText user channel (Just text))) = do
  message <- (fmap messageOf . findUrl . decodeUtf8 . flip (^.) responseBody) <$> get ("http://" <> (unpack subdomain) <> ".jpg.to/")
  case (debug, message) of
    (True, _) -> putStrLn ("+ Pretending to post " <> (unpack . decodeUtf8 . encode) message) >> return ""
    (False, Just m) -> config' >>= say m >> return ""
    (False, Nothing) -> return "Something went wrong!"
  where config' = (Config "trello.slack.com" . filter (/= '\n') . pack) <$> readFile "token"
        subdomain = (intercalate "." . fmap (filter isLetter . filter isAscii) . words) text
        messageOf url = FormattedMessage (EmojiIcon "gift") "jpgtobot" channel [FormatAt user, FormatLink url (subdomain <> ".jpg.to>")]
        debug = False
jpgto _ = return "Type more! (Did you know? jpgtobot is only 26 lines of Haskell. <https://github.com/hlian/jpgtobot/blob/master/Main.hs>)"

main :: IO ()
main = let port = 3000 in putStrLn ("+ Listening on port " <> show port) >> run port (slashSimple jpgto)
