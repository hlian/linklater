{-# LANGUAGE OverloadedStrings #-}

module JointPhotographicExpertsGroupTonga where

import           Control.Applicative
import           Control.Lens hiding ((.=))
import           Control.Monad
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Format as F
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Data.Text.Lazy.IO
import           Data.Word8
import           Network.HTTP.Types (status200)
import           Network.Linklater
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Network.Wreq hiding (params, headers)
import           Prelude hiding (readFile, writeFile, putStrLn)

toText :: B.ByteString -> Text
toText = decodeUtf8 . L.fromChunks . return

token :: Text
token = undefined

urlParser :: Parser B.ByteString
urlParser = p
  where
    p = garbage *> url
    garbage = string "src=\"" <|> (P.take 1 *> garbage)
    url = takeTill (== _quotedbl)

urlFor :: Text -> IO Text
urlFor search = do
  r <- get (T.unpack $ F.format "http://{}.jpg.to/" [search])
  (return . handle . parse urlParser . strictly) (r ^. responseBody)
  where
    strictly = B.concat . L.toChunks
    handle (Fail i ctxs s) = error (show (i, ctxs, s))
    handle (Partial f) = handle (f "")
    handle (Done _ r) = toText r

jpgto :: Maybe Command -> Application
jpgto (Just (Command (User user) channel text)) req respond = do
  url <- urlFor (maybe "spolsky" id text)
  say (Message channel (response url) (EmojiIcon "gift")) config
  (respond . responseLBS status200 headers) ""
  where
    response url = F.format "@{} {}" (user, url)
    config = Config token "trello.slack.com"
    headers = [("Content-Type", "text/plain")]

main :: IO ()
main = do
  let port = 80
  putStrLn (F.format "+ Listening on port {}" [port])
  run port (slash jpgto)
  return ()
