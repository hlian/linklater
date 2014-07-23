{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Network.Linklater
-- Copyright: (c) The Linklaterteers
--
-- License: BSD-style
-- Stability: experimental
-- Portability: GHC
--
-- Features:
--
-- * Uses @Text@ everywhere so you can send your slash commands crazy Unicode characters all day long.
-- * Lovely documentation.
-- * Battle-tested.
--
-- Here's a @/jpgto@ bot! If you run this program and then tell Slack
-- about your server (incoming hook and custom slash command) and then
-- type @/jpgto diplomatico@ in one of your channels, you'll get the
-- image from @http://diplomatico.jpg.to@. How, you say? /Screen scraping/.
--
-- @
-- urlParser :: Parser B.ByteString
-- urlParser = p
--   where
--     p = garbage *> url
--     garbage = string "src=\"" <|> (P.take 1 *> garbage)
--     url = takeTill (== _quotedbl)
--
-- urlFor :: Text -> IO Text
-- urlFor search = do
--   r <- get (T.unpack $ F.format "http://{}.jpg.to/" [search])
--   (return . handle . parse urlParser . strictly) (r ^. responseBody)
--   where
--     strictly = B.concat . L.toChunks
--     handle (Fail i ctxs s) = error (show (i, ctxs, s))
--     handle (Partial f) = handle (f "")
--     handle (Done _ r) = toText r
--
-- jpgto :: Maybe Command -> Application
-- jpgto (Just (Command (User user) channel text)) req respond = do
--   url <- urlFor (maybe "spolsky" id text)
--   say (Message channel (response url) (EmojiIcon "gift")) config
--   (respond . responseLBS status200 headers) ""
--   where
--     response url = F.format "@{} {}" (user, url)
--     config = Config token "trello.slack.com"
--     headers = [("Content-Type", "text/plain")]
--
-- main :: IO ()
-- main = do
--   let port = 80
--   putStrLn (F.format "+ Listening on port {}" [port])
--   run port (slash jpgto)
--    return ()
-- @
--
-- For the full example (since this one is missing a ton of imports),
-- see the @examples/@ directory on GitHub.
--
-- https://github.com/hlian/linklater

module Network.Linklater
       (
         say,
         slash,
         Channel(..),
         User(..),
         Message(..),
         Config(..),
         Command(..),
         Icon(..),
       ) where

import Control.Applicative ((<$>))
import Data.Aeson
import Data.ByteString.Lazy as BSL
import qualified Data.Map as M
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import qualified Network.Wai as W
import Network.Wreq hiding (params)

-- | Where slash commands can come from, and where messages can go.
data Channel =
   -- | A public or private group.
    GroupChannel Text
  -- | A private conversation with your best friend (or lover?).
  | IMChannel Text
  deriving (Eq, Ord, Show)

-- | A username: no at-signs, just text!
newtype User = User Text deriving (Eq, Ord, Show)

-- | Incoming HTTP requests to the slash function get parsed into one
-- of these babies.
data Command = Command {
  -- | Who ran your slash command.
  _commandUser :: User,
  -- | Where the person ran your slash command.
  _commandChannel :: Channel,
  -- | Text for the slash command, if any.
  _commandText :: Maybe Text
  } deriving (Eq, Ord, Show)

-- | For example, @":stars2:"@. Future versions should support actual
-- images, I GUESS.
newtype Icon = EmojiIcon Text deriving (Eq, Ord, Show)

-- | Here's how you talk.
data Message = Message {
  -- | You need a channel. It can be a group channel or a private IM.
  -- Alert: if it's a private IM, it'll look like it came from
  -- slackbot (as of writing).
  _messageChannel :: Channel,
  -- | What you want to say. This will be parsed in full (parse=full).
  -- In the future I'll support other forms of parsing, hopefully.
  -- Poke me with a pull request if I forget.
  _messageText :: Text,
  -- | The icon you want your message to appear as.
  _messageIcon :: Icon
  } deriving (Eq, Ord, Show)

instance ToJSON Message where
  toJSON (Message channel text (EmojiIcon emoji)) =
    object [ "channel" .= stringOfChannel channel
           , "icon_emoji" .= TL.concat [":", emoji, ":"]
           , "parse" .= String "full"
           , "username" .= String "jpgtobot"
           , "text" .= text
           ]
    where
      stringOfChannel (GroupChannel c) = TL.concat ["#", c]
      stringOfChannel (IMChannel m) = TL.concat ["@", m]

-- | Like a curiosity about the world, you'll need one of these to say something.
data Config = Config {
  -- | This is the incoming web hook token that Slack gave you. It's usually a long alphanumberic string of garbage.
  _configIncomingHookToken :: Text,
  -- | This is where your Slack account is hosted. For example, 'trello.slack.com'.
  _configHostname :: Text
  }

-- | The 'say' function posts a Message, with a capital M, to Slack.
-- It'll, ahem, need your token first though.
say :: Message -> Config -> IO (Response ByteString)
say message config =
  post (TL.unpack url) (encode message)
  where
    -- TODO: use a URL builder
    url = TL.concat ["https://", _configHostname config, "/services/hooks/incoming-webhook?token=", _configIncomingHookToken config]

-- | A bot server! As if by magic. This acts like a WAI middleware in
-- that you let us wrap around your application.
slash :: (Maybe Command -> W.Application) -> W.Application
slash f req respond = f command req respond
  where
    command = do
      user <- userOf <$> wishFor "user_name"
      channel <- wishFor "channel_name" >>= channelOf user
      let text = wishFor "text"
      return (Command user channel text)
    wishFor key = case M.lookup (key :: Text) params of
      Just (Just "") -> Nothing
      Just (Just value) -> Just value
      _ -> Nothing
    userOf = User . TL.filter (/= '@')
    channelOf (User u) "directmessage" = Just (IMChannel u)
    channelOf _ "privategroup" = Nothing
    channelOf _ c = Just (GroupChannel c)
    params = M.fromList [(toText k, toText <$> v) | (k, v) <- W.queryString req]
    toText = TLE.decodeUtf8 . BSL.fromChunks . return
