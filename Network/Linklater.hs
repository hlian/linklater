{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module: Network.Linklater
-- Copyright: (c) The Linklaterteers
-- License: BSD-style
-- Maintainer: hi@haolian.org
-- Stability: experimental
-- Portability: GHC
--
-- Here's a @/jpgto@ bot! If you run this program and then tell Slack
-- about your server (incoming hook and custom slash command) and then
-- type @/jpgto baby corgi@ in one of your channels, you'll get the
-- image from @http://baby.corgi.jpg.to@.
--
-- <https://github.com/hlian/linklater/blob/master/examples/JointPhotographicExpertsGroupTonga.hs>
--
-- One @/jpgto baby corgi@, et voila.
--
-- <<https://raw.githubusercontent.com/hlian/linklater/6232b950a333cfa6d5fffea997ec9ab8c2ce31ba/corgi.jpg>>

module Network.Linklater
       (
         say,
         slash,
         slashSimple,
         Channel(..),
         User(..),
         Message(..),
         Config(..),
         Command(..),
         Icon(..),
         Format(..)
       ) where

import           BasePrelude
import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Network.HTTP.Types (status200)
import qualified Network.Wai as W
import           Network.Wreq hiding (params, headers)

-- | Where 'slash' commands come from, and where 'Message's go.
data Channel =
   -- | A public or private group.
    GroupChannel Text
  -- | A private conversation with your best friend -- or lover ;).
  | IMChannel Text
  deriving (Eq, Ord, Show)

-- | A username: no at-signs, just text!
newtype User = User Text deriving (Eq, Ord, Show)

-- | Incoming HTTP requests to the slash function get parsed into one
-- of these babies.
data Command = Command {
  -- | The command name.
  _commandName :: Text,
  -- | Who ran your slash command.
  _commandUser :: User,
  -- | Where the person ran your slash command.
  _commandChannel :: Channel,
  -- | Text for the slash command, if any.
  _commandText :: Maybe Text
  } deriving (Eq, Ord, Show)

-- | The icon next to the messages you `say`. (Images unsupported
-- right now, sorry.)
newtype Icon =
  -- | For example, ":stars2:".
  EmojiIcon Text deriving (Eq, Ord, Show)

-- | A little DSL for <https://api.slack.com/docs/formatting Slack formatting>.
data Format =
  -- | @"\<\@user|user>"@
    FormatAt User
  -- | @"\<\@user|user did this and that>"@
  | FormatUser User Text
  -- | @"\<http://example.com|user did this and that>"@
  | FormatLink Text Text
  -- | @"user did this &amp; that"@
  | FormatString Text

unformat :: Format -> Text
unformat (FormatAt user@(User u)) = unformat (FormatUser user u)
unformat (FormatUser (User u) t) = "<@" <> u <> "|" <> t <> ">"
unformat (FormatLink url t) = "<" <> url <> "|" <> t <> ">"
unformat (FormatString t) = foldr (uncurry T.replace) t [("<", "&lt;"), (">", "&gt;"), ("&", "&amp;")]

-- | Here's how you talk: you make one of these and pass it to 'say'.
-- Before the day is done, Linklater will convert this to a JSON blob
-- using 'Data.Aeson'.
--
--   * Simple messages are parsed by Slack with parse=full (i.e. as if you had typed it into the input box).
--
--   * Complex messages are parsed according to Slack formatting. See 'Format'.
--
data Message =
    SimpleMessage Icon Text Channel Text
  | FormattedMessage Icon Text Channel [Format]

instance ToJSON Channel where
  toJSON (GroupChannel c) =
    String ("#" <> c)
  toJSON (IMChannel im) =
    String ("@" <> im)

instance ToJSON Message where
  toJSON m = case m of
    (FormattedMessage emoji username channel formats) ->
      toJSON_ emoji username channel (T.intercalate " " (fmap unformat formats)) False
    (SimpleMessage emoji username channel text) ->
      toJSON_ emoji username channel text True
    where
      toJSON_ (EmojiIcon emoji) username channel raw toParse =
        object [ "channel" .= channel
               , "icon_emoji" .= (":" <> emoji <> ":")
               , "parse" .= String (if toParse then "full" else "poop")
               , "username" .= username
               , "text" .= raw
               , "unfurl_links" .= True
               ]

-- | Like a curiosity about the world, you'll need one of these to
-- 'say' something.
data Config = Config {
  -- | This is the incoming web hook URL that Slack gave you. It's
  -- usually @https://hooks.slack.com/services/...@.
  _configHookURL :: Text
  }

-- | The 'say' function posts a 'Message', with a capital M, to Slack.
-- It'll, however, need a 'Config' (a.k.a. incoming token) first.
say :: Message -> Config -> IO (Response BSL.ByteString)
say message Config{..} =
  post (T.unpack _configHookURL) (encode message)

-- | A bot server for people who are in a hurry. Make a function that
-- takes a 'Command' and returns some 'Text' in 'IO' world, and we'll
-- convert it into a 'Network.WAI' application. If you want more
-- control over the request and respond, see 'slash'.
slashSimple :: (Maybe Command -> IO Text) -> W.Application
slashSimple f =
  slash (\command _ respond -> f command >>= (respond . makeResponse . TL.fromStrict))
  where
    headers =
      [("Content-type", "text/plain")]
    makeResponse =
      W.responseLBS status200 headers . TLE.encodeUtf8

channelOf :: User -> Text -> Maybe Channel
channelOf (User u) "directmessage" =
  Just (IMChannel u)
channelOf _ "privategroup" =
  Nothing
channelOf _ c =
  Just (GroupChannel c)

-- | A bot server! As if by magic. This acts like a 'Network.WAI'
-- middleware: Linklater wraps around your application. (Really, it
-- just gives you a 'Command' to work with instead of a raw HTTP
-- request.)
slash :: (Maybe Command -> W.Application) -> W.Application
slash f req respond = f command req respond
  where
    command = do
      user <- userOf <$> wishFor "user_name"
      Command <$> (nameOf <$> wishFor "command")
              <*> return user
              <*> (wishFor "channel_name" >>= channelOf user)
              <*> return (wishFor "text")
    wishFor key =
      case M.lookup (key :: TL.Text) params of
       Just (Just "") ->
         Nothing
       Just (Just value) ->
         Just (TL.toStrict value)
       _ ->
         Nothing
    userOf = User . T.filter (/= '@')
    nameOf = T.filter (/= '/')
    params = M.fromList [(toText k, toText <$> v) | (k, v) <- W.queryString req]
    toText = TLE.decodeUtf8 . BSL.fromChunks . return
