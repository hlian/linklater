{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as Map

import Data.Bifunctor (bimap)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Base (urlEncodeVars)
import Network.Linklater.Types

import Test.Tasty
import Test.Tasty.HUnit

commandOfParamsTest :: Assertion
commandOfParamsTest =
  commandOfParams fullBody incoming @?= fixture
  where
    fixture =
      Right (Command "prove" (User "Knuth") (Channel "C2147483705" "test") (Just "text") "https://hooks.slack.com/commands/00/B4R" "token=XXXXXXXXXXXXXXXXXX&team_id=T0001&team_domain=example&channel_id=C2147483705&channel_name=test&timestamp=1355517523.000005&user_id=U2147483697&user_name=Knuth&text=text&command=%2Fprove&response_url=https%253A%252F%252Fhooks.slack.com%252Fcommands%252F00%252FB4R")
    vars = [
        ("token", "XXXXXXXXXXXXXXXXXX")
      , ("team_id", "T0001")
      , ("team_domain", "example")
      , ("channel_id", "C2147483705")
      , ("channel_name", "test")
      , ("timestamp", "1355517523.000005")
      , ("user_id", "U2147483697")
      , ("user_name", "Knuth")
      , ("text", "text")
      , ("command", "/prove")
      , ("response_url", "https%3A%2F%2Fhooks.slack.com%2Fcommands%2F00%2FB4R")
      ]
    fullBody = T.encodeUtf8
      . T.pack
      . urlEncodeVars
      . fmap (bimap T.unpack T.unpack)
      $ vars
    incoming = Map.fromList vars

unitTests :: TestTree
unitTests =
  testGroup "unit" [testCase "commandOfParams" commandOfParamsTest]

main :: IO ()
main =
  defaultMain unitTests
