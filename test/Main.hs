{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as Map

import Network.Linklater.Types

import Test.Tasty
import Test.Tasty.HUnit

commandOfParamsTest :: Assertion
commandOfParamsTest =
  commandOfParams incoming @?= fixture
  where
    fixture =
      Right (Command "prove" (User "Knuth") (Channel "C2147483705" "test") (Just "text"))
    incoming = Map.fromList [
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
      ]

unitTests :: TestTree
unitTests =
  testGroup "unit" [testCase "commandOfParams" commandOfParamsTest]

main :: IO ()
main =
  defaultMain unitTests
