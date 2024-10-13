{-# LANGUAGE OverloadedStrings #-}

module Spec.Notes.Parser where

import Notes.Parser (body, title)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Text.Megaparsec (parse)

parser :: Spec
parser =
  describe "parser" $ do
    it "should parse the title" $ do
      (parse title "test" "-- # Note [Hello, this is a title]")
        `shouldBe` (Right "Hello, this is a title")
    it "should parse the body" $
      (parse body "test" "-- this is the body of the note")
        `shouldBe` (Right "this is the body of the note")

test_testTree :: IO TestTree
test_testTree =
  testSpec "Parser Spec" $ do
    parser
