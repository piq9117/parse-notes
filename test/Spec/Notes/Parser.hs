module Spec.Notes.Parser where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

parser :: Spec
parser =
  describe "parser" $ do
    it "should blah" $ do
      1 `shouldBe` 2

test_testTree :: IO TestTree
test_testTree =
  testSpec "Parser Spec" $ do
    parser
