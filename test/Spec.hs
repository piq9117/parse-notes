{-# LANGUAGE ImportQualifiedPost #-}

module Spec (main) where

import Spec.Notes.Parser qualified
import Spec.Notes.Render qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain . testGroup "Parse Note Spec"
    =<< sequence
      [ Spec.Notes.Parser.test_testTree,
        Spec.Notes.Render.test_testTree
      ]
