{-# LANGUAGE ImportQualifiedPost #-}

module Spec (main) where

import Spec.Notes.Parser qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain . testGroup "Parse Note Spec"
    =<< sequence
      [ Spec.Notes.Parser.test_testTree
      ]
