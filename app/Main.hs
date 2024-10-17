module Main where

import Notes.File (parseFile)

main :: IO ()
main = do
  parseFile "./test/sample-file/TestFile.hs"
