module Main where

import Notes.File (parseFile)

main :: IO ()
main = do
  notes <- parseFile "./test/sample-file/TestFile.hs"
  print notes
