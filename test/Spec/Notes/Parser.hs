{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Notes.Parser where

import Notes.Parser
  ( Note (..),
    note,
    noteBody,
    noteBodyLine,
    noteTitle,
    parseNotes,
  )
import Notes.Parser qualified
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Text.Megaparsec (parse)

parser :: Spec
parser =
  describe "parser" $ do
    it "should parse the title" $ do
      (parse noteTitle "test" "-- # Note [Hello, this is a title]")
        `shouldBe` (Right "Hello, this is a title")

      (parse noteTitle "test" "-- # Note [Hello, this is a title]\n")
        `shouldBe` (Right "Hello, this is a title")

    it "should parse the body" $
      (parse noteBodyLine "test" "-- this is the body of the note\n")
        `shouldBe` (Right "this is the body of the note")

    it "should parse all the body" $
      (parse noteBody "test" "-- first line of the note\n-- second line of the note\n")
        `shouldBe` (Right ["first line of the note", "second line of the note"])

    it "should parse note" $ do
      (parse note "test" "-- # Note [This is this is the title of the note]\n-- First line of the note\n-- Second line of the note")
        `shouldBe` ( Right
                       Note
                         { title = Notes.Parser.NoteTitle "This is this is the title of the note",
                           body =
                             [ Notes.Parser.BodyContent "First line of the note",
                               Notes.Parser.BodyContent "Second line of the note"
                             ]
                         }
                   )

    it "note body parser" $
      (parse Notes.Parser.noteBodyLineParser "test" "-- this is a comment")
        `shouldBe` (Right $ Notes.Parser.BodyContent "this is a comment")

    it "note title parser" $
      (parse Notes.Parser.noteTitleParser "test" "-- # Note [This is a title]")
        `shouldBe` (Right $ Notes.Parser.NoteTitle "This is a title")

    it "parseNotes" $ do
      (parseNotes "-- # Note [This is this is the title of the note]\n-- First line of the note\n-- Second line of the note")
        `shouldBe` [ Note
                       { title = Notes.Parser.NoteTitle "This is this is the title of the note",
                         body =
                           [ Notes.Parser.BodyContent "First line of the note",
                             Notes.Parser.BodyContent "Second line of the note"
                           ]
                       }
                   ]

test_testTree :: IO TestTree
test_testTree =
  testSpec "Parser Spec" $ do
    parser
