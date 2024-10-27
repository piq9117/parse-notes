{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Notes.Parser where

import Notes.Parser
  ( Note (..),
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
import Text.Megaparsec qualified

smolParser :: Spec
smolParser =
  describe "smol parser" $ do
    it "noteTitle" $ do
      (parse noteTitle "test" "-- # Note [Hello, this is a title]")
        `shouldBe` (Right "Hello, this is a title")

      (parse noteTitle "test" "-- # Note [Hello, this is a title]\n")
        `shouldBe` (Right "Hello, this is a title")

    it "noteBodyLine" $
      (parse noteBodyLine "test" "-- this is the body of the note\n")
        `shouldBe` (Right "this is the body of the note")

    it "noteBody" $
      (parse noteBody "test" "-- first line of the note\n-- second line of the note")
        `shouldBe` (Right ["first line of the note", "second line of the note"])

    it "uuid" $
      (parse Notes.Parser.uuid "test" "2efcf3a3-1f17-4f3a-8e6a-ea0fe2bac197")
        `shouldBe` (Right "2efcf3a3-1f17-4f3a-8e6a-ea0fe2bac197")

    it "nonNoteLine" $
      parse Notes.Parser.nonNoteLine "test" "this is not a note"
        `shouldBe` (Right "this is not a note")

    it "nonNotesParser" $
      parse Notes.Parser.nonNotesParser "test" "this is not a note\nthis is another line of non note"
        `shouldBe` ( Right
                       [ Notes.Parser.NonNote "this is not a note",
                         Notes.Parser.NonNote "this is another line of non note"
                       ]
                   )

parser :: Spec
parser =
  describe "parser" $ do
    it "blanklineParser" $ do
      parse Notes.Parser.blanklineParser "test" "\n"
        `shouldBe` (Right Notes.Parser.Blankline)

    it "noteBodyLineParser" $ do
      (parse Notes.Parser.noteBodyLineParser "test" "-- this is a comment\n")
        `shouldBe` (Right $ Notes.Parser.BodyContent "this is a comment")

    it "noteBodyParser" $
      (parse Notes.Parser.noteBodyParser "test" "-- this is a comment\n-- this is another comment")
        `shouldBe` ( Right
                       [ Notes.Parser.BodyContent "this is a comment",
                         Notes.Parser.BodyContent "this is another comment"
                       ]
                   )

    it "noteTitleParser" $
      (parse Notes.Parser.noteTitleParser "test" "-- # Note [This is a title]")
        `shouldBe` (Right $ Notes.Parser.NoteTitle "This is a title")

    it "noteBodyIdParser" $
      (parse Notes.Parser.noteBodyIdParser "test" "-- id:2efcf3a3-1f17-4f3a-8e6a-ea0fe2bac197")
        `shouldBe` (Right $ Notes.Parser.BodyId "2efcf3a3-1f17-4f3a-8e6a-ea0fe2bac197")

    it "noteParser" $ do
      (parse Notes.Parser.noteParser "test" "-- # Note [This is this is the title of the note]\n-- First line of the note\n-- Second line of the note")
        `shouldBe` ( Right
                       Note
                         { title = Notes.Parser.NoteTitle "This is this is the title of the note",
                           body =
                             [ Notes.Parser.BodyContent "First line of the note",
                               Notes.Parser.BodyContent "Second line of the note"
                             ]
                         }
                   )

    it "fileNoteParser" $ do
      parse Notes.Parser.fileNoteParser "test" "-- # Note [This is the title of the note]\n-- First line of the note\n-- Second line of the note\n-- id:2efcf3a3-1f17-4f3a-8e6a-ea0fe2bac197\n\n\n"
        `shouldBe` ( Right $
                       Notes.Parser.NoteContent
                         ( Note
                             { title = Notes.Parser.NoteTitle "This is the title of the note",
                               body =
                                 [ Notes.Parser.BodyContent "First line of the note",
                                   Notes.Parser.BodyContent "Second line of the note",
                                   Notes.Parser.BodyId "2efcf3a3-1f17-4f3a-8e6a-ea0fe2bac197"
                                 ]
                             }
                         )
                   )

    it "fileParser" $ do
      parse
        Notes.Parser.fileParser
        "test"
        "-- # Note [This is the title of the note]\n-- First line of the note\n-- Second line of the note\n-- id:2efcf3a3-1f17-4f3a-8e6a-ea0fe2bac197\n\n\n"
        `shouldBe` ( Right
                       [ Notes.Parser.NoteContent
                           ( Note
                               { title = Notes.Parser.NoteTitle "This is the title of the note",
                                 body =
                                   [ Notes.Parser.BodyContent "First line of the note",
                                     Notes.Parser.BodyContent "Second line of the note",
                                     Notes.Parser.BodyId "2efcf3a3-1f17-4f3a-8e6a-ea0fe2bac197"
                                   ]
                               }
                           ),
                         Notes.Parser.Blankline,
                         Notes.Parser.Blankline
                       ]
                   )

test_testTree :: IO TestTree
test_testTree =
  testSpec "Parser Spec" $ do
    smolParser
    parser
