{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Notes.Parser where

import Data.Maybe (fromJust)
import Data.UUID qualified
import Notes.Parser qualified
import Test.Hspec (Spec, describe, it, shouldBe, xit)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Text.Megaparsec (parse)
import Text.Megaparsec qualified
import Text.Megaparsec.Char qualified

smolParser :: Spec
smolParser =
  describe "smol parser" $ do
    it "noteTitle" $ do
      (parse Notes.Parser.noteTitle "test" "-- # Note [Hello, this is a title]")
        `shouldBe` (Right "Hello, this is a title")

      (parse Notes.Parser.noteTitle "test" "-- # Note [Hello, this is a title]\n")
        `shouldBe` (Right "Hello, this is a title")

    it "noteBodyLine" $ do
      (parse Notes.Parser.noteBodyLine "test" "-- this is the body of the note\n")
        `shouldBe` (Right "this is the body of the note")

      parse (many $ Notes.Parser.noteBodyLine) "test" "-- id:0d8a653d-166a-4f37-894d-06bf44bddcdd\n"
        `shouldBe` (Right [])

      (parse Notes.Parser.noteBodyLine "test" "-- this is the body of the note")
        `shouldBe` (Right "this is the body of the note")

    it "noteBody" $
      (parse Notes.Parser.noteBody "test" "-- first line of the note\n-- second line of the note")
        `shouldBe` (Right ["first line of the note", "second line of the note"])

    it "uuid" $
      (parse Notes.Parser.uuid "test" "2efcf3a3-1f17-4f3a-8e6a-ea0fe2bac197")
        `shouldBe` (Right $ fromJust $ Data.UUID.fromText "2efcf3a3-1f17-4f3a-8e6a-ea0fe2bac197")

    it "uuidbodyLine" $ do
      (parse Notes.Parser.uuidBodyLine "test" "-- id:2efcf3a3-1f17-4f3a-8e6a-ea0fe2bac197")
        `shouldBe` (Right $ fromJust $ Data.UUID.fromText "2efcf3a3-1f17-4f3a-8e6a-ea0fe2bac197")

      (parse Notes.Parser.uuidBodyLine "test" "-- id:2efcf3a3-1f17-4f3a-8e6a-ea0fe2bac197\n")
        `shouldBe` (Right $ fromJust $ Data.UUID.fromText "2efcf3a3-1f17-4f3a-8e6a-ea0fe2bac197")

    it "nonNoteLine" $
      parse Notes.Parser.nonNoteLine "test" "this is not a note"
        `shouldBe` (Right "this is not a note")

    it "nonNoteParser" $
      parse (many Notes.Parser.nonNoteParser) "test" "this is not a note\nthis is another line of non note"
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

      parse
        ( many $
            (fmap Notes.Parser.NonNoteContent Notes.Parser.nonNoteParser)
              <|> Notes.Parser.blanklineParser
        )
        "test"
        "this is not a note\n\n"
        `shouldBe` ( Right
                       [ Notes.Parser.NonNoteContent (Notes.Parser.NonNote "this is not a note"),
                         Notes.Parser.Blankline
                       ]
                   )

    it "noteBodyParser" $ do
      (parse Notes.Parser.noteBodyParser "test" "-- this is a comment\n")
        `shouldBe` (Right $ Notes.Parser.NoteBody ["this is a comment"])

    it "noteBodyParser" $
      (parse Notes.Parser.noteBodyParser "test" "-- this is a comment\n-- this is another comment")
        `shouldBe` ( Right $
                       Notes.Parser.NoteBody
                         [ "this is a comment",
                           "this is another comment"
                         ]
                   )

    it "noteTitleParser" $
      (parse Notes.Parser.noteTitleParser "test" "-- # Note [This is a title]")
        `shouldBe` (Right $ Notes.Parser.NoteTitle "This is a title")

    it "noteBodyIdParser" $
      (parse Notes.Parser.noteIdParser "test" "-- id:2efcf3a3-1f17-4f3a-8e6a-ea0fe2bac197")
        `shouldBe` (Right $ Notes.Parser.NoteId $ fromJust $ Data.UUID.fromText "2efcf3a3-1f17-4f3a-8e6a-ea0fe2bac197")

    it "uuidBodyLine & noteBodyLine" $
      parse
        (many $ Notes.Parser.noteBodyLine <|> (fmap Data.UUID.toText Notes.Parser.uuidBodyLine))
        "test"
        "-- this is a comment\n-- id:2efcf3a3-1f17-4f3a-8e6a-ea0fe2bac197\n"
        `shouldBe` (Right ["this is a comment", "2efcf3a3-1f17-4f3a-8e6a-ea0fe2bac197"])

    it "nonNoteLine & noteBodyLine" $ do
      parse
        (many $ Notes.Parser.nonNoteLine <|> Notes.Parser.noteBodyLine)
        "test"
        "this is not a comment\n-- this is a comment"
        `shouldBe` (Right ["this is not a comment", "this is a comment"])

      parse
        ( many $
            Notes.Parser.nonNoteLine
              <|> Notes.Parser.noteBodyLine
              <|> (pure "blankline" <* Text.Megaparsec.Char.eol)
        )
        "test"
        "this is not a comment\n\n-- this is a comment\n\n"
        `shouldBe` (Right ["this is not a comment", "blankline", "this is a comment", "blankline"])

    it "noteBodyLine & blankline" $
      parse
        (many $ Notes.Parser.noteBodyLine <|> (pure "blankline" <* Text.Megaparsec.Char.eol))
        "test"
        "-- this is the body\n\n"
        `shouldBe` (Right ["this is the body", "blankline"])

    it "noteTitle & blankline" $
      parse
        (many $ Notes.Parser.noteTitle <|> (pure "blankline" <* Text.Megaparsec.Char.eol))
        "test"
        "-- # Note [This is a title]\n\n"
        `shouldBe` (Right ["This is a title", "blankline"])

test_testTree :: IO TestTree
test_testTree =
  testSpec "Parser Spec" $ do
    smolParser
    parser
