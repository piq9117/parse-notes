{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Notes.Render (test_testTree) where

import Notes.Parser qualified
import Notes.Render qualified
import Test.Hspec (Spec, describe, it, shouldBe, xit)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

renderSpec :: Spec
renderSpec =
  describe "render spec" $ do
    it "render NonNote" $ do
      Notes.Render.render (Notes.Render.prettyPrint $ Notes.Parser.NonNote "this is not a note")
        `shouldBe` "this is not a note\n"

    it "render NoteBody" $ do
      Notes.Render.render (Notes.Render.prettyPrint $ Notes.Parser.BodyContent "this is the note body content")
        `shouldBe` "-- this is the note body content\n"

      ( fmap
          (Notes.Render.render <<< Notes.Render.prettyPrint)
          [ Notes.Parser.BodyContent "first line of content",
            Notes.Parser.BodyContent "second line of content"
          ]
        )
        `shouldBe` [ "-- first line of content\n",
                     "-- second line of content\n"
                   ]

      Notes.Render.render (Notes.Render.prettyPrint $ Notes.Parser.NoteId "6548a81e-5225-4bc9-99ac-31e0f355f762")
        `shouldBe` "-- id:6548a81e-5225-4bc9-99ac-31e0f355f762\n"

    it "render NoteTitle" $ do
      Notes.Render.render (Notes.Render.prettyPrint $ Notes.Parser.NoteTitle "This is the title of the note")
        `shouldBe` "-- # Note [This is the title of the note]\n"

    it "render Note" $ do
      Notes.Render.render
        ( Notes.Render.prettyPrint $
            Notes.Parser.Note
              { Notes.Parser.title = Notes.Parser.NoteTitle "This is the title of the note",
                Notes.Parser.body =
                  [ Notes.Parser.BodyContent "first line of the body",
                    Notes.Parser.BodyContent "second line of the body"
                  ],
                Notes.Parser.id = Just (Notes.Parser.NoteId "6548a81e-5225-4bc9-99ac-31e0f355f762")
              }
        )
        `shouldBe` "-- # Note [This is the title of the note]\n-- first line of the body\n-- second line of the body\n-- id:6548a81e-5225-4bc9-99ac-31e0f355f762\n"

    it "renderFileContents" $ do
      Notes.Render.renderFileContents
        [ Notes.Parser.NoteContent
            ( Notes.Parser.Note
                { Notes.Parser.title = Notes.Parser.NoteTitle "This is the title of the note",
                  Notes.Parser.body =
                    [ Notes.Parser.BodyContent "First line of the body",
                      Notes.Parser.BodyContent "Second line of the body"
                    ],
                  Notes.Parser.id = Just (Notes.Parser.NoteId "2efcf3a3-1f17-4f3a-8e6a-ea0fe2bac197")
                }
            )
        ]
        `shouldBe` "-- # Note [This is the title of the note]\n-- First line of the body\n-- Second line of the body\n-- id:2efcf3a3-1f17-4f3a-8e6a-ea0fe2bac197\n"

test_testTree :: IO TestTree
test_testTree =
  testSpec "Render Spec" $ do
    renderSpec
