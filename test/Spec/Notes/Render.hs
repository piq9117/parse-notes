{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Notes.Render (test_testTree) where

import Data.Maybe (fromJust)
import Data.UUID qualified
import Data.UUID.V4 qualified
import Notes.Parser qualified
import Notes.Render qualified
import Notes.Tracing (nullTracer, runTracer, spanOpts, traced_)
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
      Notes.Render.render (Notes.Render.prettyPrint $ Notes.Parser.NoteBody ["this is the note body content"])
        `shouldBe` "-- this is the note body content\n"

      ( (Notes.Render.render <<< Notes.Render.prettyPrint)
          ( Notes.Parser.NoteBody
              [ "first line of content",
                "second line of content"
              ]
          )
        )
        `shouldBe` "-- first line of content\n-- second line of content\n"

      Notes.Render.render (Notes.Render.prettyPrint $ Notes.Parser.NoteId $ fromJust $ Data.UUID.fromText "6548a81e-5225-4bc9-99ac-31e0f355f762")
        `shouldBe` "-- id:6548a81e-5225-4bc9-99ac-31e0f355f762"

    it "render NoteTitle" $ do
      Notes.Render.render (Notes.Render.prettyPrint $ Notes.Parser.NoteTitle "This is the title of the note")
        `shouldBe` "-- # Note [This is the title of the note]\n"

    it "render Note" $ do
      Notes.Render.render
        ( Notes.Render.prettyPrint $
            Notes.Parser.Note
              { Notes.Parser.title = Notes.Parser.NoteTitle "This is the title of the note",
                Notes.Parser.body =
                  Notes.Parser.NoteBody
                    [ "first line of the body",
                      "second line of the body"
                    ],
                Notes.Parser.id = Notes.Parser.NoteId (fromJust $ Data.UUID.fromText "6548a81e-5225-4bc9-99ac-31e0f355f762")
              }
        )
        `shouldBe` "-- # Note [This is the title of the note]\n-- first line of the body\n-- second line of the body\n-- id:6548a81e-5225-4bc9-99ac-31e0f355f762"

    it "renderFileContents" $ do
      Notes.Render.renderFileContents
        [ Notes.Parser.NoteContent
            ( Notes.Parser.Note
                { Notes.Parser.title = Notes.Parser.NoteTitle "This is the title of the note",
                  Notes.Parser.body =
                    Notes.Parser.NoteBody
                      [ "First line of the body",
                        "Second line of the body"
                      ],
                  Notes.Parser.id = Notes.Parser.NoteId (fromJust $ Data.UUID.fromText "2efcf3a3-1f17-4f3a-8e6a-ea0fe2bac197")
                }
            )
        ]
        `shouldBe` "-- # Note [This is the title of the note]\n-- First line of the body\n-- Second line of the body\n-- id:2efcf3a3-1f17-4f3a-8e6a-ea0fe2bac197"

      runTracer nullTracer $
        traced_ (spanOpts "parse-file-m-test" mempty) $ \span -> do
          result <-
            Notes.Parser.parseFileM
              span
              ( Notes.Parser.GenerateBodyId
                  { Notes.Parser.generate = \span -> liftIO Data.UUID.V4.nextRandom
                  }
              )
              "-- # Note [This is the title of the note]\n-- This is the body of the note from the test file.\n-- Second line of the body from the test file.\n-- id:c7017b1a-adba-43d8-9a59-37a202982932\n\ndata NotNote = NotNote"
          liftIO
            ( result
                `shouldBe` [ Notes.Parser.NoteContent
                               Notes.Parser.Note
                                 { Notes.Parser.title = Notes.Parser.NoteTitle "This is the title of the note",
                                   Notes.Parser.body =
                                     Notes.Parser.NoteBody
                                       [ "This is the body of the note from the test file.",
                                         "Second line of the body from the test file."
                                       ],
                                   Notes.Parser.id =
                                     Notes.Parser.NoteId
                                       (fromJust $ Data.UUID.fromText "c7017b1a-adba-43d8-9a59-37a202982932")
                                 },
                             Notes.Parser.Blankline,
                             Notes.Parser.NonNoteContent (Notes.Parser.NonNote "data NotNote = NotNote")
                           ]
            )

test_testTree :: IO TestTree
test_testTree =
  testSpec "Render Spec" $ do
    renderSpec
