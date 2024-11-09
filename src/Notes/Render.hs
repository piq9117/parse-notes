{-# LANGUAGE OverloadedStrings #-}

module Notes.Render
  ( renderFileContentsToFile,
    render,
    prettyPrint,
  )
where

import Notes.Parser qualified
import Notes.Tracing
  ( ActiveSpan,
    MonadTracer,
    childOf,
    spanOpts,
    traced_,
  )
import Text.PrettyPrint qualified
import Text.PrettyPrint.HughesPJClass (Pretty (..))

render :: Text.PrettyPrint.Doc -> String
render = Text.PrettyPrint.render

prettyPrint :: (Pretty a) => a -> Text.PrettyPrint.Doc
prettyPrint = pPrint

renderFileContentsToFile ::
  (MonadTracer r m) =>
  ActiveSpan ->
  FilePath ->
  [Notes.Parser.FileContent] ->
  m ()
renderFileContentsToFile span filepath contents =
  traced_ (spanOpts "render-file-contents-to-file" $ childOf span) $ \_span -> do
    let renderContents =
          Text.PrettyPrint.render $
            Text.PrettyPrint.cat $
              fmap pPrint contents

    writeFileBS filepath (encodeUtf8 renderContents)
