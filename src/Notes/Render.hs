module Notes.Render
  ( renderFileContents,
    render,
    prettyPrint,
  )
where

import Notes.Parser qualified
import Text.PrettyPrint qualified
import Text.PrettyPrint.HughesPJClass (Pretty (..))

render :: Text.PrettyPrint.Doc -> String
render = Text.PrettyPrint.render

prettyPrint :: (Pretty a) => a -> Text.PrettyPrint.Doc
prettyPrint = pPrint

renderFileContents :: [Notes.Parser.FileContent] -> String
renderFileContents contents =
  Text.PrettyPrint.render $
    Text.PrettyPrint.cat $
      fmap pPrint contents
