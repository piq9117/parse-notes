module Notes.Render where

import Notes.Parser qualified
import Text.PrettyPrint qualified
import Text.PrettyPrint.HughesPJClass (Pretty (..))

renderFileContents :: [Notes.Parser.FileContent] -> String
renderFileContents contents =
  Text.PrettyPrint.render $
    Text.PrettyPrint.cat $
      fmap pPrint contents
