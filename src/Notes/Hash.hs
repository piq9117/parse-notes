module Notes.Hash (hashContent) where

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16 qualified

hashContent :: Text -> Text
hashContent content =
  decodeUtf8With lenientDecode $ Data.ByteString.Base16.encode $ hash (encodeUtf8 content)
