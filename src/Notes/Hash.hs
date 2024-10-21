module Notes.Hash (hashContent) where

import Crypto.Hash.MD5 (hash)

hashContent :: Text -> Text
hashContent content = decodeUtf8 $ hash (encodeUtf8 content)
