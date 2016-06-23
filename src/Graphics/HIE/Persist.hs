module Graphics.HIE.Persist where

import qualified Data.ByteString as BS

import Graphics.HIE.Session

serializeHie :: HieValues -> BS.ByteString
serializeHie = undefined

deserializeHie :: BS.ByteString -> Maybe HieValues
deserializeHie = undefined
