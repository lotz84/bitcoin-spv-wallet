module Bitcoin.Hash where

import Crypto.Hash
import qualified Crypto.Hash as Crypto
import Data.ByteArray (convert)
import Data.ByteString (ByteString)

sha256 :: ByteString -> ByteString
sha256 bs = convert (Crypto.hash bs :: Digest SHA256)

hash256 :: ByteString -> ByteString
hash256 = sha256 . sha256

ripemd160 :: ByteString -> ByteString
ripemd160 bs = convert (Crypto.hash bs :: Digest RIPEMD160)

hash160 :: ByteString -> ByteString
hash160 = ripemd160 . sha256

