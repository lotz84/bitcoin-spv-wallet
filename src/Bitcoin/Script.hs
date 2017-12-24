module Bitcoin.Script where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

op_pushdata :: ByteString -> ByteString
op_pushdata bs =
  let size = BS.singleton . fromIntegral $ BS.length bs
   in size `BS.append` bs

op_dup, op_equal, op_equalverify, op_hash160, op_checksig :: ByteString
op_dup         = BS.singleton 0x76
op_equal       = BS.singleton 0x87
op_equalverify = BS.singleton 0x88
op_hash160     = BS.singleton 0xA9
op_checksig    = BS.singleton 0xAC

