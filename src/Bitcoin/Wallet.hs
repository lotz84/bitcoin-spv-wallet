module Bitcoin.Wallet where

import Control.Monad (guard)
import Crypto.Secp256k1
import Crypto.Random (getRandomBytes)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base58 (bitcoinAlphabet, encodeBase58, decodeBase58)

import Bitcoin.Hash (hash160, hash256)

-- | Secp256k1の秘密鍵を生成する
genSecKey :: IO SecKey
genSecKey = do
  bs <- getRandomBytes 32
  case secKey bs of
    Just sk -> pure sk
    Nothing -> genSecKey


-- | 秘密鍵をWIFに変換する
encodeWIF :: SecKey -> ByteString
encodeWIF sk =
  let xs = BS.concat [BS.singleton 0xEF, getSecKey sk]
      checksum = BS.take 4 . hash256 $ xs
   in encodeBase58 bitcoinAlphabet $ BS.concat [xs, checksum]

-- | WIFから通常の秘密鍵に変換する
decodeWIF :: ByteString -> Maybe SecKey
decodeWIF wif = do
  xs <- decodeBase58 bitcoinAlphabet wif
  let l         = BS.length xs
      ys        = BS.take (l - 4) xs
      checksum  = BS.drop (l - 4) xs
      checksum' = BS.take 4 . hash256 $ ys
  guard  $ checksum == checksum'
  secKey $ BS.drop 1 ys

-- | 公開鍵をBitcoinアドレスに変換する
encodeBitcoinAddress :: PubKey -> ByteString
encodeBitcoinAddress pubKey =
  let xs = BS.concat [BS.singleton 0x6F, hash160 $ exportPubKey False pubKey]
      checksum = BS.take 4 . hash256 $ xs
   in encodeBase58 bitcoinAlphabet $ BS.concat [xs, checksum]


-- | addressからHash160後の公開鍵を取得する
decodeAddress :: ByteString -> Maybe ByteString
decodeAddress address =
  case decodeBase58 bitcoinAlphabet address of
    Nothing -> Nothing
    Just bs ->
      let l         = BS.length bs
          payload   = BS.take (l - 4) bs
          checksum  = BS.drop (l - 4) bs
          checksum' = BS.take 4 . hash256 $ payload
       in if checksum == checksum'
             then Just $ BS.drop 1 payload
             else Nothing

