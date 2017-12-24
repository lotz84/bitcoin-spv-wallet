{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Protocol.Filterload where

import Prelude hiding (filter)

import Data.Array (Array, listArray, elems, accum)
import Data.Binary (Binary(..))
import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import Data.Word (Word8, Word32)
import GHC.Generics (Generic)
import GHC.Exts (fromList)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Hash.Murmur (murmur3)
import System.Random.MWC (withSystemRandom, asGenIO, uniform)

import Bitcoin.Protocol.VarList (VarList)
import qualified Bitcoin.Protocol.VarList as VarList
import Bitcoin.Types (Message(..), Word32le(..))

-- | Bloom filterを設定する
data Filterload = Filterload
  { filter       :: VarList Word8 -- Bloom filterのビット配列
  , nHashFuncs   :: Word32le      -- ハッシュ関数の個数
  , nTweak       :: Word32le      -- ハッシュ関数を生成する乱数
  , nFlags       :: Word8         -- Bloom filterの更新方法
  } deriving (Show, Generic)

instance Binary Filterload

instance Message Filterload where
  commandName = "filterload"

-- | ビット配列の長さ、ハッシュ関数の個数、含める文字列からFilterloadを作る
filterload :: Word32 -> Word32 -> [ByteString] -> IO Filterload
filterload size nHashFuncs queries = do
  nTweak <- withSystemRandom $ asGenIO uniform
  let seeds = map (\n -> n * 0xFBA4C795 + nTweak) [0 .. nHashFuncs - 1]
      bits = do
        seed  <- seeds
        query <- queries
        pure $ murmur3 seed query `mod` size
      bitArray = accum
        (\e a -> e .|. shiftL 1 (fromIntegral $ 7 .&. a))
        (listArray (0, size `div` 8 - 1) (repeat 0))
        (map (\i -> (shiftR i 3, i)) bits)
  pure $ Filterload (fromList $ elems bitArray) (Word32le nHashFuncs) (Word32le nTweak) 1

