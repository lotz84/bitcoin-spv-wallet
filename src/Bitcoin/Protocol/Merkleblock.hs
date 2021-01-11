{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Protocol.Merkleblock where

import Control.Monad.State (State)
import qualified Control.Monad.State as State
import Data.Binary (Binary(..), encode)
import Data.Binary.Put (putInt32le, putWord32le, putByteString)
import Data.Binary.Get (getInt32le, getWord32le, getByteString)
import Data.Bits (shiftL, shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)
import GHC.Generics (Generic)

import Bitcoin.Protocol.VarList (VarList)
import qualified Bitcoin.Protocol.VarList as VarList
import Bitcoin.Hash (hash256)
import Bitcoin.Types (Message(..), Chars, toChars, toByteString, Int32le, Word32le)

-- | Word8で表現されているFlagsをBoolのリストに変換する
unpack :: Word8 -> [Bool]
unpack w = map (==1) $ map (\b -> (w `div` 2^b) `rem` 2) [0..7]

-- | 特定のノード数を持つマークル木の特定の高さの幅を計算する
calcTreeWidth :: Word32le -> Int -> Int
calcTreeWidth nTransactions height = (fromIntegral nTransactions + (1 `shiftL` height) - 1) `shiftR` height

-- | マークルパスを検証しマッチしたトランザクションを取り出す
validate :: Merkleblock -> Maybe [Chars 32]
validate block =
  let hs = VarList.elems $ hashes block
      fs = concatMap unpack . VarList.elems $ flags block
      ctw = calcTreeWidth (totalTransactions block)
      height = ceiling $ logBase 2 (fromIntegral $ totalTransactions block)
      (root, (_, _, ms)) = State.runState (accum ctw height 0) (hs, fs, [])
   in if root == toByteString (merkleRoot block) -- マークルルートが一致することを確認する
         then Just ms
         else Nothing
  where
    accum :: (Int -> Int) -> Int -> Int -> State ([Chars 32], [Bool], [Chars 32]) ByteString
    accum ctw height pos = do
      -- (h:hs, f:fs, ms) <- State.get
      
      -------- This is a dirty and quick patch, avoiding MonadFailDesugaring ---------
      (hl, fl, ms) <- State.get
      let   monadfail l   = (case l of 
                (x:xs)              -> (x:xs)
                _                   -> error "patern match failure" )
      let   (h:hs)        = monadfail hl
            (f:fs)        = monadfail fl
      -----------------------------------------------------------------------------
      
      case (f, height == 0) of
        -- 表の手順に従って探索する
        (False, _)     -> State.put (hs, fs,   ms) >> pure (toByteString h)
        (True, True)   -> State.put (hs, fs, h:ms) >> pure (toByteString h)
        (True, False)  -> do
          State.put (h:hs, fs, ms)
          left <- accum ctw (height - 1) (2 * pos)
          -- 幅が奇数個しか無かったら端のノードを複製してハッシュを計算する
          if ctw (height - 1) > 2 * pos + 1
             then do
               right <- accum ctw (height - 1) (2 * pos + 1)
               pure $ hash256 (BS.concat [left, right])
             else pure $ hash256 (BS.concat [left, left])

-- | マークルブロック
data Merkleblock = Merkleblock
  { version           :: Int32le            -- バージョン
  , prevBlock         :: Chars 32           -- 前のブロックのハッシュ値
  , merkleRoot        :: Chars 32           -- マークルルート
  , timestamp         :: Word32le           -- UNIXTIMESTAMP
  , bits              :: Word32le           -- 難易度
  , nonce             :: Word32le           -- ノンス
  , totalTransactions :: Word32le           -- ブロックに含まれるトランザクションの総数
  , hashes            :: VarList (Chars 32) -- マークルパスを構築するためのハッシュ列
  , flags             :: VarList Word8      -- ークルパスを構築するためのフラグ列
  } deriving (Show, Generic)

instance Binary Merkleblock

instance Message Merkleblock where
  commandName = "merkleblock"

-- | ブロックハッシュを計算する
blockHash :: Merkleblock -> Chars 32
blockHash = toChars . hash256 . BS.take 80 . BL.toStrict . encode

