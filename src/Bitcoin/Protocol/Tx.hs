{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Protocol.Tx where

import Prelude hiding (sequence)
import Data.List (find)
import GHC.Generics (Generic)

import Data.Binary (Binary(..), encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Bitcoin.Hash (hash256)
import Bitcoin.Types (Message(..), Chars, toChars, Word32le, Int32le, Int64le)
import Bitcoin.Protocol.VarStr (VarStr)
import qualified Bitcoin.Protocol.VarStr as VarStr
import Bitcoin.Protocol.VarList (VarList)
import qualified Bitcoin.Protocol.VarList as VarList
import Bitcoin.Script (op_dup, op_hash160)

type TxId = Chars 32

data OutPoint = OutPoint
  { hash  :: TxId
  , index :: Word32le
  } deriving (Show, Eq, Generic)

instance Binary OutPoint

data TxIn = TxIn
  { previousOutput  :: OutPoint
  , signatureScript :: VarStr
  , sequence        :: Word32le
  } deriving (Show, Generic)

instance Binary TxIn

data TxOut = TxOut
  { value          :: Int64le
  , pkScript       :: VarStr
  } deriving (Show, Generic)

instance Binary TxOut

-- | 簡便のため witness は対応しません
data Tx = Tx
  { version  :: Int32le
  , txIn     :: VarList TxIn
  , txOut    :: VarList TxOut
  , lockTime :: Word32le
  } deriving (Show, Generic)

instance Binary Tx

instance Message Tx where
  commandName = "tx"

txId :: Tx -> TxId
txId = toChars . hash256 . BL.toStrict . encode

-- | 与えられた公開鍵をP2PKHのアウトプットに持つTxOutのインデックスを取得する
-- | 存在しなければNothingとなる
findP2PkhIndex :: ByteString -> Tx -> Maybe Word32le
findP2PkhIndex pkh tx =
  let scripts     = map (VarStr.string . pkScript) . VarList.elems $ txOut tx
      p2PkhHeader = BS.concat [op_dup, op_hash160]
      isP2Pkh     = BS.isPrefixOf p2PkhHeader
      getPkh      = BS.take 20 . BS.drop (BS.length p2PkhHeader + 1)
   in (fromIntegral . snd) <$> (find ((pkh ==) . getPkh . fst) . filter (isP2Pkh . fst) $ zip scripts [0..])

-- | トランザクションが与えられたOutPointを持つか調べる
hasOutPoint :: OutPoint -> Tx -> Bool
hasOutPoint op tx = elem op . map previousOutput . VarList.elems $ txIn tx

-- | 与えられたインデックスのTxOutのvalueを取得する
valueAt :: Word32le -> Tx -> Int64le
valueAt index tx = value $ (VarList.elems $ txOut tx) !! (fromIntegral index)

