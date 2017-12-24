{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Protocol.Reject where

import Data.Binary (Binary(..))
import Data.Binary.Put (putLazyByteString)
import Data.Binary.Get (getRemainingLazyByteString)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)

import Bitcoin.Types (Message(..))
import Bitcoin.Protocol.VarStr (VarStr)

-- | 受け取ったメッセージをRejectした時のメッセージ
data Reject = Reject
  { message :: VarStr -- Rejectの種類
  , ccode   :: Word8  -- Reject code
  , reason  :: VarStr -- Reject 理由
  , _data   :: BL.ByteString -- TX IDとかブロックヘッダが入る
  } deriving Show

instance Binary Reject where
  put x = do
    put               $ message x
    put               $ ccode   x
    put               $ reason  x
    putLazyByteString $ _data   x

  get = Reject
    <$> get
    <*> get
    <*> get
    <*> getRemainingLazyByteString

instance Message Reject where
  commandName = "reject"

