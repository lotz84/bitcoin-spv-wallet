{-# LANGUAGE DataKinds #-}

module Bitcoin.Protocol.VarStr where

import Prelude hiding (length)

import Data.Binary (Binary(..))
import Data.Binary.Put (putByteString)
import Data.Binary.Get (getByteString)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Bitcoin.Protocol.VarInt (VarInt)

fromByteString :: ByteString -> VarStr
fromByteString bs = VarStr (fromIntegral $ BS.length bs) bs

data VarStr = VarStr
  { length :: VarInt
  , string :: ByteString
  } deriving Show

instance Binary VarStr where
  put x = do
    put           $ length x
    putByteString $ string x
  get = do
    size <- get
    xs   <- getByteString (fromIntegral size)
    pure (VarStr size xs)

