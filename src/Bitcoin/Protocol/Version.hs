{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Protocol.Version where

import Data.Binary (Binary(..))
import GHC.Generics (Generic)

import Bitcoin.Types (Message(..), Int32le, Word64le, Int64le)
import Bitcoin.Protocol.NetAddr (NetAddr)
import Bitcoin.Protocol.VarStr (VarStr)

data Version = Version
  { version     :: Int32le
  , services    :: Word64le
  , timestamp   :: Int64le
  , addrRecv    :: NetAddr
  , addrFrom    :: NetAddr
  , nonce       :: Word64le
  , userAgent   :: VarStr
  , startHeight :: Int32le
  , relay       :: Bool
  } deriving (Show, Generic)

instance Binary Version

instance Message Version where
  commandName = "version"

