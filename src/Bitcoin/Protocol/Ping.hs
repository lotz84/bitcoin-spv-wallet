{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Protocol.Ping where

import Data.Binary (Binary(..))
import GHC.Generics (Generic)

import Bitcoin.Types (Message(..), Int64le)

newtype Ping = Ping { nonce :: Int64le }
  deriving (Show, Generic)

instance Binary Ping

instance Message Ping where
  commandName = "ping"

