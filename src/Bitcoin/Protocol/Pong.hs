{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Protocol.Pong where

import Data.Binary (Binary(..))
import GHC.Generics (Generic)

import Bitcoin.Types (Message(..), Int64le)

newtype Pong = Pong { nonce :: Int64le }
  deriving (Show, Generic)

instance Binary Pong

instance Message Pong where
  commandName = "pong"

