{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Protocol.Mempool where

import Data.Binary (Binary)
import GHC.Generics (Generic)

import Bitcoin.Types (Message(..))

data Mempool = Mempool deriving (Show, Generic)

instance Binary Mempool

instance Message Mempool where
  commandName = "mempool"

