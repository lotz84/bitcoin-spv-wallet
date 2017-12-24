{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Protocol.Verack where

import Data.Binary (Binary(..))
import GHC.Generics (Generic)

import Bitcoin.Types (Message(..))

data Verack = Verack
  deriving (Show, Generic)

instance Binary Verack

instance Message Verack where
  commandName = "verack"
