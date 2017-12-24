{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Protocol.GetData where

import Data.Binary (Binary(..))
import GHC.Generics (Generic)

import Bitcoin.Protocol.Inv (Inventory)
import Bitcoin.Protocol.VarList (VarList)
import Bitcoin.Types (Message(..))

-- | データを要求する
newtype GetData = GetData (VarList Inventory)
  deriving (Show, Generic)

instance Binary GetData

instance Message GetData where
  commandName = "getdata"

