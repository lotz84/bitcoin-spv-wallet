{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Protocol.Inv where

import Data.Binary (Binary(..))
import Data.Binary.Put (putWord32le)
import Data.Binary.Get (getWord32le)
import GHC.Generics (Generic)

import Bitcoin.Types (Message(..), Chars)
import Bitcoin.Protocol.VarList (VarList(VarList))

-- | Inventoryに含まれるデータの種類
data InvType = ERROR              -- エラー
             | MSG_TX             -- トランザクション
             | MSG_BLOCK          -- ブロック
             | MSG_FILTERED_BLOCK -- マークルブロック
             | MSG_CMPCT_BLOCK    -- コンパクトブロック
             deriving (Show, Eq)

instance Binary InvType where
  put ERROR              = putWord32le 0
  put MSG_TX             = putWord32le 1
  put MSG_BLOCK          = putWord32le 2
  put MSG_FILTERED_BLOCK = putWord32le 3
  put MSG_CMPCT_BLOCK    = putWord32le 4

  get = do
    _type <- getWord32le
    pure $ case _type of
      0 -> ERROR
      1 -> MSG_TX
      2 -> MSG_BLOCK
      3 -> MSG_FILTERED_BLOCK
      4 -> MSG_CMPCT_BLOCK

-- | ハッシュとそれが示すデータの種類
data Inventory = Inventory
  { invType :: InvType
  , hash    :: Chars 32
  } deriving (Show, Generic)

instance Binary Inventory

-- | Inventoryのリスト
newtype Inv = Inv (VarList Inventory)
  deriving (Show, Generic)

instance Binary Inv

instance Message Inv where
  commandName = "inv"

