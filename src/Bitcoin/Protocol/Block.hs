{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Protocol.Block where

import Data.Binary (Binary(..))
import GHC.Generics (Generic)

import Bitcoin.Protocol.VarList (VarList)
import Bitcoin.Protocol.Tx (Tx)
import Bitcoin.Types (Message(..), Chars, Int32le, Word32le)

data Block = Block
  { version    :: Int32le
  , prevBlock  :: Chars 32
  , merkleRoot :: Chars 32
  , timestamp  :: Word32le
  , bits       :: Word32le
  , nonce      :: Word32le
  , txns       :: VarList Tx
  } deriving (Show, Generic)

instance Binary Block

instance Message Block where
  commandName = "block"

