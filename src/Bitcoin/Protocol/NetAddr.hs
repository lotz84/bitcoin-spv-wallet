{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Bitcoin.Protocol.NetAddr where

import Data.Word (Word16)

import Data.Binary (Binary(..))
import GHC.Generics (Generic)

import Bitcoin.Types (Chars, Word64le)

data NetAddr = NetAddr
  { services :: Word64le
  , ip       :: Chars 16
  , port     :: Word16
  } deriving (Show, Generic)

instance Binary NetAddr

