{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Bitcoin.Protocol.VarList where

import Prelude hiding (length)
import qualified Prelude as P

import Control.Monad (replicateM)
import GHC.Exts (IsList(..))

import Data.Binary (Binary(..))

import Bitcoin.Protocol.VarInt (VarInt)

data VarList a = VarList
  { length :: VarInt
  , elems  :: [a]
  } deriving Show

instance IsList (VarList a) where
  type Item (VarList a) = a
  fromList xs = VarList (fromIntegral $ P.length xs) xs
  toList = elems

instance Binary a => Binary (VarList a) where
  put x = do
    put       $ length x
    mapM_ put $ elems  x
  get = do
    size <- get
    es   <- replicateM (fromIntegral size) get
    pure (VarList size es)

