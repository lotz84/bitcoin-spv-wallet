{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Bitcoin.Types where

import Control.Monad (replicateM)
import Data.Int (Int32, Int64)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy(..))
import Data.Word (Word8, Word32, Word64)
import GHC.TypeLits (KnownNat, natVal)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Data.Binary (Binary(..), Put(..), Get(..))
import Data.Binary.Put (putWord32le, putWord64le, putInt32le, putInt64le)
import Data.Binary.Get (getWord32le, getWord64le, getInt32le, getInt64le)
import Data.Vector.Sized (Vector)
import qualified Data.Vector.Sized as Vector

newtype Word32le = Word32le Word32
  deriving (Show, Eq, Ord, Enum, Bounded, Num, Real, Integral)

instance Binary Word32le where
  put (Word32le x) = putWord32le x
  get = Word32le <$> getWord32le

newtype Word64le = Word64le Word64
  deriving (Show, Eq, Ord, Enum, Bounded, Num, Real, Integral)

instance Binary Word64le where
  put (Word64le x) = putWord64le x
  get = Word64le <$> getWord64le

newtype Int32le = Int32le Int32
  deriving (Show, Eq, Ord, Enum, Bounded, Num, Real, Integral)

instance Binary Int32le where
  put (Int32le x) = putInt32le x
  get = Int32le <$> getInt32le

newtype Int64le = Int64le Int64
  deriving (Show, Eq, Ord, Enum, Bounded, Num, Real, Integral)

instance Binary Int64le where
  put (Int64le x) = putInt64le x
  get = Int64le <$> getInt64le

newtype Chars n = Chars (Vector n Word8)
  deriving (Show, Eq, Ord)

instance forall n. KnownNat n => Binary (Chars n) where
  put (Chars x) = mapM_ put $ Vector.toList x
  get = (Chars . fromJust . Vector.fromList) <$> (replicateM (fromInteger $ natVal (Proxy @n)) get)

toChars :: forall n. KnownNat n => ByteString -> Chars n
toChars bs = Chars . fromJust . Vector.fromList . take (fromInteger $ natVal (Proxy @n)) $ (BS.unpack bs ++ repeat 0x00)

toByteString :: Chars n -> ByteString
toByteString (Chars cs) = BS.pack $ Vector.toList cs

class Message a where
  commandName :: ByteString

