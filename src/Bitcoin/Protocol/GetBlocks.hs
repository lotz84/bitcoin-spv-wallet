{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Protocol.GetBlocks where

import GHC.Generics (Generic)

import Data.Binary (Binary(..))
import qualified Data.Vector.Sized as Vector
import Data.Maybe (fromJust)

import Bitcoin.Protocol.VarList (VarList)
import Bitcoin.Types (Message(..), Chars(..), Int32le)

-- | ブロックの情報を要求する
data GetBlocks = GetBlocks
  { version            :: Int32le            -- プロトコルのバージョン
  , blockLocatorHashes :: VarList (Chars 32) -- 取得を開始するブロックのハッシュ
  , hashStop           :: Chars 32           -- 取得を終わるブロックのハッシュ
  } deriving (Show, Generic)

instance Binary GetBlocks

instance Message GetBlocks where
  commandName = "getblocks"

-- | 全てのブロックを取得する時に指定するハッシュ
zeroHash :: Chars 32
zeroHash = Chars . fromJust . Vector.fromList $ replicate 32 0

