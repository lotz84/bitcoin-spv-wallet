{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Balance where

import Control.Monad (forever, when, guard)
import Control.Monad.STM (atomically)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TVar (newTVarIO, readTVarIO, modifyTVar)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Binary (encode)
import Data.List (maximumBy)
import Data.Function (fix, on)
import Data.Maybe (fromJust, isNothing, isJust)
import qualified Data.Map as Map
import GHC.Exts (fromList)

import Crypto.Secp256k1
import Bitcoin.Hash (hash160)
import Bitcoin.Protocol (recvMessage, recvMessageHeader, sendMessage, withBitcoinConnection, readHex)
import Bitcoin.Protocol.Ping (Ping(..))
import qualified Bitcoin.Protocol.Ping as Ping
import Bitcoin.Protocol.Pong (Pong(..))
import Bitcoin.Protocol.Tx (Tx(..), findP2PkhIndex, OutPoint(..), hasOutPoint, valueAt)
import qualified Bitcoin.Protocol.Tx as Tx
import Bitcoin.Protocol.Inv (Inv(..), Inventory(..))
import qualified Bitcoin.Protocol.Inv as Inv
import Bitcoin.Protocol.GetData (GetData(..))
import Bitcoin.Protocol.Reject (Reject(..))
import Bitcoin.Protocol.Filterload (filterload)
import Bitcoin.Protocol.GetBlocks (GetBlocks(GetBlocks), zeroHash)
import Bitcoin.Protocol.Merkleblock (Merkleblock)
import qualified Bitcoin.Protocol.Merkleblock as Merkleblock
import Bitcoin.Protocol.VarList (VarList(VarList))
import qualified Bitcoin.Protocol.VarList as VarList
import qualified Bitcoin.Protocol.Version as Version
import Bitcoin.Types (toChars)
import Bitcoin.Wallet (decodeAddress)

import Wallet (getWalletSecretKey)

showBalance :: IO ()
showBalance = withBitcoinConnection $ \(sock, version) -> do
  secKey <- fromJust <$> getWalletSecretKey
  let fromPubKey       = exportPubKey False $ derivePubKey secKey
      fromPubKeyHashed = hash160 fromPubKey

      -- 簡便のため高さ1255000のブロックから取得する
      startBlockHash = toChars . BS.reverse $ readHex "000000006ba819bcaa50e6fb2fb89deb4ff894068cbaf4b4b9b7e89f24ccdc2f"
      leftBlocks = fromIntegral $ Version.startHeight version - 1255000

  -- Bloom filterの設定
  sendMessage sock =<< filterload 1024 10 [fromPubKeyHashed]

  -- スレッドを別に立てて受け取ったブロックとトランザクションをMapに記録していく
  blockMapTVar <- newTVarIO Map.empty
  txMapTVar    <- newTVarIO Map.empty
  forkIO . forever $ dispatch sock (blockMapTVar, txMapTVar) =<< recvMessageHeader sock

  -- 目標の個数まで500個ずつブロックを取得していく
  let loop n = do
        threadDelay 100000 -- sleep 0.1s
        blockMap <- readTVarIO blockMapTVar
        if Map.size blockMap >= leftBlocks
           then pure blockMap
           else
             if Map.size blockMap >= n
                then do
                  let latestBlockHash = Merkleblock.blockHash . maximumBy (compare `on` Merkleblock.timestamp) $ Map.elems blockMap
                  sendMessage sock $ GetBlocks 70015 [latestBlockHash] zeroHash
                  loop (n + 500)
                else loop n
  sendMessage sock $ GetBlocks 70015 [startBlockHash] zeroHash
  blockMap <- loop 500

  print $ Map.size blockMap


  -- マークルブロックからトランザクションを取り出しまだデータを取得していないトランザクションを列挙する
  txMap <- readTVarIO txMapTVar
  let txs = concat . fromJust . sequence . map Merkleblock.validate $ Map.elems blockMap
      unknowns = filter (isNothing . flip Map.lookup txMap) txs

  -- データを取得していないトランザクションのデータを取得する
  sendMessage sock (GetData . fromList $ map (Inventory Inv.MSG_TX) unknowns)
  txMap <- fix $ \loop -> do
    threadDelay 100000 -- sleep 0.1s
    txMap <- readTVarIO txMapTVar
    if and $ map (isJust . flip Map.lookup txMap) unknowns
       then pure txMap
       else loop

  -- UTXOを計算する
  let txs = Map.elems txMap
      myTxs = do
        tx <- txs
        case findP2PkhIndex fromPubKeyHashed tx of
          Just index -> [(tx, index)]
          Nothing    -> []
      utxo = do
        (tx, index) <- myTxs
        let op = OutPoint (Tx.txId tx) index
        guard $ and (map (not . (hasOutPoint op)) txs)
        pure (tx, index)
      balance = sum $ map (\(tx, index) -> valueAt index tx) utxo
  putStrLn $ "残高: " ++ show balance

    where
      dispatch sock (blockMapTVar, txMapTVar) (name, size)
        | "inv" `BS.isPrefixOf` name = do
            (Inv inv) <- recvMessage sock size :: IO Inv
            -- MSG_BLOCKを受け取ったらMSG_FILTERED_BLOCKに変換する
            let toFilteredBlock x = if Inv.invType x == Inv.MSG_BLOCK then Inv.Inventory Inv.MSG_FILTERED_BLOCK (Inv.hash x) else x
                inv' = map toFilteredBlock $ VarList.elems inv
                hashes = map Inv.hash inv'
            sendMessage sock (GetData $ fromList inv')
        | "merkleblock" `BS.isPrefixOf` name = do
            block <- recvMessage sock size :: IO Merkleblock
            atomically . modifyTVar blockMapTVar $ Map.insert (Merkleblock.blockHash block) block
        | "tx" `BS.isPrefixOf` name = do
            tx <- recvMessage sock size :: IO Tx
            atomically . modifyTVar txMapTVar $ Map.insert (Tx.txId tx) tx
        | "reject" `BS.isPrefixOf` name = print =<< (recvMessage sock size :: IO Reject)
        | otherwise =
            if size > 0
               then () <$ (recvMessage sock size :: IO ByteString)
               else pure ()

