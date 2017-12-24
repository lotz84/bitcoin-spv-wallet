{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Send where

import Control.Monad (forever, when)

import Crypto.Secp256k1
import Data.Binary (encode, decode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (chr)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import Data.Word (Word32)
import qualified Data.Vector.Sized as Vector

import Bitcoin.Hash (hash160, hash256)
import Bitcoin.Protocol (readHex, recvMessage, recvMessageHeader, sendMessage, createMessageHeader, withBitcoinConnection)
import Bitcoin.Protocol.VarInt (VarInt)
import qualified Bitcoin.Protocol.VarStr as VarStr
import Bitcoin.Protocol.MessageHeader (MessageHeader(..))
import Bitcoin.Protocol.Ping (Ping(..))
import qualified Bitcoin.Protocol.Ping as Ping
import Bitcoin.Protocol.Pong (Pong(..))
import Bitcoin.Protocol.Inv (Inv(..), Inventory(..), InvType(..))
import qualified Bitcoin.Protocol.Inv as Inv
import Bitcoin.Protocol.Mempool (Mempool(..))
import Bitcoin.Protocol.GetData (GetData(..))
import qualified Bitcoin.Protocol.GetData as GetData
import Bitcoin.Protocol.GetBlocks (GetBlocks(..), zeroHash)
import Bitcoin.Protocol.Reject (Reject(..))
import Bitcoin.Protocol.Tx (Tx(..), TxOut(..), TxIn(..), OutPoint(..))
import qualified Bitcoin.Protocol.Tx as Tx
import Bitcoin.Protocol.Block (Block(..))
import qualified Bitcoin.Protocol.VarList as VarList
import Bitcoin.Script (op_pushdata, op_dup, op_equal, op_equalverify, op_hash160, op_checksig)
import Bitcoin.Types (toChars, Word32le)
import Bitcoin.Wallet (decodeAddress)

import Wallet (getWalletSecretKey)

sendBitcoin :: IO ()
sendBitcoin = withBitcoinConnection $ \(sock, version) -> do
  let -- TxInの作成
      outTxId  = toChars . BS.reverse $ readHex "35ba3ed9c42b1441d7f6da40f7bd9f81747b74e8b6f1e8ea040163fdf36d48f0"
      outPoint = OutPoint outTxId 0x00
      txIn xs  = TxIn outPoint xs 0xFFFFFFFF

  secKey <- fromJust <$> getWalletSecretKey

  let -- TxOutの作成
      balance = 200000000
      amount  =  20000000
      fee     =  10000000

      toAddress        = "2N8hwP1WmJrFF5QWABn38y63uYLhnJYJYTF"
      toPubKeyHashed   = fromJust $ decodeAddress toAddress
      fromPubKey       = exportPubKey False $ derivePubKey secKey
      fromPubKeyHashed = hash160 fromPubKey

      lockingScript1 = VarStr.fromByteString $ BS.concat [op_hash160, op_pushdata toPubKeyHashed,  op_equal]
      lockingScript2 = VarStr.fromByteString $ BS.concat [op_dup, op_hash160, op_pushdata fromPubKeyHashed, op_equalverify, op_checksig]

      txOut1 = TxOut amount                   lockingScript1
      txOut2 = TxOut (balance - amount - fee) lockingScript2

  let -- 署名の作成
      subscript    = VarStr.fromByteString $ readHex "76a914aa8e6d2c98a67feba1c8eaed8bf1c168fc3ff74588ac"
      _tx          = Tx 1 [txIn subscript] [txOut1, txOut2] 0x00
      hashType     = BS.singleton 0x01
      hashTypeCode = BS.pack [0x01, 0x00, 0x00, 0x00]
      sign         = exportSig . signMsg secKey . fromJust . msg . hash256 $ BS.concat [BL.toStrict $ encode _tx, hashTypeCode]
      signWithType = sign `BS.append` hashType


  let -- Txの作成
      unlockingScript = VarStr.fromByteString $ BS.concat [op_pushdata signWithType, op_pushdata fromPubKey]
      tx              = Tx 1 [txIn unlockingScript] [txOut1, txOut2] 0x00
      txId            = Tx.txId tx
      inv             = Inv [Inventory MSG_TX txId]

  sendMessage sock inv
  forever $ dispatch sock tx txId =<< recvMessageHeader sock
    where
      dispatch sock tx txId (name, size)
        | "getdata" `BS.isPrefixOf` name = do
            (GetData getdata) <- recvMessage sock size :: IO GetData
            let isTx inv = Inv.invType inv == MSG_TX && Inv.hash inv == txId
                inv = filter isTx $ VarList.elems getdata
            if null inv
               then pure ()
               else sendMessage sock tx >> pure ()
        | otherwise = () <$ (if size > 0 then recvMessage sock size :: IO ByteString else pure "")

