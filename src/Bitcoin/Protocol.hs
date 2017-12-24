{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Bitcoin.Protocol where

import Control.Applicative ((<|>))
import Control.Exception (bracket)
import Data.Binary (Binary(..), encode, decode)
import Data.ByteArray.Encoding (convertFromBase, Base(Base16))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Char (chr)
import Data.Function (fix)
import Data.List (isPrefixOf)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Vector.Sized as Vector
import Network.BSD (getHostByName, hostAddress)
import Network.Socket (Socket, socket, Family(AF_INET), SocketType(Stream), defaultProtocol, SockAddr(SockAddrInet), PortNumber, connect, close)
import Network.Socket.ByteString (send, recv)
import System.Random.MWC (withSystemRandom, asGenIO, uniform)

import Bitcoin.Hash (hash256)
import Bitcoin.Protocol.NetAddr (NetAddr(NetAddr))
import Bitcoin.Protocol.VarStr (VarStr(VarStr))
import Bitcoin.Protocol.VarInt (VarInt)
import Bitcoin.Protocol.MessageHeader (MessageHeader(MessageHeader))
import qualified Bitcoin.Protocol.MessageHeader as MessageHeader
import Bitcoin.Protocol.Version (Version(..))
import Bitcoin.Protocol.Verack (Verack(..))
import Bitcoin.Types (Message(..), Word64le(..), Chars(..), toChars, toByteString)

readHex :: ByteString -> ByteString
readHex = either error id . convertFromBase Base16

recvAll :: Socket -> Int -> IO ByteString
recvAll sock size = go [] sock size
  where
    go recieved _ 0 = pure . BS.concat $ reverse recieved
    go recieved sock size = do
      bs <- recv sock size
      go (bs:recieved) sock (size - BS.length bs)

recvMessage :: Binary msg => Socket -> Int -> IO msg
recvMessage sock size = decode . BL.fromStrict <$> recvAll sock size

recvMessageHeader :: Socket -> IO (ByteString, Int)
recvMessageHeader sock = do
  mh <- recvMessage sock 24 :: IO MessageHeader
  let name = toByteString $ MessageHeader.commandName mh
      size = fromIntegral $ MessageHeader.payloadSize mh
  putStrLn $ "Recv: " ++ BC.unpack name ++ " " ++ show size
  pure (name, size)

sendMessage :: forall msg. (Binary msg, Message msg) => Socket -> msg -> IO ()
sendMessage sock msg = do
  let header = createMessageHeader msg
      payload = encode msg
      size = BL.length payload
  send sock (BL.toStrict $ BL.concat [encode header, payload])
  putStrLn $ "Send: " ++ (BC.unpack $ commandName @msg) ++ " " ++ show size

createMessageHeader :: forall msg. (Binary msg, Message msg) => msg -> MessageHeader
createMessageHeader message =
  let payload = BL.toStrict $ encode message
   in MessageHeader
        0x0709110B -- Testnet magic value
        (toChars $ commandName @msg)
        (fromIntegral $ BS.length payload)
        (toChars $ hash256 payload)

withBitcoinConnection :: ((Socket, Version) -> IO a) -> IO a
withBitcoinConnection between = bracket first (close . fst) between
  where
    first = do
      host <- hostAddress <$> getHostByName "testnet-seed.bitcoin.jonasschnelli.ch"
      sock <- socket AF_INET Stream defaultProtocol
      connect sock (SockAddrInet 18333 host)

      -- | versionを送る
      unixtime <- getPOSIXTime
      let ip = readHex $ BS.concat ["00000000", "00000000", "0000FFFF", "7F000001"]
          addr = NetAddr 1 (toChars ip) 8333
          userAgent = VarStr 0 BS.empty
          version = Version 70015 1 (round unixtime) addr addr 0 userAgent 0 False
      sendMessage sock version

      -- | versionを受け取ってverackを送り返す
      version <- handshake sock Nothing False
      pure (sock, version)
    handshake sock version verack = do
      case (version, verack) of
        (Just version, True) -> pure version
        _                    -> do
          (name, size) <- recvMessageHeader sock
          (version', verack') <- dispatch sock name size
          handshake sock (version <|> version') (verack || verack')
    dispatch sock name size
      | "version" `BS.isPrefixOf` name = do
          version <- recvMessage sock size :: IO Version
          sendMessage sock Verack
          pure (Just version, False)
      | "verack" `BS.isPrefixOf` name = pure (Nothing, True)
      | otherwise = do
          recvMessage sock size :: IO ByteString
          pure (Nothing, False)

