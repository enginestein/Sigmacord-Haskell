{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Sigmacord.Internal.Gateway.EventLoop where

import Prelude hiding (log)

import Control.Monad (forever, void)
import Control.Monad.Random (getRandomR)
import Control.Concurrent.Async (race)
import Control.Concurrent.Chan
import Control.Concurrent (threadDelay, killThread, forkIO)
import Control.Exception.Safe (try, finally, SomeException)
import Data.IORef
import Data.Aeson (eitherDecode, encode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.Time (getCurrentTime)

import Wuss (runSecureClient)
import Network.Socket (HostName)
import Network.WebSockets (ConnectionException(..), Connection,
                           receiveData, sendTextData, sendClose)

import Sigmacord.Internal.Types
import Sigmacord.Internal.Rest.Prelude (apiVersion)



data GatewayHandle = GatewayHandle
  { 
    gatewayHandleEvents         :: Chan (Either GatewayException EventInternalParse),
    
    gatewayHandleUserSendables  :: Chan GatewaySendable,
    
    gatewayHandleLastStatus     :: IORef (Maybe UpdateStatusOpts),
    
    gatewayHandleLastSequenceId :: IORef Integer,
    
    gatewayHandleSessionId      :: IORef T.Text,
    
    
    
    
    
    gatewayHandleHostname       :: IORef HostName,
    
    gatewayHandleHeartbeatAckTimes    :: IORef UTCTime,
    
    gatewayHandleHeartbeatTimes       :: IORef (UTCTime, UTCTime)
  }


newtype GatewayException = GatewayExceptionIntent T.Text
  deriving (Show)



data LoopState = LoopStart
               | LoopClosed
               | LoopReconnect
  deriving Show


data SendablesData = SendablesData
  { sendableConnection :: Connection
  , librarySendables :: Chan GatewaySendableInternal
  , startsendingUsers :: IORef Bool
  , heartbeatInterval :: Integer
  }

















connectionLoop :: Auth -> GatewayIntent -> GatewayHandle -> Chan T.Text -> IO ()
connectionLoop auth intent gatewayHandle log = outerloop LoopStart
    where

    
    outerloop :: LoopState -> IO ()
    outerloop state = do
        gatewayHost <- readIORef (gatewayHandleHostname gatewayHandle)
        mfirst <- firstmessage state 
        case mfirst of
          Nothing -> pure () 

          Just message -> do
              nextstate <- try (startOneConnection gatewayHost message)  
              case nextstate :: Either SomeException LoopState of
                Left _ -> do t <- getRandomR (3,20)
                             threadDelay (t * (10^(6 :: Int)))
                             writeChan log "gateway - trying to reconnect after failure(s)"
                             outerloop LoopReconnect
                Right n -> outerloop n

    
    
    firstmessage :: LoopState -> IO (Maybe GatewaySendableInternal)
    firstmessage state =
      case state of
        LoopStart -> pure $ Just $ Identify auth intent (0, 1)
        LoopReconnect -> do seqId  <- readIORef (gatewayHandleLastSequenceId gatewayHandle)
                            seshId <- readIORef (gatewayHandleSessionId gatewayHandle)
                            if seshId == ""
                            then do writeChan log "gateway - WARNING seshID was not set by READY?"
                                    pure $ Just $ Identify auth intent (0, 1)
                            else pure $ Just $ Resume auth seshId seqId
        LoopClosed -> pure Nothing

    startOneConnection
      :: HostName
      
      
      
      -> GatewaySendableInternal
      
      -> IO LoopState
    startOneConnection gatewayAddr message = runSecureClient gatewayAddr 443 ("/?v=" <> T.unpack apiVersion <>"&encoding=json") $ \conn -> do
                        msg <- getPayload conn log
                        case msg of
                            Right (Hello interval) -> do
                                
                                internal <- newChan :: IO (Chan GatewaySendableInternal)
                                sendingUser <- newIORef False
                                let sending = SendablesData { sendableConnection = conn
                                                            , librarySendables = internal
                                                            , startsendingUsers = sendingUser
                                                            , heartbeatInterval = interval
                                                            }
                                
                                sendsId <- forkIO $ sendableLoop conn gatewayHandle sending log
                                heart <- forkIO $ heartbeat sending (gatewayHandleHeartbeatTimes gatewayHandle) (gatewayHandleLastSequenceId gatewayHandle)
                                writeChan internal message

                                
                                finally (runEventLoop gatewayHandle sending log)
                                        (killThread heart >> killThread sendsId)

                            _ -> do
                                writeChan log "gateway - WARNING could not connect. Expected hello"
                                sendClose conn ("expected hello" :: BL.ByteString)
                                void $ forever $ void (receiveData conn :: IO BL.ByteString)
                                
                                
                                
                                threadDelay (3 * (10^(6 :: Int)))
                                pure LoopStart



runEventLoop :: GatewayHandle -> SendablesData -> Chan T.Text -> IO LoopState
runEventLoop thehandle sendablesData log = do loop
  where
  eventChan :: Chan (Either GatewayException EventInternalParse)
  eventChan = gatewayHandleEvents thehandle

  
  loop = do
    eitherPayload <- getPayloadTimeout sendablesData log
    case eitherPayload :: Either ConnectionException GatewayReceivable of

      Right (Dispatch event sq) -> do 
                                      writeIORef (gatewayHandleLastSequenceId thehandle) sq
                                      writeChan eventChan (Right event) 
                                      case event of
                                        (InternalReady _ _ _ seshID resumeHost _ _) -> do
                                            writeIORef (gatewayHandleSessionId thehandle) seshID
                                            writeIORef (gatewayHandleHostname thehandle) resumeHost
                                        _ -> writeIORef (startsendingUsers sendablesData) True
                                      loop
      Right (Hello _interval) -> do writeChan log "eventloop - unexpected hello"
                                    loop
      Right (HeartbeatRequest sq) -> do writeIORef (gatewayHandleLastSequenceId thehandle) sq
                                        sendHeartbeat sendablesData (gatewayHandleHeartbeatTimes thehandle) sq
                                        loop
      Right (InvalidSession retry) -> pure $ if retry then LoopReconnect else LoopStart
      Right Reconnect        -> pure LoopReconnect
      Right HeartbeatAck     -> do
        currTime <- getCurrentTime
        _ <- atomicModifyIORef' (gatewayHandleHeartbeatAckTimes thehandle) (dupe . const currTime)
        loop
      Right (ParseError _)   -> loop  

      Left (CloseRequest code str) -> case code of
          
          
          
          1000 -> pure LoopReconnect
          1001 -> pure LoopReconnect
          4000 -> pure LoopReconnect
          4006 -> pure LoopStart
          4007 -> pure LoopStart
          4014 -> do writeChan eventChan (Left (GatewayExceptionIntent $
                           "Tried to declare an unauthorized GatewayIntent. " <>
                           "Use the Sigmacord app manager to authorize by following: " <>
                           "https://github.com/Sigmacord-haskell/Sigmacord-haskell/blob/master/docs/intents.md"))
                     pure LoopClosed
          _ -> do writeChan log ("gateway - unknown websocket close code " <> T.pack (show code)
                                  <> " [" <> TE.decodeUtf8 (BL.toStrict str) <> "]. Consider opening an issue "
                                  <> "https://github.com/Sigmacord-haskell/Sigmacord-haskell/issues")
                  pure LoopStart
      Left _ -> pure LoopReconnect



getPayloadTimeout :: SendablesData -> Chan T.Text -> IO (Either ConnectionException GatewayReceivable)
getPayloadTimeout sendablesData log = do
  let interval = heartbeatInterval sendablesData
  res <- race (threadDelay (fromInteger ((interval * 1000 * 3) `div` 2)))
              (getPayload (sendableConnection sendablesData) log)
  case res of
    Left () -> pure (Right Reconnect)
    Right other -> pure other


getPayload :: Connection -> Chan T.Text -> IO (Either ConnectionException GatewayReceivable)
getPayload conn log = try $ do
  msg' <- receiveData conn
  case eitherDecode msg' of
    Right msg -> pure msg
    Left  err -> do writeChan log ("gateway - received exception [" <> T.pack err <> "]"
                                      <> " while decoding " <> TE.decodeUtf8 (BL.toStrict msg'))
                    pure (ParseError (T.pack err))


heartbeat :: SendablesData -> IORef (UTCTime, UTCTime) -> IORef Integer -> IO ()
heartbeat sendablesData sendTimes seqKey = do
  threadDelay (3 * 10^(6 :: Int))
  forever $ do
    num <- readIORef seqKey
    sendHeartbeat sendablesData sendTimes num
    threadDelay (fromInteger (heartbeatInterval sendablesData * 1000))

sendHeartbeat :: SendablesData -> IORef (UTCTime, UTCTime) -> Integer -> IO ()
sendHeartbeat sendablesData sendTimes seqKey = do
  currTime <- getCurrentTime
  _ <- atomicModifyIORef' sendTimes (dupe . (currTime,) . fst)
  writeChan (librarySendables sendablesData) (Heartbeat seqKey)


sendableLoop :: Connection -> GatewayHandle -> SendablesData -> Chan T.Text -> IO ()
sendableLoop conn ghandle sendablesData _log = sendLoop
  where
  sendLoop = do
   
      threadDelay $ round ((10^(6 :: Int)) * (62 / 120) :: Double)
   
      payload <- race nextLibrary nextUser
      sendTextData conn (either encode encode payload)
      sendLoop

  
  nextLibrary :: IO GatewaySendableInternal
  nextLibrary = readChan (librarySendables sendablesData)

  
  nextUser :: IO GatewaySendable
  nextUser = do usersending <- readIORef (startsendingUsers sendablesData)
                if usersending
                then readChan (gatewayHandleUserSendables ghandle)
                else threadDelay (4 * (10^(6::Int))) >> nextUser

dupe :: a -> (a, a)
dupe a = (a, a)
