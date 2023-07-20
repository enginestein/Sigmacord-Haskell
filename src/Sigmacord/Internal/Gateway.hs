{-# LANGUAGE OverloadedStrings #-}

module Sigmacord.Internal.Gateway
  ( GatewayHandle(..)
  , CacheHandle(..)
  , GatewayException(..)
  , Cache(..)
  , startCacheThread
  , startGatewayThread
  , module Sigmacord.Internal.Types
  ) where

import Prelude hiding (log)
import Control.Concurrent.Chan (newChan, dupChan, Chan)
import Control.Concurrent (forkIO, ThreadId, newEmptyMVar, MVar)
import Data.IORef (newIORef)
import qualified Data.Text as T
import Data.Time (getCurrentTime)

import Sigmacord.Internal.Types (Auth, EventInternalParse, GatewayIntent)
import Sigmacord.Internal.Gateway.EventLoop (connectionLoop, GatewayHandle(..), GatewayException(..))
import Sigmacord.Internal.Gateway.Cache (cacheLoop, Cache(..), CacheHandle(..))


startCacheThread :: Bool -> Chan T.Text -> IO (CacheHandle, ThreadId)
startCacheThread isEnabled log = do
  events <- newChan :: IO (Chan (Either GatewayException EventInternalParse))
  cache <- newEmptyMVar :: IO (MVar (Either (Cache, GatewayException) Cache))
  let cacheHandle = CacheHandle events cache
  tid <- forkIO $ cacheLoop isEnabled cacheHandle log
  pure (cacheHandle, tid)



startGatewayThread :: Auth -> GatewayIntent -> CacheHandle -> Chan T.Text -> IO (GatewayHandle, ThreadId)
startGatewayThread auth intent cacheHandle log = do
  events <- dupChan (cacheHandleEvents cacheHandle)
  sends <- newChan
  status <- newIORef Nothing
  seqid <- newIORef 0
  seshid <- newIORef ""
  host <- newIORef "gateway.Sigmacord.gg"
  currTime <- getCurrentTime
  hbAcks <- newIORef currTime
  hbSends <- newIORef (currTime, currTime)
  let gatewayHandle = GatewayHandle events sends status seqid seshid host hbAcks hbSends
  tid <- forkIO $ connectionLoop auth intent gatewayHandle log
  pure (gatewayHandle, tid)
