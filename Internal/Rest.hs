{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings  #-}

module Sigmacord.Internal.Rest
  ( module Sigmacord.Internal.Types
  , RestChanHandle(..)
  , Request(..)
  , writeRestCall
  , startRestThread
  , RestCallInternalException(..)
  ) where

import Prelude hiding (log)
import Data.Aeson (FromJSON, eitherDecode)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent (forkIO, ThreadId)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T


import Sigmacord.Internal.Types
import Sigmacord.Internal.Rest.HTTP


data RestChanHandle = RestChanHandle
      { restHandleChan :: Chan (String, JsonRequest, MVar (Either RestCallInternalException BL.ByteString))
      }


startRestThread :: Auth -> Chan T.Text -> IO (RestChanHandle, ThreadId)
startRestThread auth log = do
  c <- newChan
  tid <- forkIO $ restLoop auth c log
  pure (RestChanHandle c, tid)


writeRestCall :: (Request (r a), FromJSON a) => RestChanHandle -> r a -> IO (Either RestCallInternalException a)
writeRestCall c req = do
  m <- newEmptyMVar
  writeChan (restHandleChan c) (majorRoute req, jsonRequest req, m)
  r <- readMVar m
  pure $ case eitherDecode <$> r of
    Right (Right o) -> Right o
    (Right (Left er)) -> Left (RestCallInternalNoParse er (case r of
      Right x -> x
      Left _ -> ""))
    Left e -> Left e


