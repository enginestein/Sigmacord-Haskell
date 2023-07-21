module Sigmacord.Handle
  ( SigmacordHandle(..)
  , HandleThreadId(..)
  ) where

import Control.Concurrent (ThreadId, Chan, MVar)
import qualified Data.Text as T

import Sigmacord.Internal.Rest (RestChanHandle(..))
import Sigmacord.Internal.Gateway (GatewayHandle(..), CacheHandle(..))


data HandleThreadId
  = 
    HandleThreadIdRest ThreadId
  | 
  HandleThreadIdCache ThreadId
  | 
  HandleThreadIdLogger ThreadId
  | 
  HandleThreadIdGateway ThreadId


data SigmacordHandle = SigmacordHandle
  { 
    SigmacordHandleRestChan :: RestChanHandle
  , 
    SigmacordHandleGateway :: GatewayHandle
  , 
    SigmacordHandleCache :: CacheHandle
  , 
    SigmacordHandleThreads :: [HandleThreadId]
  , 
    SigmacordHandleLog :: Chan T.Text
  , 
    SigmacordHandleLibraryError :: MVar T.Text
  }
