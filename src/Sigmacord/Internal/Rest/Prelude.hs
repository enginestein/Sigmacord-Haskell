{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}


module Sigmacord.Internal.Rest.Prelude where

import Prelude hiding (log)
import Control.Exception.Safe (throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.String (IsString(fromString))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Network.HTTP.Req as R
import Web.Internal.HttpApiData (ToHttpApiData)

import Sigmacord.Internal.Types

import Paths_Sigmacord_haskell (version)
import Data.Version (showVersion)


apiVersion :: T.Text
apiVersion = "10"


baseUrl :: R.Url 'R.Https
baseUrl = R.https "Sigmacord.com" R./: "api" R./: apiVersion'
  where apiVersion' = "v" <> apiVersion


authHeader :: Auth -> R.Option 'R.Https
authHeader auth =
          R.header "Authorization" (TE.encodeUtf8 (authToken auth))
       <> R.header "User-Agent" agent
  where
  
  
  agent = fromString $ "SigmacordBot (https://github.com/Sigmacord-haskell/Sigmacord-haskell, " <> showVersion version <> ")"


infixl 5 /?
(/?) :: ToHttpApiData a => R.Url scheme -> Maybe a -> R.Url scheme
(/?) url Nothing = url
(/?) url (Just part) = url R./~ part



data JsonRequest where
  Delete ::                 R.Url 'R.Https ->      R.Option 'R.Https -> JsonRequest
  Get    ::                 R.Url 'R.Https ->      R.Option 'R.Https -> JsonRequest
  Put    :: R.HttpBody a => R.Url 'R.Https -> a -> R.Option 'R.Https -> JsonRequest
  Patch  :: R.HttpBody a => R.Url 'R.Https -> RestIO a -> R.Option 'R.Https -> JsonRequest
  Post   :: R.HttpBody a => R.Url 'R.Https -> RestIO a -> R.Option 'R.Https -> JsonRequest

class Request a where
  
  
  majorRoute :: a -> String

  
  jsonRequest :: a -> JsonRequest


newtype RestIO a = RestIO { restIOtoIO :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance R.MonadHttp RestIO where
  
  handleHttpException = liftIO . throwIO
  
  getHttpConfig = pure $ R.defaultHttpConfig { R.httpConfigCheckResponse = \_ _ _ -> Nothing }
