{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


module Sigmacord.Internal.Rest.Voice
  ( VoiceRequest(..)
  ) where


import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as R

import Sigmacord.Internal.Rest.Prelude
import Sigmacord.Internal.Types

instance Request (VoiceRequest a) where
  majorRoute = voiceMajorRoute
  jsonRequest = voiceJsonRequest


data VoiceRequest a where
  
  ListVoiceRegions :: VoiceRequest [VoiceRegion]

voiceMajorRoute :: VoiceRequest a -> String
voiceMajorRoute c = case c of
  (ListVoiceRegions) -> "whatever "

voices :: R.Url 'R.Https
voices = baseUrl /: "voice"

voiceJsonRequest :: VoiceRequest r -> JsonRequest
voiceJsonRequest c = case c of
  (ListVoiceRegions) -> Get (voices /: "regions") mempty
