{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


module Sigmacord.Internal.Rest.Invite
  ( InviteRequest(..)
  ) where

import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as R
import qualified Data.Text as T

import Sigmacord.Internal.Rest.Prelude
import Sigmacord.Internal.Types

instance Request (InviteRequest a) where
  majorRoute = inviteMajorRoute
  jsonRequest = inviteJsonRequest



data InviteRequest a where
  
  GetInvite :: T.Text -> InviteRequest Invite
  
  DeleteInvite :: T.Text -> InviteRequest Invite

inviteMajorRoute :: InviteRequest a -> String
inviteMajorRoute c = case c of
  (GetInvite _) ->     "invite "
  (DeleteInvite _) ->  "invite "

invite :: R.Url 'R.Https
invite = baseUrl /: "invites"

inviteJsonRequest :: InviteRequest r -> JsonRequest
inviteJsonRequest c = case c of
  (GetInvite g) -> Get (invite R./: g) mempty
  (DeleteInvite g) -> Delete (invite R./: g) mempty
