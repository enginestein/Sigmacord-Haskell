{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


module Sigmacord.Internal.Rest.User
  ( UserRequest(..)
  , parseAvatarImage
  ) where


import Data.Aeson
import Network.HTTP.Req ((/:), (/~))
import qualified Network.HTTP.Req as R
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64

import Sigmacord.Internal.Rest.Prelude
import Sigmacord.Internal.Types

instance Request (UserRequest a) where
  majorRoute = userMajorRoute
  jsonRequest = userJsonRequest



data UserRequest a where
  
  
  
  GetCurrentUser       :: UserRequest User
  
  GetUser              :: UserId -> UserRequest User
  
  ModifyCurrentUser    :: T.Text -> Base64Image User -> UserRequest User
  
  
  GetCurrentUserGuilds :: UserRequest [PartialGuild]
  
  LeaveGuild           :: GuildId -> UserRequest ()
  
  GetUserDMs           :: UserRequest [Channel]
  
  CreateDM             :: UserId -> UserRequest Channel

  GetUserConnections   :: UserRequest [ConnectionObject]








parseAvatarImage :: B.ByteString -> Either T.Text (Base64Image User)
parseAvatarImage bs
  | Just mime <- getMimeType bs = Right (Base64Image mime (TE.decodeUtf8 (B64.encode bs)))
  | otherwise                   = Left "Unsupported image format provided"

userMajorRoute :: UserRequest a -> String
userMajorRoute c = case c of
  (GetCurrentUser) ->                        "me "
  (GetUser _) ->                           "user "
  (ModifyCurrentUser _ _) ->        "modify_user "
  (GetCurrentUserGuilds) ->     "get_user_guilds "
  (LeaveGuild g) ->                 "leave_guild " <> show g
  (GetUserDMs) ->                       "get_dms "
  (CreateDM _) ->                       "make_dm "
  (GetUserConnections) ->           "connections "

users :: R.Url 'R.Https
users = baseUrl /: "users"

userJsonRequest :: UserRequest r -> JsonRequest
userJsonRequest c = case c of
  (GetCurrentUser) -> Get (users /: "@me") mempty

  (GetUser user) -> Get (users /~ user ) mempty

  (ModifyCurrentUser name b64im) ->
      Patch (users /: "@me")  (pure (R.ReqBodyJson (object [ "username" .= name
                                                           , "avatar" .= b64im ]))) mempty

  (GetCurrentUserGuilds) -> Get (users /: "@me" /: "guilds") mempty

  (LeaveGuild guild) -> Delete (users /: "@me" /: "guilds" /~ guild) mempty

  (GetUserDMs) -> Get (users /: "@me" /: "channels") mempty

  (CreateDM user) ->
      let body = R.ReqBodyJson $ object ["recipient_id" .= user]
      in Post (users /: "@me" /: "channels") (pure body) mempty

  (GetUserConnections) ->
    Get (users /: "@me" /: "connections") mempty
