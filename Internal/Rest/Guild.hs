{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


module Sigmacord.Internal.Rest.Guild
  ( GuildRequest(..)
  , CreateGuildChannelOpts(..)
  , ModifyGuildOpts(..)
  , AddGuildMemberOpts(..)
  , ModifyGuildMemberOpts(..)
  , GuildMembersTiming(..)
  , CreateGuildBanOpts(..)
  , ModifyGuildRoleOpts(..)
  , CreateGuildIntegrationOpts(..)
  , ModifyGuildIntegrationOpts(..)
  ) where


import Data.Aeson
import Network.HTTP.Req ((/:), (/~))
import qualified Network.HTTP.Req as R
import qualified Data.Text as T

import Sigmacord.Internal.Rest.Prelude
import Sigmacord.Internal.Types
import Data.Default (Default(..))

instance Request (GuildRequest a) where
  majorRoute = guildMajorRoute
  jsonRequest = guildJsonRequest


data GuildRequest a where
  
  

  
  GetGuild                 :: GuildId -> GuildRequest Guild
  
  
  ModifyGuild              :: GuildId -> ModifyGuildOpts -> GuildRequest Guild
  
  DeleteGuild              :: GuildId -> GuildRequest ()
  
  GetGuildChannels         :: GuildId -> GuildRequest [Channel]
  
  
  
  CreateGuildChannel       :: GuildId -> T.Text -> [Overwrite] -> CreateGuildChannelOpts -> GuildRequest Channel
  
  
  
  ModifyGuildChannelPositions      :: GuildId -> [(ChannelId,Int)] -> GuildRequest [Channel]
  
  GetGuildMember           :: GuildId -> UserId -> GuildRequest GuildMember
  
  ListGuildMembers         :: GuildId -> GuildMembersTiming -> GuildRequest [GuildMember]
  
  
  
  
  AddGuildMember           :: GuildId -> UserId -> AddGuildMemberOpts
                                      -> GuildRequest ()
  
  ModifyGuildMember        :: GuildId -> UserId -> ModifyGuildMemberOpts -> GuildRequest GuildMember
  
  ModifyCurrentUserNick    :: GuildId -> T.Text -> GuildRequest ()
  
  AddGuildMemberRole    :: GuildId -> UserId -> RoleId -> GuildRequest ()
  
  RemoveGuildMemberRole    :: GuildId -> UserId -> RoleId -> GuildRequest ()
  
  
  RemoveGuildMember        :: GuildId -> UserId -> GuildRequest ()
  
  
  GetGuildBans             :: GuildId -> GuildRequest [GuildBan]
  
  
  GetGuildBan              :: GuildId -> UserId -> GuildRequest GuildBan
  
  
  CreateGuildBan           :: GuildId -> UserId -> CreateGuildBanOpts -> GuildRequest ()
  
  
  RemoveGuildBan           :: GuildId -> UserId -> GuildRequest ()
  
  
  GetGuildRoles            :: GuildId -> GuildRequest [Role]
  
  
  CreateGuildRole          :: GuildId -> ModifyGuildRoleOpts -> GuildRequest Role
  
  
  
  ModifyGuildRolePositions :: GuildId -> [(RoleId, Integer)] -> GuildRequest [Role]
  
  
  ModifyGuildRole          :: GuildId -> RoleId -> ModifyGuildRoleOpts -> GuildRequest Role
  
  
  DeleteGuildRole          :: GuildId -> RoleId -> GuildRequest ()
  
  
  
  GetGuildPruneCount       :: GuildId -> Integer -> GuildRequest Object
  
  
  
  BeginGuildPrune          :: GuildId -> Integer -> GuildRequest Object
  
  
  GetGuildVoiceRegions     :: GuildId -> GuildRequest [VoiceRegion]
  
  
  GetGuildInvites          :: GuildId -> GuildRequest [Invite]
  
  
  GetGuildIntegrations     :: GuildId -> GuildRequest [Integration]
  
  
  CreateGuildIntegration   :: GuildId -> IntegrationId -> CreateGuildIntegrationOpts -> GuildRequest ()
  
  
  ModifyGuildIntegration   :: GuildId -> IntegrationId -> ModifyGuildIntegrationOpts
                                      -> GuildRequest ()
  
  
  DeleteGuildIntegration   :: GuildId -> IntegrationId -> GuildRequest ()
  
  SyncGuildIntegration     :: GuildId -> IntegrationId -> GuildRequest ()
  
  GetGuildWidget            :: GuildId -> GuildRequest GuildWidget
  
  
  
  ModifyGuildWidget         :: GuildId -> GuildWidget -> GuildRequest GuildWidget
  
  GetGuildVanityURL        :: GuildId -> GuildRequest T.Text


data ModifyGuildIntegrationOpts = ModifyGuildIntegrationOpts
  { modifyGuildIntegrationOptsExpireBehavior :: Integer
  , modifyGuildIntegrationOptsExpireGraceSeconds :: Integer
  , modifyGuildIntegrationOptsEmoticonsEnabled :: Bool
  } deriving (Show, Read, Eq, Ord)

instance ToJSON ModifyGuildIntegrationOpts where
  toJSON ModifyGuildIntegrationOpts{..} = objectFromMaybes
         [ "expire_grace_period" .== modifyGuildIntegrationOptsExpireGraceSeconds
         , "expire_behavior" .== modifyGuildIntegrationOptsExpireBehavior
         , "enable_emoticons" .== modifyGuildIntegrationOptsEmoticonsEnabled ]


newtype CreateGuildIntegrationOpts = CreateGuildIntegrationOpts
  { createGuildIntegrationOptsType :: T.Text
  } deriving (Show, Read, Eq, Ord)

instance ToJSON CreateGuildIntegrationOpts where
  toJSON CreateGuildIntegrationOpts{..} = objectFromMaybes
                       ["type" .== createGuildIntegrationOptsType]


data CreateGuildBanOpts = CreateGuildBanOpts
  { createGuildBanOptsDeleteLastNMessages :: Maybe Int
  , createGuildBanOptsReason              :: Maybe T.Text
  } deriving (Show, Read, Eq, Ord)

instance ToJSON CreateGuildBanOpts where
  toJSON CreateGuildBanOpts{..} = objectFromMaybes
                       [ "delete_message_days"
                           .=? createGuildBanOptsDeleteLastNMessages
                       , "reason" .=? createGuildBanOptsReason]


data ModifyGuildRoleOpts = ModifyGuildRoleOpts
  { modifyGuildRoleOptsName            :: Maybe T.Text
  , modifyGuildRoleOptsPermissions     :: Maybe RolePermissions
  , modifyGuildRoleOptsColor           :: Maybe SigmacordColor
  , modifyGuildRoleOptsSeparateSidebar :: Maybe Bool
  , modifyGuildRoleOptsMentionable     :: Maybe Bool
  , modifyGuildRoleOptsIcon            :: Maybe T.Text
  } deriving (Show, Read, Eq, Ord)

instance ToJSON ModifyGuildRoleOpts where
  toJSON ModifyGuildRoleOpts{..} = objectFromMaybes
                       ["name" .=? modifyGuildRoleOptsName,
                        "permissions" .=? modifyGuildRoleOptsPermissions,
                        "color" .=? modifyGuildRoleOptsColor,
                        "hoist" .=? modifyGuildRoleOptsSeparateSidebar,
                        "mentionable" .=? modifyGuildRoleOptsMentionable,
                        "icon" .=? modifyGuildRoleOptsIcon]


data AddGuildMemberOpts = AddGuildMemberOpts
  { addGuildMemberOptsAccessToken :: T.Text
  , addGuildMemberOptsNickname    :: Maybe T.Text
  , addGuildMemberOptsRoles       :: Maybe [RoleId]
  , addGuildMemberOptsIsMuted     :: Maybe Bool
  , addGuildMemberOptsIsDeafened  :: Maybe Bool
  } deriving (Show, Read, Eq, Ord)

instance ToJSON AddGuildMemberOpts where
  toJSON AddGuildMemberOpts{..} = objectFromMaybes
                                  ["access_token" .== addGuildMemberOptsAccessToken,
                                   "nick" .=? addGuildMemberOptsNickname,
                                   "roles" .=? addGuildMemberOptsRoles,
                                   "mute" .=? addGuildMemberOptsIsMuted,
                                   "deaf" .=? addGuildMemberOptsIsDeafened]


data ModifyGuildMemberOpts = ModifyGuildMemberOpts
  { modifyGuildMemberOptsNickname      :: Maybe T.Text
  , modifyGuildMemberOptsRoles         :: Maybe [RoleId]
  , modifyGuildMemberOptsIsMuted       :: Maybe Bool
  , modifyGuildMemberOptsIsDeafened    :: Maybe Bool
  , modifyGuildMemberOptsMoveToChannel :: Maybe ChannelId
  , modifyGuildMemberOptsTimeoutUntil  :: Maybe (Maybe UTCTime) 
  } deriving (Show, Read, Eq, Ord)

instance Default ModifyGuildMemberOpts where
  def = ModifyGuildMemberOpts Nothing Nothing Nothing Nothing Nothing Nothing

instance ToJSON ModifyGuildMemberOpts where
  toJSON ModifyGuildMemberOpts{..} = objectFromMaybes
                                  ["nick" .=? modifyGuildMemberOptsNickname,
                                   "roles" .=? modifyGuildMemberOptsRoles,
                                   "mute" .=? modifyGuildMemberOptsIsMuted,
                                   "deaf" .=? modifyGuildMemberOptsIsDeafened,
                                   "channel_id" .=? modifyGuildMemberOptsMoveToChannel,
                                   "communication_disabled_until" .=? modifyGuildMemberOptsTimeoutUntil]


data CreateGuildChannelOpts
  
  = CreateGuildChannelOptsText {
    createGuildChannelOptsTopic :: Maybe T.Text
  , createGuildChannelOptsUserMessageRateDelay :: Maybe Integer
  , createGuildChannelOptsIsNSFW :: Maybe Bool
  , createGuildChannelOptsCategoryId :: Maybe ChannelId }
  
  | CreateGuildChannelOptsVoice {
    createGuildChannelOptsBitrate :: Maybe Integer
  , createGuildChannelOptsMaxUsers :: Maybe Integer
  , createGuildChannelOptsCategoryId :: Maybe ChannelId }
  
  | CreateGuildChannelOptsCategory
  deriving (Show, Read, Eq, Ord)


createChannelOptsToJSON :: T.Text -> [Overwrite] -> CreateGuildChannelOpts -> Value
createChannelOptsToJSON name perms opts = objectFromMaybes optsJSON
  where
  optsJSON = case opts of
    CreateGuildChannelOptsText{..} ->
                          ["name" .== String name
                          ,"type" .== Number 0
                          ,"permission_overwrites" .== perms
                          ,"topic" .=? createGuildChannelOptsTopic
                          ,"rate_limit_per_user" .=? createGuildChannelOptsUserMessageRateDelay
                          ,"nsfw" .=? createGuildChannelOptsIsNSFW
                          ,"parent_id" .=? createGuildChannelOptsCategoryId]
    CreateGuildChannelOptsVoice{..} ->
                          ["name" .== String name
                          ,"type" .== Number 2
                          ,"permission_overwrites" .== perms
                          ,"bitrate" .=? createGuildChannelOptsBitrate
                          ,"user_limit" .=? createGuildChannelOptsMaxUsers
                          ,"parent_id" .=? createGuildChannelOptsCategoryId]
    CreateGuildChannelOptsCategory ->
                          ["name" .== String name
                          ,"type" .== Number 4
                          ,"permission_overwrites" .== perms]





data ModifyGuildOpts = ModifyGuildOpts
  { modifyGuildOptsName         :: Maybe T.Text
  , modifyGuildOptsAFKChannelId :: Maybe ChannelId
  , modifyGuildOptsIcon         :: Maybe T.Text
  , modifyGuildOptsOwnerId      :: Maybe UserId
   
   
   
   
  } deriving (Show, Read, Eq, Ord)

instance ToJSON ModifyGuildOpts where
  toJSON ModifyGuildOpts{..} = objectFromMaybes
                                  ["name" .=? modifyGuildOptsName,
                                   "afk_channel_id" .=? modifyGuildOptsAFKChannelId,
                                   "icon" .=? modifyGuildOptsIcon,
                                   "owner_id" .=? modifyGuildOptsOwnerId]

data GuildMembersTiming = GuildMembersTiming
                          { guildMembersTimingLimit :: Maybe Int
                          , guildMembersTimingAfter :: Maybe UserId
                          } deriving (Show, Read, Eq, Ord)

guildMembersTimingToQuery :: GuildMembersTiming -> R.Option 'R.Https
guildMembersTimingToQuery (GuildMembersTiming mLimit mAfter) =
  let limit = case mLimit of
              Nothing -> mempty
              Just lim -> "limit" R.=: lim
      after = case mAfter of
              Nothing -> mempty
              Just aft -> "after" R.=: show aft
  in limit <> after

guildMajorRoute :: GuildRequest a -> String
guildMajorRoute c = case c of
  (GetGuild g) ->                         "guild " <> show g
  (ModifyGuild g _) ->                    "guild " <> show g
  (DeleteGuild g) ->                      "guild " <> show g
  (GetGuildChannels g) ->            "guild_chan " <> show g
  (CreateGuildChannel g _ _ _) ->    "guild_chan " <> show g
  (ModifyGuildChannelPositions g _) -> "guild_chan " <> show g
  (GetGuildMember g _) ->            "guild_memb " <> show g
  (ListGuildMembers g _) ->         "guild_membs " <> show g
  (AddGuildMember g _ _) ->         "guild_membs " <> show g
  (ModifyGuildMember g _ _) ->      "guild_membs " <> show g
  (ModifyCurrentUserNick g _) ->    "guild_membs " <> show g
  (AddGuildMemberRole g _ _) ->     "guild_membs " <> show g
  (RemoveGuildMemberRole g _ _) ->  "guild_membs " <> show g
  (RemoveGuildMember g _) ->        "guild_membs " <> show g
  (GetGuildBan g _) ->               "guild_bans " <> show g
  (GetGuildBans g) ->                "guild_bans " <> show g
  (CreateGuildBan g _ _) ->           "guild_ban " <> show g
  (RemoveGuildBan g _) ->             "guild_ban " <> show g
  (GetGuildRoles g) ->              "guild_roles " <> show g
  (CreateGuildRole g _) ->          "guild_roles " <> show g
  (ModifyGuildRolePositions g _) -> "guild_roles " <> show g
  (ModifyGuildRole g _ _) ->         "guild_role " <> show g
  (DeleteGuildRole g _) ->           "guild_role " <> show g
  (GetGuildPruneCount g _) ->       "guild_prune " <> show g
  (BeginGuildPrune g _) ->          "guild_prune " <> show g
  (GetGuildVoiceRegions g) ->       "guild_voice " <> show g
  (GetGuildInvites g) ->            "guild_invit " <> show g
  (GetGuildIntegrations g) ->       "guild_integ " <> show g
  (CreateGuildIntegration g _ _) -> "guild_integ " <> show g
  (ModifyGuildIntegration g _ _) -> "guild_intgr " <> show g
  (DeleteGuildIntegration g _) ->   "guild_intgr " <> show g
  (SyncGuildIntegration g _) ->      "guild_sync " <> show g
  (GetGuildWidget g) ->            "guild_widget " <> show g
  (ModifyGuildWidget g _) ->       "guild_widget " <> show g
  (GetGuildVanityURL g) ->                "guild " <> show g


guilds :: R.Url 'R.Https
guilds = baseUrl /: "guilds"

guildJsonRequest :: GuildRequest r -> JsonRequest
guildJsonRequest c = case c of
  (GetGuild guild) ->
      Get (guilds /~ guild) mempty

  (ModifyGuild guild patch) ->
      Patch (guilds /~ guild) (pure (R.ReqBodyJson patch)) mempty

  (DeleteGuild guild) ->
      Delete (guilds /~ guild) mempty

  (GetGuildChannels guild) ->
      Get (guilds /~ guild /: "channels") mempty

  (CreateGuildChannel guild name perms patch) ->
      Post (guilds /~ guild /: "channels")
           (pure (R.ReqBodyJson (createChannelOptsToJSON name perms patch))) mempty

  (ModifyGuildChannelPositions guild newlocs) ->
      let patch = map (\(a, b) -> object [("id", toJSON a)
                                         ,("position", toJSON b)]) newlocs
      in Patch (guilds /~ guild /: "channels") (pure (R.ReqBodyJson patch)) mempty

  (GetGuildMember guild member) ->
      Get (guilds /~ guild /: "members" /~ member) mempty

  (ListGuildMembers guild range) ->
      Get (guilds /~ guild /: "members") (guildMembersTimingToQuery range)

  (AddGuildMember guild user patch) ->
      Put (guilds /~ guild /: "members" /~ user) (R.ReqBodyJson patch) mempty

  (ModifyGuildMember guild member patch) ->
      Patch (guilds /~ guild /: "members" /~ member) (pure (R.ReqBodyJson patch)) mempty

  (ModifyCurrentUserNick guild name) ->
      let patch = object ["nick" .= name]
      in Patch (guilds /~ guild /: "members/@me/nick") (pure (R.ReqBodyJson patch)) mempty

  (AddGuildMemberRole guild user role) ->
      let body = R.ReqBodyJson (object [])
      in Put (guilds /~ guild /: "members" /~ user /: "roles" /~ role) body mempty

  (RemoveGuildMemberRole guild user role) ->
      Delete (guilds /~ guild /: "members" /~ user /: "roles" /~ role) mempty

  (RemoveGuildMember guild user) ->
      Delete (guilds /~ guild /: "members" /~ user) mempty

  (GetGuildBan guild user) -> Get (guilds /~ guild /: "bans" /~ user) mempty

  (GetGuildBans guild) -> Get (guilds /~ guild /: "bans") mempty

  (CreateGuildBan guild user patch) ->
      Put (guilds /~ guild /: "bans" /~ user) (R.ReqBodyJson patch) mempty

  (RemoveGuildBan guild ban) ->
      Delete (guilds /~ guild /: "bans" /~ ban) mempty

  (GetGuildRoles guild) ->
      Get (guilds /~ guild /: "roles") mempty

  (CreateGuildRole guild patch) ->
      Post (guilds /~ guild /: "roles") (pure (R.ReqBodyJson patch)) mempty

  (ModifyGuildRolePositions guild patch) ->
      let body = map (\(role, pos) -> object ["id".=role, "position".=pos]) patch
      in Patch (guilds /~ guild /: "roles") (pure (R.ReqBodyJson body)) mempty

  (ModifyGuildRole guild role patch) ->
      Patch (guilds /~ guild /: "roles" /~ role) (pure (R.ReqBodyJson patch)) mempty

  (DeleteGuildRole guild role) ->
      Delete (guilds /~ guild /: "roles" /~ role) mempty

  (GetGuildPruneCount guild days) ->
      Get (guilds /~ guild /: "prune") ("days" R.=: days)

  (BeginGuildPrune guild days) ->
      Post (guilds /~ guild /: "prune") (pure R.NoReqBody) ("days" R.=: days)

  (GetGuildVoiceRegions guild) ->
      Get (guilds /~ guild /: "regions") mempty

  (GetGuildInvites guild) ->
      Get (guilds /~ guild /: "invites") mempty

  (GetGuildIntegrations guild) ->
      Get (guilds /~ guild /: "integrations") mempty

  (CreateGuildIntegration guild iid opts) ->
      let patch = object ["type" .= createGuildIntegrationOptsType opts, "id" .= iid]
      in Post (guilds /~ guild /: "integrations") (pure (R.ReqBodyJson patch)) mempty

  (ModifyGuildIntegration guild iid patch) ->
      let body = pure (R.ReqBodyJson patch)
      in Patch (guilds /~ guild /: "integrations" /~ iid) body mempty

  (DeleteGuildIntegration guild integ) ->
      Delete (guilds /~ guild /: "integrations" /~ integ) mempty

  (SyncGuildIntegration guild integ) ->
      Post (guilds /~ guild /: "integrations" /~ integ) (pure R.NoReqBody) mempty

  (GetGuildWidget guild) ->
      Get (guilds /~ guild /: "integrations") mempty

  (ModifyGuildWidget guild patch) ->
      Patch (guilds /~ guild /: "widget") (pure (R.ReqBodyJson patch)) mempty

  (GetGuildVanityURL guild) ->
      Get (guilds /~ guild /: "vanity-url") mempty

