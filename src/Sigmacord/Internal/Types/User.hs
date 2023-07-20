{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Sigmacord.Internal.Types.User where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Sigmacord.Internal.Types.Prelude
import Data.Time (UTCTime)


data User = User
  { userId          :: UserId             
  , userName        :: T.Text             
  , userDiscrim     :: Maybe T.Text       
  , userGlobalName  :: Maybe T.Text       
  , userAvatar      :: Maybe T.Text       
  , userIsBot       :: Bool               
  , userIsWebhook   :: Bool               
  , userIsSystem    :: Maybe Bool         
  , userMfa         :: Maybe Bool         
  , userBanner      :: Maybe T.Text       
  , userAccentColor :: Maybe Int          
  , userLocale      :: Maybe T.Text       
  , userVerified    :: Maybe Bool         
  , userEmail       :: Maybe T.Text       
  , userFlags       :: Maybe Integer      
  , userPremiumType :: Maybe Integer      
  , userPublicFlags :: Maybe Integer      
  , userMember      :: Maybe GuildMember  
  } deriving (Show, Read, Eq, Ord)

instance FromJSON User where
  parseJSON = withObject "User" $ \o ->
    User <$> o .:  "id"
         <*> o .:  "username"
         <*> o .:? "discriminator" 
         <*> o .:? "global_name"
         <*> o .:? "avatar"
         <*> o .:? "bot" .!= False
         <*> pure False 
         <*> o .:? "system"
         <*> o .:? "mfa_enabled"
         <*> o .:? "banner"
         <*> o .:? "accent_color"
         <*> o .:? "locale"
         <*> o .:? "verified"
         <*> o .:? "email"
         <*> o .:? "flags"
         <*> o .:? "premium_type"
         <*> o .:? "public_flags"
         <*> o .:? "member"

instance ToJSON User where
  toJSON User{..} = objectFromMaybes
              [ "id" .== userId
              , "username" .== userName
              , "discriminator" .=? userDiscrim
              , "global_name" .=? userGlobalName
              , "avatar" .=? userAvatar
              , "bot" .== userIsBot
              , "system" .=? userIsSystem
              , "mfa_enabled" .=? userMfa
              , "banner" .=? userBanner
              , "accent_color" .=? userAccentColor
              , "verified" .=? userVerified
              , "email" .=? userEmail
              , "flags" .=? userFlags
              , "premium_type" .=? userPremiumType
              , "public_flags" .=? userPublicFlags
              , "member" .=? userPublicFlags
              ]


data Webhook = Webhook
  { webhookId :: WebhookId
  , webhookToken :: Maybe WebhookToken
  , webhookChannelId :: ChannelId
  } deriving (Show, Read, Eq, Ord)

instance FromJSON Webhook where
  parseJSON = withObject "Webhook" $ \o ->
    Webhook <$> o .:  "id"
            <*> o .:? "token"
            <*> o .:  "channel_id"


data ConnectionObject = ConnectionObject
  { connectionObjectId :: Text 
  , connectionObjectName :: Text 
  , connectionObjectType :: Text 
  , connectionObjectRevoked :: Bool 
  , connectionObjectIntegrations :: [IntegrationId] 
  , connectionObjectVerified :: Bool 
  , connectionObjectFriendSyncOn :: Bool 
  , connectionObjectShownInPresenceUpdates :: Bool 
  , connectionObjectVisibleToOthers :: Bool 
  } deriving (Show, Read, Eq, Ord)

instance FromJSON ConnectionObject where
  parseJSON = withObject "ConnectionObject" $ \o -> do
    integrations <- o .: "integrations"
    ConnectionObject <$> o .: "id"
               <*> o .: "name"
               <*> o .: "type"
               <*> o .: "revoked"
               <*> mapM (.: "id") integrations
               <*> o .: "verified"
               <*> o .: "friend_sync"
               <*> o .: "show_activity"
               <*> ( (==) (1::Int) <$> o .: "visibility")



data GuildMember = GuildMember
      { memberUser     :: Maybe User 
      , memberNick     :: Maybe T.Text 
      , memberAvatar   :: Maybe T.Text 
      , memberRoles    :: [RoleId] 
      , memberJoinedAt :: UTCTime 
      , memberPremiumSince :: Maybe UTCTime 
      , memberDeaf     :: Bool 
      , memberMute     :: Bool 
      , memberPending     :: Bool 
      , memberPermissions     :: Maybe T.Text 
      , memberTimeoutEnd :: Maybe UTCTime 
      } deriving (Show, Read, Eq, Ord)

instance FromJSON GuildMember where
  parseJSON = withObject "GuildMember" $ \o ->
    GuildMember <$> o .:? "user"
                <*> o .:? "nick"
                <*> o .:? "avatar"
                <*> o .:  "roles"
                <*> o .:  "joined_at"
                <*> o .:? "premium_since"
                <*> o .:  "deaf"
                <*> o .:  "mute"
                <*> o .:? "pending" .!= False
                <*> o .:? "permissions"
                <*> o .:? "communication_disabled_until"

instance ToJSON GuildMember where
  toJSON GuildMember {..} = objectFromMaybes
      [ "user" .=? memberUser
      , "nick" .=? memberNick
      , "avatar" .=? memberAvatar
      , "roles" .== memberRoles
      , "joined_at" .== memberJoinedAt
      , "premium_since" .=? memberPremiumSince
      , "deaf" .== memberDeaf
      , "mute" .== memberMute
      , "pending" .== memberPending
      , "permissions" .=? memberPermissions
      , "communication_disabled_until" .=? memberTimeoutEnd
      ]
