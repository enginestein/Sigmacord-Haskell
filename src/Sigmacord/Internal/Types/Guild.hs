{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Sigmacord.Internal.Types.Guild where

import Data.Time.Clock

import Data.Aeson
import qualified Data.Text as T
import Data.Data (Data)
import Data.Default (Default(..))

import Sigmacord.Internal.Types.Prelude
import Sigmacord.Internal.Types.Color (SigmacordColor)
import Sigmacord.Internal.Types.User (User)
import Sigmacord.Internal.Types.Emoji (Emoji, StickerItem)
import Data.List





data Guild = Guild
      { guildId                   :: GuildId              
      , guildName                 :: T.Text               
      , guildIcon                 :: Maybe T.Text         
      , guildIconHash             :: Maybe T.Text         
      , guildSplash               :: Maybe T.Text         
      , guildDiscoverySplash      :: Maybe T.Text         
      , guildOwner                :: Maybe Bool           
      , guildOwnerId              :: UserId               
      , guildPermissions          :: Maybe T.Text         
      , guildAfkId                :: Maybe ChannelId      
      , guildAfkTimeout           :: Integer              
      , guildWidgetEnabled        :: Maybe Bool           
      , guildWidgetChannelId      :: Maybe ChannelId      
      , guildVerificationLevel    :: Integer              
      , guildNotification         :: Integer              
      , guildExplicitFilterLevel  :: Integer              
      , guildRoles                :: [Role]               
      , guildEmojis               :: [Emoji]              
      , guildFeatures             :: [T.Text]             
      , guildMultiFactAuth        :: !Integer             
      , guildApplicationId        :: Maybe ApplicationId  
      , guildSystemChannelId      :: Maybe ChannelId      
      , guildSystemChannelFlags   :: Integer              
      , guildRulesChannelId       :: Maybe ChannelId      
      , guildMaxPresences         :: Maybe Integer        
      , guildMaxMembers           :: Maybe Integer        
      , guildVanityURL            :: Maybe T.Text         
      , guildDescription          :: Maybe T.Text         
      , guildBanner               :: Maybe T.Text         
      , guildPremiumTier          :: Integer              
      , guildSubscriptionCount    :: Maybe Integer        
      , guildPreferredLocale      :: T.Text               
      , guildPublicUpdatesChannel :: Maybe ChannelId      
      , guildMaxVideoUsers        :: Maybe Integer        
      , guildApproxMemberCount    :: Maybe Integer        
      , guildApproxPresenceCount  :: Maybe Integer        
      
      , guildNSFWLevel            :: Integer              
      
      , guildStickers             :: Maybe [StickerItem]  
      
      , guildPremiumBar           :: Bool                 
      } deriving (Show, Read, Eq, Ord)

instance FromJSON Guild where
  parseJSON = withObject "Guild" $ \o ->
    Guild <$> o .:  "id"
          <*> o .:  "name"
          <*> o .:? "icon"
          <*> o .:? "icon_hash"
          <*> o .:? "splash"
          <*> o .:? "discovery_splash"
          <*> o .:? "owner"
          <*> o .:  "owner_id"
          <*> o .:? "permissions"
          <*> o .:? "afk_channel_id"
          <*> o .:  "afk_timeout"
          <*> o .:? "widget_enabled"
          <*> o .:? "widget_channel_id"
          <*> o .:  "verification_level"
          <*> o .:  "default_message_notifications"
          <*> o .:  "explicit_content_filter"
          <*> o .:  "roles"
          <*> o .:  "emojis"
          <*> o .:  "features"
          <*> o .:  "mfa_level"
          <*> o .:? "application_id"
          <*> o .:? "system_channel_id"
          <*> o .:  "system_channel_flags"
          <*> o .:? "rules_channel_id"
          <*> o .:? "max_presences"
          <*> o .:? "max_members"
          <*> o .:? "vanity_url_code"
          <*> o .:? "description"
          <*> o .:? "banner"
          <*> o .:  "premium_tier"
          <*> o .:? "premium_subscription_count"
          <*> o .:  "preferred_locale"
          <*> o .:? "public_updates_channel_id"
          <*> o .:? "max_video_channel_users"
          <*> o .:? "approximate_member_count"
          <*> o .:? "approximate_presence_count"
          
          <*> o .: "nsfw_level"
          
          <*> o .:? "stickers"
          <*> o .: "premium_progress_bar_enabled"

newtype GuildUnavailable = GuildUnavailable
      { idOnceAvailable :: GuildId
      } deriving (Show, Read, Eq, Ord)

instance FromJSON GuildUnavailable where
  parseJSON = withObject "GuildUnavailable" $ \o ->
       GuildUnavailable <$> o .: "id"

data PresenceInfo = PresenceInfo
  { presenceUserId     :: UserId
  
  , presenceActivities :: Maybe [Activity]
  , presenceGuildId    :: Maybe GuildId
  , presenceStatus     :: T.Text
  } deriving (Show, Read, Eq, Ord)

instance FromJSON PresenceInfo where
  parseJSON = withObject "PresenceInfo" $ \o ->
    PresenceInfo <$> (o .: "user" >>= (.: "id"))
                 <*> o .:  "activities"
                 <*> o .:? "guild_id"
                 <*> o .:  "status"







data Activity =
  Activity
    { activityName :: T.Text 
    , activityType :: ActivityType 
    , activityUrl :: Maybe T.Text 
    , activityCreatedAt :: Integer 
    , activityTimeStamps :: Maybe ActivityTimestamps 
    , activityApplicationId :: Maybe ApplicationId 
    , activityDetails :: Maybe T.Text 
    , activityState :: Maybe T.Text 
    , activityEmoji :: Maybe Emoji 
    , activityParty :: Maybe ActivityParty 
    
    
    , activityInstance :: Maybe Bool 
    , activityFlags :: Maybe Integer 
    , activityButtons :: Maybe [ActivityButton] 
    }
  deriving (Show, Read, Eq, Ord)

instance Default Activity where
  def = Activity "Sigmacord-haskell" ActivityTypeGame Nothing 0 Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance FromJSON Activity where
  parseJSON = withObject "Activity" $ \o -> do
    Activity <$> o .:  "name"
             <*> o .:  "type"
             <*> o .:? "url"
             <*> o .:  "created_at"
             <*> o .:? "timestamps"
             <*> o .:? "application_id"
             <*> o .:? "details"
             <*> o .:? "state"
             <*> o .:? "emoji"
             <*> o .:? "party"
             
             
             <*> o .:? "instance"
             <*> o .:? "flags"
             <*> o .:? "buttons"

data ActivityTimestamps = ActivityTimestamps
  { activityTimestampsStart :: Maybe Integer 
  , activityTimestampsEnd :: Maybe Integer 
  } deriving (Show, Read, Eq, Ord)

instance FromJSON ActivityTimestamps where
  parseJSON = withObject "ActivityTimestamps" $ \o ->
    ActivityTimestamps <$> o .:? "start"
                       <*> o .:? "end"

data ActivityParty = ActivityParty
  { activityPartyId :: Maybe T.Text
  , activityPartySize :: Maybe (Integer, Integer)
  } deriving (Show, Read, Eq, Ord)

instance FromJSON ActivityParty where
  parseJSON = withObject "ActivityParty" $ \o ->
    ActivityParty <$> o .:? "id"
                  <*> o .:? "size"

data ActivityButton = ActivityButton
  { activityButtonLabel :: T.Text
  , activityButtonUrl :: T.Text
  } deriving (Show, Read, Eq, Ord)

instance FromJSON ActivityButton where
  parseJSON = withObject "ActivityButton" $ \o ->
    ActivityButton <$> o .: "label"
                   <*> o .: "url"



data ActivityType =
    ActivityTypeGame
  | ActivityTypeStreaming
  | ActivityTypeListening
  | ActivityTypeWatching
  | ActivityTypeCustom
  | ActivityTypeCompeting
  deriving (Show, Read, Eq, Ord, Data)

instance InternalSigmacordEnum ActivityType where
  SigmacordTypeStartValue = ActivityTypeGame
  fromSigmacordType ActivityTypeGame = 0
  fromSigmacordType ActivityTypeStreaming = 1
  fromSigmacordType ActivityTypeListening = 2
  fromSigmacordType ActivityTypeWatching = 3
  fromSigmacordType ActivityTypeCustom = 4
  fromSigmacordType ActivityTypeCompeting = 5

instance FromJSON ActivityType where
  parseJSON = SigmacordTypeParseJSON "ActivityType"

data PartialGuild = PartialGuild
      { partialGuildId          :: GuildId
      , partialGuildName        :: T.Text
      , partialGuildIcon        :: Maybe T.Text
      , partialGuildOwner       :: Bool
      , partialGuildPermissions :: T.Text
      } deriving (Show, Read, Eq, Ord)

instance FromJSON PartialGuild where
  parseJSON = withObject "PartialGuild" $ \o ->
    PartialGuild <$> o .:  "id"
                 <*> o .:  "name"
                 <*> o .:? "icon"
                 <*> o .:?  "owner" .!= False
                 <*> o .:  "permissions"






data Role =
    Role {
        roleId      :: RoleId 
      , roleName    :: T.Text                    
      , roleColor   :: SigmacordColor              
      , roleHoist   :: Bool                      
      , rolePos     :: Integer                   
      , rolePerms   :: RolePermissions           
      , roleManaged :: Bool                      
      , roleMention :: Bool                      
    } deriving (Show, Read, Eq, Ord)

instance FromJSON Role where
  parseJSON = withObject "Role" $ \o ->
    Role <$> o .: "id"
         <*> o .: "name"
         <*> o .: "color"
         <*> o .: "hoist"
         <*> o .: "position"
         <*> o .: "permissions"
         <*> o .: "managed"
         <*> o .: "mentionable"




roleIdToRole :: Guild -> RoleId -> Maybe Role
roleIdToRole  g r = find(\x -> roleId x == r) $ guildRoles g



data VoiceRegion = VoiceRegion
      { voiceRegionId          :: T.Text      
      , voiceRegionName        :: T.Text      
      , voiceRegionVip         :: Bool        
      , voiceRegionOptimal     :: Bool        
      , voiceRegionDeprecated  :: Bool        
      , voiceRegionCustom      :: Bool        
      } deriving (Show, Read, Eq, Ord)

instance FromJSON VoiceRegion where
  parseJSON = withObject "VoiceRegion" $ \o ->
    VoiceRegion <$> o .: "id"
                <*> o .: "name"
                <*> o .: "vip"
                <*> o .: "optimal"
                <*> o .: "deprecated"
                <*> o .: "custom"


data GuildBan = GuildBan
      { guildBanReason  :: T.Text
      , guildBanUser    :: User
      } deriving (Show, Read, Eq, Ord)

instance FromJSON GuildBan where
  parseJSON = withObject "GuildBan" $ \o -> GuildBan <$> o .: "reason" <*> o .: "user"


data Invite = Invite
      { inviteCode  :: T.Text    
      , inviteGuildId :: Maybe GuildId 
      , inviteChannelId :: ChannelId 
      } deriving (Show, Read, Eq, Ord)

instance FromJSON Invite where
  parseJSON = withObject "Invite" $ \o ->
    Invite <$>  o .: "code"
           <*> (do g <- o .:? "guild"
                   case g of Just g2 -> g2 .: "id"
                             Nothing -> pure Nothing)
           <*> ((o .:  "channel") >>= (.: "id"))


data InviteWithMeta = InviteWithMeta Invite InviteMeta

instance FromJSON InviteWithMeta where
  parseJSON ob = InviteWithMeta <$> parseJSON ob <*> parseJSON ob


data InviteMeta = InviteMeta
    { inviteCreator :: User    
    , inviteUses    :: Integer 
    , inviteMax     :: Integer 
    , inviteAge     :: Integer 
    , inviteTemp    :: Bool    
    , inviteCreated :: UTCTime 
    , inviteRevoked :: Bool    
    } deriving (Show, Read, Eq, Ord)

instance FromJSON InviteMeta where
  parseJSON = withObject "InviteMeta" $ \o ->
    InviteMeta <$> o .: "inviter"
               <*> o .: "uses"
               <*> o .: "max_uses"
               <*> o .: "max_age"
               <*> o .: "temporary"
               <*> o .: "created_at"
               <*> o .: "revoked"


data Integration = Integration
      { integrationId       :: !Snowflake 
      , integrationName     :: T.Text                    
      , integrationType     :: T.Text                    
      , integrationEnabled  :: Bool                      
      , integrationSyncing  :: Bool                      
      , integrationRole     :: RoleId                 
      , integrationBehavior :: Integer                   
      , integrationGrace    :: Integer                   
      , integrationOwner    :: User                      
      , integrationAccount  :: IntegrationAccount        
      , integrationSync     :: UTCTime                   
      } deriving (Show, Read, Eq, Ord)

instance FromJSON Integration where
  parseJSON = withObject "Integration" $ \o ->
    Integration <$> o .: "id"
                <*> o .: "name"
                <*> o .: "type"
                <*> o .: "enabled"
                <*> o .: "syncing"
                <*> o .: "role_id"
                <*> o .: "expire_behavior"
                <*> o .: "expire_grace_period"
                <*> o .: "user"
                <*> o .: "account"
                <*> o .: "synced_at"


data IntegrationAccount = IntegrationAccount
    { accountId   :: T.Text 
    , accountName :: T.Text 
    } deriving (Show, Read, Eq, Ord)

instance FromJSON IntegrationAccount where
  parseJSON = withObject "IntegrationAccount" $ \o ->
    IntegrationAccount <$> o .: "id" <*> o .: "name"


data GuildWidget = GuildWidget
      { widgetEnabled :: Bool      
      , widgetChannelId :: ChannelId 
      } deriving (Show, Read, Eq, Ord)

instance FromJSON GuildWidget where
  parseJSON = withObject "GuildWidget" $ \o ->
    GuildWidget <$> o .: "enabled" <*> o .: "channel_id"

instance ToJSON GuildWidget where
  toJSON (GuildWidget enabled snowflake) = object
    [ "enabled"   .= enabled
    , "channel_id" .= snowflake
    ]
