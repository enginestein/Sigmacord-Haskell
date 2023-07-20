{-# LANGUAGE OverloadedStrings #-}


module Sigmacord.Internal.Types.Events where

import Prelude hiding (id)

import Data.Time.ISO8601 (parseISO8601)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.Socket (HostName)

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T

import Sigmacord.Internal.Types.Prelude
import Sigmacord.Internal.Types.Channel
import Sigmacord.Internal.Types.Guild
import Sigmacord.Internal.Types.User (User, GuildMember)
import Sigmacord.Internal.Types.Interactions (Interaction)
import Sigmacord.Internal.Types.Emoji (Emoji)
import Sigmacord.Internal.Types.ScheduledEvents (ScheduledEvent)



data Event =
  
    Ready                      Int User [GuildUnavailable] T.Text HostName (Maybe Shard) PartialApplication
  
  | Resumed                    [T.Text]
  
  | ChannelCreate              Channel
  
  | ChannelUpdate              Channel
  
  | ChannelDelete              Channel
  
  | ThreadCreate               Channel
  
  | ThreadUpdate               Channel
  
  | ThreadDelete               Channel
  
  | ThreadListSync             ThreadListSyncFields
  
  | ThreadMembersUpdate        ThreadMembersUpdateFields
  
  | ChannelPinsUpdate          ChannelId (Maybe UTCTime)
  
  | GuildCreate                Guild GuildCreateData
  
  | GuildUpdate                Guild
  
  | GuildDelete                GuildUnavailable
  
  | GuildBanAdd                GuildId User
  
  | GuildBanRemove             GuildId User
  
  | GuildEmojiUpdate           GuildId [Emoji]
  
  | GuildIntegrationsUpdate    GuildId
  
  | GuildMemberAdd             GuildId GuildMember
  
  | GuildMemberRemove          GuildId User
  
  | GuildMemberUpdate          GuildId [RoleId] User (Maybe T.Text)
  
  | GuildMemberChunk           GuildId [GuildMember]
  
  | GuildRoleCreate            GuildId Role
  
  | GuildRoleUpdate            GuildId Role
  
  | GuildRoleDelete            GuildId RoleId
  
  | MessageCreate              Message
  
  | MessageUpdate              ChannelId MessageId
  
  | MessageDelete              ChannelId MessageId
  
  | MessageDeleteBulk          ChannelId [MessageId]
  
  | MessageReactionAdd         ReactionInfo
  
  | MessageReactionRemove      ReactionInfo
  
  | MessageReactionRemoveAll   ChannelId MessageId
  
  | MessageReactionRemoveEmoji ReactionRemoveInfo
  
  | PresenceUpdate             PresenceInfo
  
  | TypingStart                TypingInfo
  
  | UserUpdate                 User
  
  | InteractionCreate          Interaction
  
  
  
  | UnknownEvent               T.Text Object
  deriving (Show, Eq)




data EventInternalParse =
    InternalReady                      Int User [GuildUnavailable] T.Text HostName (Maybe Shard) PartialApplication
  | InternalResumed                    [T.Text]
  | InternalChannelCreate              Channel
  | InternalChannelUpdate              Channel
  | InternalChannelDelete              Channel
  | InternalThreadCreate               Channel
  | InternalThreadUpdate               Channel
  | InternalThreadDelete               Channel
  | InternalThreadListSync             ThreadListSyncFields 
  | InternalThreadMembersUpdate        ThreadMembersUpdateFields 
  | InternalChannelPinsUpdate          ChannelId (Maybe UTCTime)
  | InternalGuildCreate                Guild GuildCreateData
  | InternalGuildUpdate                Guild
  | InternalGuildDelete                GuildUnavailable
  | InternalGuildBanAdd                GuildId User
  | InternalGuildBanRemove             GuildId User
  | InternalGuildEmojiUpdate           GuildId [Emoji]
  | InternalGuildIntegrationsUpdate    GuildId
  | InternalGuildMemberAdd             GuildId GuildMember
  | InternalGuildMemberRemove          GuildId User
  | InternalGuildMemberUpdate          GuildId [RoleId] User (Maybe T.Text)
  | InternalGuildMemberChunk           GuildId [GuildMember]
  | InternalGuildRoleCreate            GuildId Role
  | InternalGuildRoleUpdate            GuildId Role
  | InternalGuildRoleDelete            GuildId RoleId
  | InternalMessageCreate              Message
  | InternalMessageUpdate              ChannelId MessageId
  | InternalMessageDelete              ChannelId MessageId
  | InternalMessageDeleteBulk          ChannelId [MessageId]
  | InternalMessageReactionAdd         ReactionInfo
  | InternalMessageReactionRemove      ReactionInfo
  | InternalMessageReactionRemoveAll   ChannelId MessageId
  | InternalMessageReactionRemoveEmoji ReactionRemoveInfo
  | InternalPresenceUpdate             PresenceInfo
  | InternalTypingStart                TypingInfo
  | InternalUserUpdate                 User
  | InternalInteractionCreate          Interaction
  
  
  | InternalUnknownEvent               T.Text Object
  deriving (Show, Eq, Read)


data PartialApplication = PartialApplication
  { partialApplicationID :: ApplicationId
  , partialApplicationFlags :: Int
  } deriving (Show, Eq, Read)

instance FromJSON PartialApplication where
  parseJSON = withObject "PartialApplication" (\v -> PartialApplication <$> v .: "id" <*> v .: "flags")

data GuildCreateData = GuildCreateData
  { guildCreateJoinedAt :: !UTCTime
  , guildCreateLarge :: !Bool
  , guildCreateUnavailable :: !(Maybe Bool)
  , guildCreateMemberCount :: !Int
  
  , guildCreateMembers :: ![GuildMember]
  , guildCreateChannels :: ![Channel]
  , guildCreateThreads :: ![Channel]
  , guildCreatePresences :: ![PresenceInfo]
  
  , guildCreateScheduledEvents :: ![ScheduledEvent]
  } deriving (Show, Eq, Read)

instance FromJSON GuildCreateData where
  parseJSON = withObject "GuildCreateData" $ \o ->
    GuildCreateData <$> o .:  "joined_at"
                    <*> o .:  "large"
                    <*> o .:? "unavailable"
                    <*> o .:  "member_count"
                    <*> o .:  "members"
                    <*> o .:  "channels"
                    <*> o .:  "threads"
                    <*> o .:  "presences"
                    <*> o .:  "guild_scheduled_events"


data ReactionInfo = ReactionInfo
  { reactionUserId    :: UserId 
  , reactionGuildId   :: Maybe GuildId 
  , reactionChannelId :: ChannelId 
  , reactionMessageId :: MessageId 
  , reactionEmoji     :: Emoji 
  } deriving (Show, Read, Eq, Ord)

instance FromJSON ReactionInfo where
  parseJSON = withObject "ReactionInfo" $ \o ->
    ReactionInfo <$> o .:  "user_id"
                 <*> o .:? "guild_id"
                 <*> o .:  "channel_id"
                 <*> o .:  "message_id"
                 <*> o .:  "emoji"


data ReactionRemoveInfo  = ReactionRemoveInfo
  { reactionRemoveChannelId :: ChannelId
  , reactionRemoveGuildId   :: GuildId
  , reactionRemoveMessageId :: MessageId
  , reactionRemoveEmoji     :: Emoji
  } deriving (Show, Read, Eq, Ord)

instance FromJSON ReactionRemoveInfo where
  parseJSON = withObject "ReactionRemoveInfo" $ \o ->
    ReactionRemoveInfo <$> o .:  "guild_id"
                       <*> o .:  "channel_id"
                       <*> o .:  "message_id"
                       <*> o .:  "emoji"


data TypingInfo = TypingInfo
  { typingUserId    :: UserId
  , typingChannelId :: ChannelId
  , typingTimestamp :: UTCTime
  } deriving (Show, Read, Eq, Ord)

instance FromJSON TypingInfo where
  parseJSON = withObject "TypingInfo" $ \o ->
    do cid <- o .: "channel_id"
       uid <- o .: "user_id"
       posix <- o .: "timestamp"
       let utc = posixSecondsToUTCTime posix
       pure (TypingInfo uid cid utc)




reparse :: (ToJSON a, FromJSON b) => a -> Parser b
reparse val = case parseEither parseJSON $ toJSON val of
                Left r -> fail r
                Right b -> pure b



extractHostname :: String -> HostName
extractHostname ('w':'s':'s':':':'/':'/':rest) = extractHostname rest
extractHostname "/" = []
extractHostname (a:b) = a:extractHostname b
extractHostname [] = []


eventParse :: T.Text -> Object -> Parser EventInternalParse
eventParse t o = case t of
    "READY"                     -> InternalReady <$> o .: "v"
                                         <*> o .: "user"
                                         <*> o .: "guilds"
                                         <*> o .: "session_id"
                                          
                                          
                                          
                                         <*> (extractHostname <$> o .: "resume_gateway_url")
                                         <*> o .: "shard"
                                         <*> o .: "application"
    "RESUMED"                   -> InternalResumed <$> o .: "_trace"
    "CHANNEL_CREATE"            -> InternalChannelCreate             <$> reparse o
    "CHANNEL_UPDATE"            -> InternalChannelUpdate             <$> reparse o
    "CHANNEL_DELETE"            -> InternalChannelDelete             <$> reparse o
    "THREAD_CREATE"             -> InternalThreadCreate              <$> reparse o
    "THREAD_UPDATE"             -> InternalThreadUpdate              <$> reparse o
    "THREAD_DELETE"             -> InternalThreadDelete              <$> reparse o
    "THREAD_LIST_SYNC"          -> InternalThreadListSync            <$> reparse o
    "THREAD_MEMBERS_UPDATE"     -> InternalThreadMembersUpdate       <$> reparse o
    "CHANNEL_PINS_UPDATE"       -> do id <- o .: "channel_id"
                                      stamp <- o .:? "last_pin_timestamp"
                                      let utc = stamp >>= parseISO8601
                                      pure (InternalChannelPinsUpdate id utc)
    "GUILD_CREATE"              -> InternalGuildCreate <$> reparse o <*> reparse o
    "GUILD_UPDATE"              -> InternalGuildUpdate               <$> reparse o
    "GUILD_DELETE"              -> InternalGuildDelete               <$> reparse o
    "GUILD_BAN_ADD"             -> InternalGuildBanAdd    <$> o .: "guild_id" <*> o .: "user"
    "GUILD_BAN_REMOVE"          -> InternalGuildBanRemove <$> o .: "guild_id" <*> o .: "user"
    "GUILD_EMOJI_UPDATE"        -> InternalGuildEmojiUpdate <$> o .: "guild_id" <*> o .: "emojis"
    "GUILD_INTEGRATIONS_UPDATE" -> InternalGuildIntegrationsUpdate   <$> o .: "guild_id"
    "GUILD_MEMBER_ADD"          -> InternalGuildMemberAdd <$> o .: "guild_id" <*> reparse o
    "GUILD_MEMBER_REMOVE"       -> InternalGuildMemberRemove <$> o .: "guild_id" <*> o .: "user"
    "GUILD_MEMBER_UPDATE"       -> InternalGuildMemberUpdate <$> o .: "guild_id"
                                                             <*> o .: "roles"
                                                             <*> o .: "user"
                                                             <*> o .:? "nick"
    "GUILD_MEMBERS_CHUNK"       -> InternalGuildMemberChunk <$> o .: "guild_id" <*> o .: "members"
    "GUILD_ROLE_CREATE"         -> InternalGuildRoleCreate  <$> o .: "guild_id" <*> o .: "role"
    "GUILD_ROLE_UPDATE"         -> InternalGuildRoleUpdate  <$> o .: "guild_id" <*> o .: "role"
    "GUILD_ROLE_DELETE"         -> InternalGuildRoleDelete  <$> o .: "guild_id" <*> o .: "role_id"
    "MESSAGE_CREATE"            -> InternalMessageCreate     <$> reparse o
    "MESSAGE_UPDATE"            -> InternalMessageUpdate     <$> o .: "channel_id" <*> o .: "id"
    "MESSAGE_DELETE"            -> InternalMessageDelete     <$> o .: "channel_id" <*> o .: "id"
    "MESSAGE_DELETE_BULK"       -> InternalMessageDeleteBulk <$> o .: "channel_id" <*> o .: "ids"
    "MESSAGE_REACTION_ADD"      -> InternalMessageReactionAdd <$> reparse o
    "MESSAGE_REACTION_REMOVE"   -> InternalMessageReactionRemove <$> reparse o
    "MESSAGE_REACTION_REMOVE_ALL" -> InternalMessageReactionRemoveAll <$> o .: "channel_id"
                                                                      <*> o .: "message_id"
    "MESSAGE_REACTION_REMOVE_EMOJI" -> InternalMessageReactionRemoveEmoji <$> reparse o
    "PRESENCE_UPDATE"           -> InternalPresenceUpdate            <$> reparse o
    "TYPING_START"              -> InternalTypingStart               <$> reparse o
    "USER_UPDATE"               -> InternalUserUpdate                <$> reparse o
 
 
    "INTERACTION_CREATE"        -> InternalInteractionCreate         <$> reparse o
    _other_event                -> InternalUnknownEvent t            <$> reparse o
