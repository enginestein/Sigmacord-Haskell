{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Sigmacord.Internal.Types.Channel (
    Channel (..)
  , channelIsInGuild
  , Overwrite (..)
  , ThreadMetadata (..)
  , ThreadMember (..)
  , ThreadListSyncFields (..)
  , ThreadMembersUpdateFields (..)
  , Message (..)
  , AllowedMentions (..)
  , MessageReaction (..)
  , Attachment (..)
  , Nonce (..)
  , MessageReference (..)
  , MessageType (..)
  , MessageActivity (..)
  , MessageActivityType (..)
  , MessageFlag (..)
  , MessageFlags (..)
  , MessageInteraction (..)

  , ChannelTypeOption (..)
  ) where

import Control.Applicative (empty)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Default (Default, def)
import Data.Text (Text)
import Data.Time.Clock
import qualified Data.Text as T
import Data.Bits
import Data.Data (Data)

import Sigmacord.Internal.Types.Prelude
import Sigmacord.Internal.Types.User (User(..), GuildMember)
import Sigmacord.Internal.Types.Embed
import Sigmacord.Internal.Types.Components (ActionRow)
import Sigmacord.Internal.Types.Emoji


data Channel
  
  = ChannelText
      { channelId          :: ChannelId         
                                                
      , channelGuild       :: GuildId           
      , channelName        :: T.Text            
      , channelPosition    :: Integer           
      , channelPermissions :: [Overwrite]       
      , channelUserRateLimit :: Integer         
      , channelNSFW        :: Bool              
      , channelTopic       :: T.Text            
      , channelLastMessage :: Maybe MessageId   
                                                
      , channelParentId    :: Maybe ParentId    
      }
  
  | ChannelNews
      { channelId          :: ChannelId       
      , channelGuild       :: GuildId         
      , channelName        :: T.Text          
      , channelPosition    :: Integer         
      , channelPermissions :: [Overwrite]     
      , channelNSFW        :: Bool            
      , channelTopic       :: T.Text          
      , channelLastMessage :: Maybe MessageId 
      , channelParentId    :: Maybe ParentId  
      }
   
  | ChannelStorePage
      { channelId          :: ChannelId      
      , channelGuild       :: GuildId        
      , channelName        :: T.Text         
      , channelPosition    :: Integer        
      , channelNSFW        :: Bool           
      , channelPermissions :: [Overwrite]    
      , channelParentId    :: Maybe ParentId 
      }
  
  | ChannelVoice
      { channelId          :: ChannelId       
      , channelGuild       :: GuildId         
      , channelName        :: T.Text          
      , channelPosition    :: Integer         
      , channelPermissions :: [Overwrite]     
      , channelNSFW        :: Bool            
      , channelBitRate     :: Integer         
      , channelUserLimit   :: Integer         
      , channelParentId    :: Maybe ParentId  
      }
  
  
  | ChannelDirectMessage
      { channelId          :: ChannelId       
      , channelRecipients  :: [User]          
      , channelLastMessage :: Maybe MessageId 
      }
  
  | ChannelGroupDM
      { channelId          :: ChannelId       
      , channelRecipients  :: [User]          
      , channelLastMessage :: Maybe MessageId 
      }
  
  | ChannelGuildCategory
      { channelId          :: ChannelId   
      , channelGuild       :: GuildId     
      , channelName        :: T.Text      
      , channelPosition    :: Integer     
      , channelPermissions :: [Overwrite] 
      }
  
  | ChannelStage
      { channelId          :: ChannelId 
      , channelGuild       :: GuildId   
      , channelStageId     :: StageId   
      , channelStageTopic  :: Text      
      }
  
  | ChannelNewsThread
      { channelId          :: ChannelId               
      , channelGuild       :: GuildId                 
      , channelThreadName  :: Maybe T.Text            
      , channelUserRateLimitThread :: Maybe Integer   
      , channelLastMessage :: Maybe MessageId         
                                                      
      , channelParentId    :: Maybe ParentId          
      , channelThreadMetadata :: Maybe ThreadMetadata 
      , channelThreadMember :: Maybe ThreadMember     
      }
  
  | ChannelPublicThread
      { channelId          :: ChannelId               
      , channelGuild       :: GuildId                 
      , channelThreadName  :: Maybe T.Text            
      , channelUserRateLimitThread :: Maybe Integer   
      , channelLastMessage :: Maybe MessageId         
                                                      
      , channelParentId    :: Maybe ParentId          
      , channelThreadMetadata :: Maybe ThreadMetadata 
      , channelThreadMember :: Maybe ThreadMember     
      }
  
  | ChannelPrivateThread
      { channelId          :: ChannelId               
      , channelGuild       :: GuildId                 
      , channelThreadName  :: Maybe T.Text            
      , channelUserRateLimitThread :: Maybe Integer   
      , channelLastMessage :: Maybe MessageId         
                                                      
      , channelParentId    :: Maybe ParentId          
      , channelThreadMetadata :: Maybe ThreadMetadata 
      , channelThreadMember :: Maybe ThreadMember     
      }
  
  | ChannelUnknownType
      { channelId          :: ChannelId 
      , channelJSON        :: Text      
      } deriving (Show, Read, Eq, Ord)

instance FromJSON Channel where
  parseJSON = withObject "Channel" $ \o -> do
    type' <- (o .: "type") :: Parser Int
    case type' of
      0 ->
        ChannelText  <$> o .:  "id"
                     <*> o .:? "guild_id" .!= 0
                     <*> o .:  "name"
                     <*> o .:  "position"
                     <*> o .:  "permission_overwrites"
                     <*> o .:  "rate_limit_per_user"
                     <*> o .:? "nsfw" .!= False
                     <*> o .:? "topic" .!= ""
                     <*> o .:? "last_message_id"
                     <*> o .:? "parent_id"
      1 ->
        ChannelDirectMessage <$> o .:  "id"
                             <*> o .:  "recipients"
                             <*> o .:? "last_message_id"
      2 ->
        ChannelVoice <$> o .:  "id"
                     <*> o .:? "guild_id" .!= 0
                     <*> o .:  "name"
                     <*> o .:  "position"
                     <*> o .:  "permission_overwrites"
                     <*> o .:? "nsfw" .!= False
                     <*> o .:  "bitrate"
                     <*> o .:  "user_limit"
                     <*> o .:? "parent_id"
      3 ->
        ChannelGroupDM <$> o .:  "id"
                       <*> o .:  "recipients"
                       <*> o .:? "last_message_id"
      4 ->
        ChannelGuildCategory <$> o .: "id"
                             <*> o .:? "guild_id" .!= 0
                             <*> o .:  "name"
                             <*> o .:  "position"
                             <*> o .:  "permission_overwrites"
      5 ->
        ChannelNews <$> o .:  "id"
                    <*> o .:? "guild_id" .!= 0
                    <*> o .:  "name"
                    <*> o .:  "position"
                    <*> o .:  "permission_overwrites"
                    <*> o .:? "nsfw" .!= False
                    <*> o .:? "topic" .!= ""
                    <*> o .:? "last_message_id"
                    <*> o .:? "parent_id"
      6 ->
        ChannelStorePage <$> o .:  "id"
                         <*> o .:? "guild_id" .!= 0
                         <*> o .:  "name"
                         <*> o .:  "position"
                         <*> o .:? "nsfw" .!= False
                         <*> o .:  "permission_overwrites"
                         <*> o .:? "parent_id"
      10 -> ChannelNewsThread <$> o.: "id"
                              <*> o .:? "guild_id" .!= 0
                              <*> o .:? "name"
                              <*> o .:? "rate_limit_per_user"
                              <*> o .:? "last_message_id"
                              <*> o .:? "parent_id"
                              <*> o .:? "thread_metadata"
                              <*> o .:? "member"
      11 -> ChannelPublicThread <$> o.: "id"
                                <*> o .:? "guild_id" .!= 0
                                <*> o .:? "name"
                                <*> o .:? "rate_limit_per_user"
                                <*> o .:? "last_message_id"
                                <*> o .:? "parent_id"
                                <*> o .:? "thread_metadata"
                                <*> o .:? "member"
      12 -> ChannelPrivateThread <$> o.: "id"
                                 <*> o .:? "guild_id" .!= 0
                                 <*> o .:? "name"
                                 <*> o .:? "rate_limit_per_user"
                                 <*> o .:? "last_message_id"
                                 <*> o .:? "parent_id"
                                 <*> o .:? "thread_metadata"
                                 <*> o .:? "member"
      13 ->
        ChannelStage <$> o .:  "id"
                     <*> o .:? "guild_id" .!= 0
                     <*> o .:  "id"
                     <*> o .:? "topic" .!= ""
      _ -> ChannelUnknownType <$> o .:  "id"
                              <*> pure (T.pack (show o))

instance ToJSON Channel where
  toJSON ChannelText{..} = objectFromMaybes
              [ "type" .== Number 0
              , "id" .== channelId
              , "guild_id" .== channelGuild
              , "name" .== channelName
              , "position" .== channelPosition
              , "rate_limit_per_user" .== channelUserRateLimit
              , "nsfw" .== channelNSFW
              , "permission_overwrites" .== channelPermissions
              , "topic" .== channelTopic
              , "last_message_id" .=? channelLastMessage
              , "parent_id" .== channelParentId
              ]
  toJSON ChannelNews{..} = objectFromMaybes
              [ "type" .== Number 5
              , "id" .== channelId
              , "guild_id" .== channelGuild
              , "name" .== channelName
              , "position" .== channelPosition
              , "permission_overwrites" .== channelPermissions
              , "nsfw" .== channelNSFW
              , "topic" .== channelTopic
              , "last_message_id" .=? channelLastMessage
              , "parent_id" .=? channelParentId
              ]
  toJSON ChannelStorePage{..} = objectFromMaybes
              [ "type" .== Number 6
              , "id" .== channelId
              , "guild_id" .== channelGuild
              , "name" .== channelName
              , "nsfw" .== channelNSFW
              , "position" .== channelPosition
              , "permission_overwrites" .== channelPermissions
              ]
  toJSON ChannelDirectMessage{..} = objectFromMaybes
              [ "type" .== Number 1
              , "id" .== channelId
              , "recipients" .== channelRecipients
              , "last_message_id" .=? channelLastMessage
              ]
  toJSON ChannelVoice{..} = objectFromMaybes
              [ "type" .== Number 2
              , "id" .== channelId
              , "guild_id" .== channelGuild
              , "name" .== channelName
              , "position" .== channelPosition
              , "nsfw" .== channelNSFW
              , "permission_overwrites" .== channelPermissions
              , "bitrate" .== channelBitRate
              , "user_limit" .== channelUserLimit
              ]
  toJSON ChannelGroupDM{..} = objectFromMaybes
              [ "type" .== Number 3
              , "id" .== channelId
              , "recipients" .== channelRecipients
              , "last_message_id" .=? channelLastMessage
              ]
  toJSON ChannelGuildCategory{..} = objectFromMaybes
              [ "type" .== Number 4
              , "id" .== channelId
              , "name" .== channelName
              , "guild_id" .== channelGuild
              ]
  toJSON ChannelStage{..} = objectFromMaybes
              [ "type" .== Number 13
              , "id" .== channelId
              , "guild_id" .== channelGuild
              , "channel_id" .== channelStageId
              , "topic" .== channelStageTopic
              ]
  toJSON ChannelNewsThread{..} = objectFromMaybes
              [ "type" .== Number 10
              , "id" .== channelId
              , "guild_id" .== channelGuild
              , "name" .=? channelThreadName
              , "rate_limit_per_user" .=? channelUserRateLimitThread
              , "last_message_id" .=? channelLastMessage
              , "parent_id" .== channelParentId
              , "thread_metadata" .=? channelThreadMetadata
              , "member" .=? channelThreadMember
              ]
  toJSON ChannelPublicThread{..} = objectFromMaybes
              [ "type" .== Number 11
              , "id" .== channelId
              , "guild_id" .== channelGuild
              , "name" .=? channelThreadName
              , "rate_limit_per_user" .=? channelUserRateLimitThread
              , "last_message_id" .=? channelLastMessage
              , "parent_id" .== channelParentId
              , "thread_metadata" .=? channelThreadMetadata
              , "member" .=? channelThreadMember
              ]
  toJSON ChannelPrivateThread{..} = objectFromMaybes
              [ "type" .== Number 12
              , "id" .== channelId
              , "guild_id" .== channelGuild
              , "name" .=? channelThreadName
              , "rate_limit_per_user" .=? channelUserRateLimitThread
              , "last_message_id" .=? channelLastMessage
              , "parent_id" .== channelParentId
              , "thread_metadata" .=? channelThreadMetadata
              , "member" .=? channelThreadMember
              ]
  toJSON ChannelUnknownType{..} = objectFromMaybes
              [ "id" .== channelId
              , "json" .== channelJSON
              ]


channelIsInGuild :: Channel -> Bool
channelIsInGuild c = case c of
        ChannelGuildCategory{} -> True
        ChannelText{} -> True
        ChannelVoice{} -> True
        ChannelNews{} -> True
        ChannelStorePage{} -> True
        ChannelNewsThread{} -> True
        ChannelPublicThread{} -> True
        ChannelPrivateThread{} -> True
        _ -> False


data Overwrite = Overwrite
  { overwriteId    :: Either RoleId UserId 
  , overwriteAllow :: T.Text               
  , overwriteDeny  :: T.Text               
  } deriving (Show, Read, Eq, Ord)

instance FromJSON Overwrite where
  parseJSON = withObject "Overwrite" $ \o -> do
    t <- o .: "type"
    i <- case (t :: Int) of
      0 -> Left <$> o .: "id"
      1 -> Right <$> o .: "id"
      _ -> error "Type field can only be 0 (role id) or 1 (user id)"
    Overwrite i
              <$> o .: "allow"
              <*> o .: "deny"

instance ToJSON Overwrite where
  toJSON Overwrite{..} = object
              [ ("id",     toJSON $ either unId unId overwriteId)
              , ("type",   toJSON (either (const 0) (const 1) overwriteId :: Int))
              , ("allow",  toJSON overwriteAllow)
              , ("deny",   toJSON overwriteDeny)
              ]


data ThreadMetadata = ThreadMetadata
 { threadMetadataArchived :: Bool 
 , threadMetadataAutoArchive :: Integer 
 , threadMetadataArchiveTime :: UTCTime 
 , threadMetadataLocked :: Bool 
 , threadMetadataInvitable :: Maybe Bool 
 , threadMetadataCreateTime :: Maybe UTCTime 
 } deriving (Show, Read, Eq, Ord)

instance FromJSON ThreadMetadata where
  parseJSON = withObject "ThreadMetadata" $ \o ->
    ThreadMetadata <$> o .:  "archived"
                   <*> o .:  "auto_archive_duration"
                   <*> o .:  "archive_timestamp"
                   <*> o .:  "locked"
                   <*> o .:? "invitable"
                   <*> o .:? "create_timestamp"

instance ToJSON ThreadMetadata where
  toJSON ThreadMetadata{..} = objectFromMaybes
              [ "archived" .== threadMetadataArchived
              , "auto_archive_duration" .== threadMetadataAutoArchive
              , "archive_timestamp" .== threadMetadataArchiveTime
              , "locked" .== threadMetadataLocked
              , "invitable" .=? threadMetadataInvitable
              , "create_timestamp" .== threadMetadataCreateTime
              ]


data ThreadMember = ThreadMember
 { threadMemberThreadId :: Maybe ChannelId 
 , threadMemberUserId   :: Maybe UserId    
 , threadMemberJoinTime :: UTCTime         
 , threadMemberFlags    :: Integer         
 } deriving (Show, Read, Eq, Ord)

instance FromJSON ThreadMember where
  parseJSON = withObject "ThreadMember" $ \o ->
    ThreadMember <$> o .:? "id"
                 <*> o .:? "user_id"
                 <*> o .:  "join_timestamp"
                 <*> o .:  "flags"

instance ToJSON ThreadMember where
  toJSON ThreadMember{..} = objectFromMaybes
              [ "id" .=? threadMemberThreadId
              , "user_id" .=? threadMemberUserId
              , "join_timestamp" .== threadMemberJoinTime
              , "flags" .== threadMemberFlags
              ]


data ThreadListSyncFields = ThreadListSyncFields
  { threadListSyncFieldsGuildId :: GuildId
  , threadListSyncFieldsChannelIds :: Maybe [ChannelId]
  , threadListSyncFieldsThreads :: [Channel]
  , threadListSyncFieldsThreadMembers :: [ThreadMember]
  } deriving (Show, Read, Eq, Ord)

instance FromJSON ThreadListSyncFields where
  parseJSON = withObject "ThreadListSyncFields" $ \o ->
    ThreadListSyncFields <$> o .: "guild_id"
                         <*> o .:? "channel_ids"
                         <*> o .:  "threads"
                         <*> o .:  "members"

data ThreadMembersUpdateFields = ThreadMembersUpdateFields
  { threadMembersUpdateFieldsThreadId :: ChannelId
  , threadMembersUpdateFieldsGuildId :: GuildId
  , threadMembersUpdateFieldsMemberCount :: Integer
  , threadMembersUpdateFieldsAddedMembers :: Maybe [ThreadMember]
  , threadMembersUpdateFieldsRemovedMembers :: Maybe [UserId]
  } deriving (Show, Read, Eq, Ord)

instance FromJSON ThreadMembersUpdateFields where
  parseJSON = withObject "ThreadMembersUpdateFields" $ \o ->
    ThreadMembersUpdateFields <$> o .:  "id"
                              <*> o .:  "guild_id"
                              <*> o .:  "member_count"
                              <*> o .:? "added_members"
                              <*> o .:? "removed_member_ids"


data Message = Message
  { messageId                 :: MessageId                
  , messageChannelId          :: ChannelId                
                                                          
  , messageGuildId            :: Maybe GuildId            
  , messageAuthor             :: User                     
                                                          
  , messageMember             :: Maybe GuildMember        
  , messageContent            :: Text                     
  , messageTimestamp          :: UTCTime                  
  , messageEdited             :: Maybe UTCTime            
  , messageTts                :: Bool                     
                                                          
  , messageEveryone           :: Bool                     
                                                          
  , messageMentions           :: [User]                   
                                                          
  , messageMentionRoles       :: [RoleId]                 
                                                          
  , messageAttachments        :: [Attachment]             
  , messageEmbeds             :: [Embed]                  
  , messageReactions          :: [MessageReaction]        
  , messageNonce              :: Maybe Nonce              
                                                          
  , messagePinned             :: Bool                     
  , messageWebhookId          :: Maybe WebhookId          
  , messageType               :: MessageType              
  , messageActivity           :: Maybe MessageActivity    
  , messageApplicationId      :: Maybe ApplicationId      
  , messageReference          :: Maybe MessageReference   
  , messageFlags              :: Maybe MessageFlags       
  , messageReferencedMessage  :: Maybe Message            
  , messageInteraction        :: Maybe MessageInteraction 
  , messageThread             :: Maybe Channel            
  , messageComponents         :: Maybe [ActionRow]        
  , messageStickerItems       :: Maybe [StickerItem]      
  } deriving (Show, Read, Eq, Ord)

instance FromJSON Message where
  parseJSON = withObject "Message" $ \o ->
    Message <$> o .:  "id"
            <*> o .:  "channel_id"
            <*> o .:? "guild_id" .!= Nothing
            <*> (do isW <- o .:? "webhook_id"
                    a <- o .: "author"
                    case isW :: Maybe WebhookId of
                      Nothing -> pure a
                      Just _ -> pure $ a { userIsWebhook = True })
            <*> o .:? "member"
            <*> o .:? "content" .!= ""
            <*> o .:? "timestamp" .!= epochTime
            <*> o .:? "edited_timestamp"
            <*> o .:? "tts" .!= False
            <*> o .:? "mention_everyone" .!= False
            <*> o .:? "mentions" .!= []
            <*> o .:? "mention_roles" .!= []
            <*> o .:? "attachments" .!= []
            <*> o .:  "embeds"
            <*> o .:? "reactions" .!= []
            <*> o .:? "nonce"
            <*> o .:? "pinned" .!= False
            <*> o .:? "webhook_id"
            <*> o .:  "type"
            <*> o .:? "activity"
            
            <*> o .:? "application_id"
            <*> o .:? "message_reference" .!= Nothing
            <*> o .:? "flags"
            <*> o .:? "referenced_message" .!= Nothing
            <*> o .:? "interaction"
            <*> o .:? "thread"
            <*> o .:? "components"
            <*> o .:? "sticker_items"


instance ToJSON Message where
  toJSON Message {..} = objectFromMaybes
      [ "id" .== messageId
      , "channel_id" .== messageChannelId
      , "guild_id" .=? messageGuildId
      , "author" .== messageAuthor
      , "member" .=? messageMember
      , "content" .== messageContent
      , "timestamp" .== messageTimestamp
      , "edited_timestamp" .=? messageEdited
      , "tts" .== messageTts
      , "mention_everyone" .== messageEveryone
      , "mentions" .== messageMentions
      , "mention_roles" .== messageMentionRoles
      , "attachments" .== messageAttachments
      , "embeds" .== messageEmbeds
      , "reactions" .== messageReactions
      , "nonce" .=? messageNonce
      , "pinned" .== messagePinned
      , "webhook_id" .=? messageWebhookId
      , "type" .== messageType
      , "activity" .=? messageActivity
      
      , "application_id" .=? messageApplicationId
      , "message_reference" .=? messageReference
      , "flags" .=? messageFlags
      , "referenced_message" .=? messageReferencedMessage
      , "interaction" .=? messageInteraction
      , "thread" .=? messageThread
      , "components" .=? messageComponents
      , "sticker_items" .=? messageStickerItems
      ]


data AllowedMentions = AllowedMentions
  { mentionEveryone    :: Bool     
  , mentionUsers       :: Bool     
  , mentionRoles       :: Bool     
  , mentionUserIds     :: [UserId] 
  , mentionRoleIds     :: [RoleId] 
  , mentionRepliedUser :: Bool     
  } deriving (Show, Read, Eq, Ord)

instance Default AllowedMentions where
  def = AllowedMentions { mentionEveryone    = False
                        , mentionUsers       = True
                        , mentionRoles       = True
                        , mentionUserIds     = []
                        , mentionRoleIds     = []
                        , mentionRepliedUser = True
                        }

instance ToJSON AllowedMentions where
  toJSON AllowedMentions{..} = object [
                                 "parse" .= [name :: T.Text | (name, True) <-
                                    [ ("everyone", mentionEveryone),
                                      ("users",    mentionUsers && null mentionUserIds),
                                      ("roles",    mentionRoles && null mentionRoleIds) ] ],
                                 
                                 
                                 "roles"        .= mentionRoleIds,
                                 "users"        .= mentionUserIds,
                                 "replied_user" .= mentionRepliedUser ]


data MessageReaction = MessageReaction
  { messageReactionCount :: Int
  , messageReactionMeIncluded :: Bool
  , messageReactionEmoji :: Emoji
  } deriving (Show, Read, Eq, Ord)

instance FromJSON MessageReaction where
  parseJSON = withObject "MessageReaction" $ \o ->
    MessageReaction <$> o .: "count"
                    <*> o .: "me"
                    <*> o .: "emoji"

instance ToJSON MessageReaction where
  toJSON MessageReaction{..} = objectFromMaybes
      [ "count" .== messageReactionCount
      , "me" .== messageReactionMeIncluded
      , "emoji" .== messageReactionEmoji
      ]


data Attachment = Attachment
  { attachmentId       :: AttachmentId     
  , attachmentFilename :: T.Text        
  , attachmentSize     :: Integer       
  , attachmentUrl      :: T.Text        
  , attachmentProxy    :: T.Text        
  , attachmentHeight   :: Maybe Integer 
  , attachmentWidth    :: Maybe Integer 
  } deriving (Show, Read, Eq, Ord)

instance FromJSON Attachment where
  parseJSON = withObject "Attachment" $ \o ->
    Attachment <$> o .:  "id"
               <*> o .:  "filename"
               <*> o .:  "size"
               <*> o .:  "url"
               <*> o .:  "proxy_url"
               <*> o .:? "height"
               <*> o .:? "width"

instance ToJSON Attachment where
  toJSON Attachment {..} = objectFromMaybes
      [ "id" .== attachmentId
      , "filename" .== attachmentFilename
      , "size" .== attachmentSize
      , "url" .== attachmentUrl
      , "proxy_url" .== attachmentProxy
      , "height" .=? attachmentHeight
      , "width" .=? attachmentWidth
      ]

newtype Nonce = Nonce T.Text
  deriving (Show, Read, Eq, Ord)

instance FromJSON Nonce where
  parseJSON (String nonce) = pure $ Nonce nonce
  parseJSON (Number nonce) = pure . Nonce . T.pack . show $ nonce
  parseJSON _ = empty

instance ToJSON Nonce where
  toJSON (Nonce t) = String t



data MessageReference = MessageReference
  { referenceMessageId      :: Maybe MessageId  
  , referenceChannelId      :: Maybe ChannelId  
  , referenceGuildId        :: Maybe GuildId    
  , failIfNotExists         :: Bool             
  } deriving (Show, Read, Eq, Ord)

instance FromJSON MessageReference where
  parseJSON = withObject "MessageReference" $ \o ->
    MessageReference <$> o .:? "message_id"
                     <*> o .:? "channel_id"
                     <*> o .:? "guild_id"
                     <*> o .:? "fail_if_not_exists" .!= True

instance ToJSON MessageReference where
  toJSON MessageReference{..} = objectFromMaybes
              [ "message_id" .== referenceMessageId
              , "channel_id" .== referenceChannelId
              , "guild_id" .== referenceGuildId
              , "fail_if_not_exists" .== failIfNotExists
              ]

instance Default MessageReference where
  def = MessageReference { referenceMessageId = Nothing
                         , referenceChannelId = Nothing
                         , referenceGuildId   = Nothing
                         , failIfNotExists    = False
                         }


data MessageType
  = MessageTypeDefault
  | MessageTypeRecipientAdd
  | MessageTypeRecipientRemove
  | MessageTypeCall
  | MessageTypeChannelNameChange
  | MessageTypeChannelIconChange
  | MessageTypeChannelPinnedMessage
  | MessageTypeGuildMemberJoin
  | MessageTypeUserPremiumGuildSubscription
  | MessageTypeUserPremiumGuildSubscriptionTier1
  | MessageTypeUserPremiumGuildSubscriptionTier2
  | MessageTypeUserPremiumGuildSubscriptionTier3
  | MessageTypeChannelFollowAdd
  | MessageTypeGuildDiscoveryDisqualified
  | MessageTypeGuildDiscoveryRequalified
  | MessageTypeGuildDiscoveryGracePeriodInitialWarning
  | MessageTypeGuildDiscoveryGracePeriodFinalWarning
  | MessageTypeThreadCreated
  | MessageTypeReply
  | MessageTypeChatInputCommand
  | MessageTypeThreadStarterMessage
  | MessageTypeGuildInviteReminder
  | MessageTypeContextMenuCommand
  deriving (Show, Read, Data, Eq, Ord)

instance InternalSigmacordEnum MessageType where
  SigmacordTypeStartValue = MessageTypeDefault
  fromSigmacordType MessageTypeDefault = 0
  fromSigmacordType MessageTypeRecipientAdd = 1
  fromSigmacordType MessageTypeRecipientRemove = 2
  fromSigmacordType MessageTypeCall = 3
  fromSigmacordType MessageTypeChannelNameChange = 4
  fromSigmacordType MessageTypeChannelIconChange = 5
  fromSigmacordType MessageTypeChannelPinnedMessage = 6
  fromSigmacordType MessageTypeGuildMemberJoin = 7
  fromSigmacordType MessageTypeUserPremiumGuildSubscription = 8
  fromSigmacordType MessageTypeUserPremiumGuildSubscriptionTier1 = 9
  fromSigmacordType MessageTypeUserPremiumGuildSubscriptionTier2 = 10
  fromSigmacordType MessageTypeUserPremiumGuildSubscriptionTier3 = 11
  fromSigmacordType MessageTypeChannelFollowAdd = 12
  fromSigmacordType MessageTypeGuildDiscoveryDisqualified = 14
  fromSigmacordType MessageTypeGuildDiscoveryRequalified = 15
  fromSigmacordType MessageTypeGuildDiscoveryGracePeriodInitialWarning = 16
  fromSigmacordType MessageTypeGuildDiscoveryGracePeriodFinalWarning = 17
  fromSigmacordType MessageTypeThreadCreated = 18
  fromSigmacordType MessageTypeReply = 19
  fromSigmacordType MessageTypeChatInputCommand = 20
  fromSigmacordType MessageTypeThreadStarterMessage = 21
  fromSigmacordType MessageTypeGuildInviteReminder = 22
  fromSigmacordType MessageTypeContextMenuCommand = 23

instance ToJSON MessageType where
  toJSON = toJSON . fromSigmacordType

instance FromJSON MessageType where
  parseJSON = SigmacordTypeParseJSON "MessageType"

data MessageActivity = MessageActivity
  { messageActivityType :: MessageActivityType
  , messageActivityPartyId :: Maybe T.Text
  }
  deriving (Show, Read, Data, Eq, Ord)

instance FromJSON MessageActivity where
  parseJSON = withObject "MessageActivity" $ \o ->
    MessageActivity <$> o .:   "type"
                     <*> o .:? "party_id"

instance ToJSON MessageActivity where
  toJSON MessageActivity{..} = objectFromMaybes
              [ "type" .== messageActivityType
              , "party_id" .=? messageActivityPartyId
              ]

data MessageActivityType
  = MessageActivityTypeJoin 
  | MessageActivityTypeSpectate 
  | MessageActivityTypeListen 
  | MessageActivityTypeJoinRequest 
  deriving (Show, Read, Data, Eq, Ord)

instance InternalSigmacordEnum MessageActivityType where
  SigmacordTypeStartValue = MessageActivityTypeJoin
  fromSigmacordType MessageActivityTypeJoin = 1
  fromSigmacordType MessageActivityTypeSpectate = 2
  fromSigmacordType MessageActivityTypeListen = 3
  fromSigmacordType MessageActivityTypeJoinRequest = 4

instance ToJSON MessageActivityType where
  toJSON = toJSON . fromSigmacordType

instance FromJSON MessageActivityType where
  parseJSON = SigmacordTypeParseJSON "MessageActivityType"


data MessageFlag =
    MessageFlagCrossposted
  | MessageFlagIsCrosspost
  | MessageFlagSupressEmbeds
  | MessageFlagSourceMessageDeleted
  | MessageFlagUrgent
  | MessageFlagHasThread
  | MessageFlagEphemeral
  | MessageFlagLoading
  | MessageFlagFailedToMentionRollesInThread
  deriving (Show, Read, Eq, Data, Ord)

newtype MessageFlags = MessageFlags [MessageFlag]
  deriving (Show, Read, Eq, Ord)

instance InternalSigmacordEnum MessageFlag where
  SigmacordTypeStartValue = MessageFlagCrossposted
  fromSigmacordType MessageFlagCrossposted = 1 `shift` 0
  fromSigmacordType MessageFlagIsCrosspost = 1 `shift` 1
  fromSigmacordType MessageFlagSupressEmbeds = 1 `shift` 2
  fromSigmacordType MessageFlagSourceMessageDeleted = 1 `shift` 3
  fromSigmacordType MessageFlagUrgent = 1 `shift` 4
  fromSigmacordType MessageFlagHasThread = 1 `shift` 5
  fromSigmacordType MessageFlagEphemeral = 1 `shift` 6
  fromSigmacordType MessageFlagLoading = 1 `shift` 7
  fromSigmacordType MessageFlagFailedToMentionRollesInThread = 1 `shift` 8

instance ToJSON MessageFlags where
  toJSON (MessageFlags fs) = Number $ fromInteger $ fromIntegral $ foldr (.|.) 0 (fromSigmacordType <$> fs)



instance FromJSON MessageFlags where
  parseJSON = withScientific "MessageFlags" $ \s ->
      let i = round s
          
          
          
      in return $ MessageFlags (snd <$> filter (\(i',_) -> i .&. i' == i') SigmacordTypeTable)


data MessageInteraction = MessageInteraction
  { messageInteractionId :: InteractionId 
  , messageInteractionType :: Integer 
  , messageInteractionName :: T.Text 
  , messageInteractionUser :: User 
  } deriving (Show, Read, Eq, Ord)

instance ToJSON MessageInteraction where
  toJSON MessageInteraction{..} = objectFromMaybes
              [ "id"   .== messageInteractionId
              , "type" .== messageInteractionType
              , "name" .== messageInteractionName
              , "user" .== messageInteractionUser
              ]

instance FromJSON MessageInteraction where
  parseJSON = withObject "MessageInteraction" $ \o ->
    MessageInteraction <$> o .: "id"
                       <*> o .: "type"
                       <*> o .: "name"
                       <*> o .: "user"
