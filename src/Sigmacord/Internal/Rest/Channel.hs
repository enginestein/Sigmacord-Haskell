{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


module Sigmacord.Internal.Rest.Channel
  ( ChannelRequest(..)
  , MessageDetailedOpts(..)
  , AllowedMentions(..)
  , ReactionTiming(..)
  , MessageTiming(..)
  , ChannelInviteOpts(..)
  , ModifyChannelOpts(..)
  , ChannelPermissionsOpts(..)
  , GroupDMAddRecipientOpts(..)
  , StartThreadOpts(..)
  , StartThreadNoMessageOpts(..)
  , ListThreads(..)
  ) where


import Data.Aeson
import Data.Default (Default, def)
import Data.Emoji (unicodeByName)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Client (RequestBody (RequestBodyBS))
import Network.HTTP.Client.MultipartFormData (partFileRequestBody, partBS)
import Network.HTTP.Req ((/:), (/~))
import qualified Network.HTTP.Req as R

import Sigmacord.Internal.Rest.Prelude
import Sigmacord.Internal.Types
import Control.Monad (join)

instance Request (ChannelRequest a) where
  majorRoute = channelMajorRoute
  jsonRequest = channelJsonRequest


data ChannelRequest a where
  
  GetChannel                :: ChannelId -> ChannelRequest Channel
  
  ModifyChannel             :: ChannelId -> ModifyChannelOpts -> ChannelRequest Channel
  
  DeleteChannel             :: ChannelId -> ChannelRequest Channel
  
  GetChannelMessages        :: ChannelId -> (Int, MessageTiming) -> ChannelRequest [Message]
  
  GetChannelMessage         :: (ChannelId, MessageId) -> ChannelRequest Message
  
  CreateMessage             :: ChannelId -> T.Text -> ChannelRequest Message
  
  CreateMessageDetailed     :: ChannelId -> MessageDetailedOpts -> ChannelRequest Message
  
  CreateReaction            :: (ChannelId, MessageId) -> T.Text -> ChannelRequest ()
  
  DeleteOwnReaction         :: (ChannelId, MessageId) -> T.Text -> ChannelRequest ()
  
  DeleteUserReaction        :: (ChannelId, MessageId) -> UserId -> T.Text -> ChannelRequest ()
  
  DeleteSingleReaction      :: (ChannelId, MessageId) -> T.Text -> ChannelRequest ()
  
  GetReactions              :: (ChannelId, MessageId) -> T.Text -> (Int, ReactionTiming) -> ChannelRequest [User]
  
  DeleteAllReactions        :: (ChannelId, MessageId) -> ChannelRequest ()
  
  EditMessage               :: (ChannelId, MessageId) -> MessageDetailedOpts
                                                      -> ChannelRequest Message
  
  DeleteMessage             :: (ChannelId, MessageId) -> ChannelRequest ()
  
  BulkDeleteMessage         :: (ChannelId, [MessageId]) -> ChannelRequest ()
  
  EditChannelPermissions    :: ChannelId -> Either RoleId UserId -> ChannelPermissionsOpts -> ChannelRequest ()
  
  GetChannelInvites         :: ChannelId -> ChannelRequest Object
  
  CreateChannelInvite       :: ChannelId -> ChannelInviteOpts -> ChannelRequest Invite
  
  DeleteChannelPermission   :: ChannelId -> Either RoleId UserId -> ChannelRequest ()
  
  TriggerTypingIndicator    :: ChannelId -> ChannelRequest ()
  
  GetPinnedMessages         :: ChannelId -> ChannelRequest [Message]
  
  AddPinnedMessage          :: (ChannelId, MessageId) -> ChannelRequest ()
  
  DeletePinnedMessage       :: (ChannelId, MessageId) -> ChannelRequest ()
  
  GroupDMAddRecipient       :: ChannelId -> GroupDMAddRecipientOpts -> ChannelRequest ()
  
  GroupDMRemoveRecipient    :: ChannelId -> UserId -> ChannelRequest ()
  
  StartThreadFromMessage    :: ChannelId -> MessageId -> StartThreadOpts -> ChannelRequest Channel
  
  StartThreadNoMessage      :: ChannelId -> StartThreadNoMessageOpts -> ChannelRequest Channel
  
  JoinThread                :: ChannelId -> ChannelRequest ()
  
  AddThreadMember           :: ChannelId -> UserId -> ChannelRequest ()
  
  LeaveThread               :: ChannelId -> ChannelRequest ()
  
  RemoveThreadMember        :: ChannelId -> UserId -> ChannelRequest ()
  
  GetThreadMember           :: ChannelId -> UserId -> ChannelRequest ThreadMember
  
  ListThreadMembers         :: ChannelId -> ChannelRequest [ThreadMember]
  
  
  
  
  ListPublicArchivedThreads :: ChannelId -> (Maybe UTCTime, Maybe Integer) -> ChannelRequest ListThreads
  
  
  
  
  ListPrivateArchivedThreads :: ChannelId -> (Maybe UTCTime, Maybe Integer) -> ChannelRequest ListThreads
  
  
  
  
  ListJoinedPrivateArchivedThreads :: ChannelId -> (Maybe UTCTime, Maybe Integer) -> ChannelRequest ListThreads



data MessageDetailedOpts = MessageDetailedOpts
  { 
    messageDetailedContent                  :: T.Text
  , 
    messageDetailedTTS                      :: Bool
  , 
    messageDetailedEmbeds                   :: Maybe [CreateEmbed]
  , 
    messageDetailedFile                     :: Maybe (T.Text, B.ByteString)
  , 
    messageDetailedAllowedMentions          :: Maybe AllowedMentions
  , 
    messageDetailedReference                :: Maybe MessageReference
  , 
    messageDetailedComponents               :: Maybe [ActionRow]
  , 
    messageDetailedStickerIds               :: Maybe [StickerId]
  } deriving (Show, Read, Eq, Ord)

instance Default MessageDetailedOpts where
  def = MessageDetailedOpts { messageDetailedContent         = ""
                            , messageDetailedTTS             = False
                            , messageDetailedEmbeds          = Nothing
                            , messageDetailedFile            = Nothing
                            , messageDetailedAllowedMentions = Nothing
                            , messageDetailedReference       = Nothing
                            , messageDetailedComponents      = Nothing
                            , messageDetailedStickerIds      = Nothing
                            }


data ReactionTiming = BeforeReaction MessageId
                    | AfterReaction MessageId
                    | LatestReaction
  deriving (Show, Read, Eq, Ord)

reactionTimingToQuery :: ReactionTiming -> R.Option 'R.Https
reactionTimingToQuery t = case t of
  (BeforeReaction snow) -> "before" R.=: show snow
  (AfterReaction snow) -> "after"  R.=: show snow
  LatestReaction -> mempty




data MessageTiming = AroundMessage MessageId
                   | BeforeMessage MessageId
                   | AfterMessage MessageId
                   | LatestMessages
  deriving (Show, Read, Eq, Ord)

messageTimingToQuery :: MessageTiming -> R.Option 'R.Https
messageTimingToQuery t = case t of
  (AroundMessage snow) -> "around" R.=: show snow
  (BeforeMessage snow) -> "before" R.=: show snow
  (AfterMessage snow) -> "after"  R.=: show snow
  LatestMessages -> mempty


data ChannelInviteOpts = ChannelInviteOpts
  { 
    channelInviteOptsMaxAgeSeconds          :: Maybe Integer
  , 
    channelInviteOptsMaxUsages              :: Maybe Integer
  , 
    channelInviteOptsIsTemporary            :: Maybe Bool
  , 
    
    channelInviteOptsDontReuseSimilarInvite :: Maybe Bool
  } deriving (Show, Read, Eq, Ord)

instance ToJSON ChannelInviteOpts where
  toJSON ChannelInviteOpts{..} = objectFromMaybes
                         ["max_age" .=? channelInviteOptsMaxAgeSeconds,
                          "max_uses" .=? channelInviteOptsMaxUsages,
                          "temporary" .=? channelInviteOptsIsTemporary,
                          "unique" .=? channelInviteOptsDontReuseSimilarInvite ]


data ModifyChannelOpts = ModifyChannelOpts
  { 
    modifyChannelName                 :: Maybe T.Text
  , 
    modifyChannelPosition             :: Maybe Integer
  , 
    modifyChannelTopic                :: Maybe T.Text
  , 
    modifyChannelNSFW                 :: Maybe Bool
  , 
    
    modifyChannelBitrate              :: Maybe Integer
  , 
    
    
    modifyChannelUserRateLimit        :: Maybe Integer
  , 
    modifyChannelUserLimit            :: Maybe Integer
  , 
    modifyChannelPermissionOverwrites :: Maybe [Overwrite]
  , 
    modifyChannelParentId             :: Maybe ChannelId
  , 
    modifyChannelDefaultAutoArchive   :: Maybe Integer
  , 
    modifyChannelThreadArchived       :: Maybe Bool
  , 
    
    modifyChannelThreadAutoArchive    :: Maybe Integer
  , 
    
    modifyChannelThreadLocked         :: Maybe Bool
  , 
    
    modifyChannelThreadInvitable     :: Maybe Bool
  } deriving (Show, Read, Eq, Ord)

instance Default ModifyChannelOpts where
  def = ModifyChannelOpts Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance ToJSON ModifyChannelOpts where
  toJSON ModifyChannelOpts{..} = objectFromMaybes
               ["name" .=? modifyChannelName,
                "position" .=? modifyChannelPosition,
                "topic" .=? modifyChannelTopic,
                "nsfw" .=? modifyChannelNSFW,
                "bitrate" .=? modifyChannelBitrate,
                "rate_limit_per_user" .=? modifyChannelUserRateLimit,
                "user_limit" .=? modifyChannelUserLimit,
                "permission_overwrites" .=? modifyChannelPermissionOverwrites,
                "parent_id" .=? modifyChannelParentId,
                "default_auto_archive_duration" .=? modifyChannelDefaultAutoArchive,
                "archived" .=? modifyChannelThreadArchived,
                "auto_archive_duration" .=? modifyChannelThreadAutoArchive,
                "locked" .=? modifyChannelThreadLocked,
                "invitable" .=? modifyChannelThreadInvitable ]







data ChannelPermissionsOpts = ChannelPermissionsOpts
  { 
    channelPermissionsOptsAllow :: Integer
  , 
    channelPermissionsOptsDeny :: Integer
  } deriving (Show, Read, Eq, Ord)




data GroupDMAddRecipientOpts = GroupDMAddRecipientOpts
  { 
    groupDMAddRecipientUserToAdd :: UserId
  , 
    groupDMAddRecipientUserToAddNickName :: T.Text
  , 
    
    groupDMAddRecipientGDMJoinAccessToken :: T.Text
  } deriving (Show, Read, Eq, Ord)


data StartThreadOpts = StartThreadOpts
  { 
    startThreadName :: T.Text
  , 
    
    
    startThreadAutoArchive :: Maybe Integer
  , 
    
    startThreadRateLimit :: Maybe Integer
  } deriving (Show, Read, Eq, Ord)

instance ToJSON StartThreadOpts where
  toJSON StartThreadOpts{..} = objectFromMaybes
      [ "name" .== startThreadName
      , "auto_archive_duration" .=? startThreadAutoArchive
      , "rate_limit_per_user" .=? startThreadRateLimit
      ]


data StartThreadNoMessageOpts = StartThreadNoMessageOpts
  { 
    startThreadNoMessageBaseOpts :: StartThreadOpts
  , 
    
    
    
    startThreadNoMessageType :: Integer
  , 
    
    startThreadNoMessageInvitable :: Maybe Bool
  } deriving (Show, Read, Eq, Ord)

instance ToJSON StartThreadNoMessageOpts where
  toJSON StartThreadNoMessageOpts{..} = objectFromMaybes
      [ "name" .== startThreadName startThreadNoMessageBaseOpts
      , "auto_archive_duration" .=? startThreadAutoArchive startThreadNoMessageBaseOpts
      , "rate_limit_per_user" .=? startThreadRateLimit startThreadNoMessageBaseOpts
      , "type" .== startThreadNoMessageType
      , "invitable" .=? startThreadNoMessageInvitable
      ]



data ListThreads = ListThreads
  { 
    listThreadsThreads :: [Channel]
  , 
    
    listThreadsMembers :: [ThreadMember]
  ,  
    listThreadsHasMore :: Bool
  } deriving (Show, Read, Eq, Ord)

instance ToJSON ListThreads where
  toJSON ListThreads{..} = object
    [ ("threads", toJSON listThreadsThreads)
    , ("members", toJSON listThreadsMembers)
    , ("has_more", toJSON listThreadsHasMore)
    ]

instance FromJSON ListThreads where
  parseJSON = withObject "ListThreads" $ \o ->
    ListThreads <$> o .: "threads"
                <*> o .: "members"
                <*> o .: "has_more"

channelMajorRoute :: ChannelRequest a -> String
channelMajorRoute c = case c of
  (GetChannel chan) ->                       "get_chan " <> show chan
  (ModifyChannel chan _) ->                  "mod_chan " <> show chan
  (DeleteChannel chan) ->                    "mod_chan " <> show chan
  (GetChannelMessages chan _) ->                  "msg " <> show chan
  (GetChannelMessage (chan, _)) ->            "get_msg " <> show chan
  (CreateMessage chan _) ->                       "msg " <> show chan
  (CreateMessageDetailed chan _) ->               "msg " <> show chan
  (CreateReaction (chan, _) _) ->           "add_react " <> show chan
  (DeleteOwnReaction (chan, _) _) ->            "react " <> show chan
  (DeleteUserReaction (chan, _) _ _) ->         "react " <> show chan
  (DeleteSingleReaction (chan, _) _) ->         "react " <> show chan
  (GetReactions (chan, _) _ _) ->               "react " <> show chan
  (DeleteAllReactions (chan, _)) ->             "react " <> show chan
  (EditMessage (chan, _) _) ->                "get_msg " <> show chan
  (DeleteMessage (chan, _)) ->                "get_msg " <> show chan
  (BulkDeleteMessage (chan, _)) ->           "del_msgs " <> show chan
  (EditChannelPermissions chan _ _) ->          "perms " <> show chan
  (GetChannelInvites chan) ->                 "invites " <> show chan
  (CreateChannelInvite chan _) ->             "invites " <> show chan
  (DeleteChannelPermission chan _) ->           "perms " <> show chan
  (TriggerTypingIndicator chan) ->                "tti " <> show chan
  (GetPinnedMessages chan) ->                    "pins " <> show chan
  (AddPinnedMessage (chan, _)) ->                 "pin " <> show chan
  (DeletePinnedMessage (chan, _)) ->              "pin " <> show chan
  (GroupDMAddRecipient chan _) ->             "groupdm " <> show chan
  (GroupDMRemoveRecipient chan _) ->          "groupdm " <> show chan
  (StartThreadFromMessage chan _ _) ->         "thread " <> show chan
  (StartThreadNoMessage chan _) ->           "thread " <> show chan
  (JoinThread chan) ->                         "thread " <> show chan
  (AddThreadMember chan _) ->                  "thread " <> show chan
  (LeaveThread chan) ->                        "thread " <> show chan
  (RemoveThreadMember chan _) ->               "thread " <> show chan
  (GetThreadMember chan _) ->                  "thread " <> show chan
  (ListThreadMembers chan) ->                  "thread " <> show chan
  (ListPublicArchivedThreads chan _) ->        "thread " <> show chan
  (ListPrivateArchivedThreads chan _) ->       "thread " <> show chan
  (ListJoinedPrivateArchivedThreads chan _) -> "thread " <> show chan

cleanupEmoji :: T.Text -> T.Text
cleanupEmoji emoji =
  let noAngles = T.replace "<" "" (T.replace ">" "" emoji)
      byName = T.pack <$> unicodeByName (T.unpack (T.replace ":" "" emoji))
  in case (byName, T.stripPrefix ":" noAngles) of
    (Just e, _) -> e
    (_, Just a) -> "custom:" <> a
    (_, Nothing) -> noAngles

channels :: R.Url 'R.Https
channels = baseUrl /: "channels"

channelJsonRequest :: ChannelRequest r -> JsonRequest
channelJsonRequest c = case c of
  (GetChannel chan) ->
      Get (channels /~ chan) mempty

  (ModifyChannel chan patch) ->
      Patch (channels /~ chan) (pure (R.ReqBodyJson patch)) mempty

  (DeleteChannel chan) ->
      Delete (channels /~ chan) mempty

  (GetChannelMessages chan (n,timing)) ->
      let n' = max 1 (min 100 n)
          options = "limit" R.=: n' <> messageTimingToQuery timing
      in Get (channels /~ chan /: "messages") options

  (GetChannelMessage (chan, msg)) ->
      Get (channels /~ chan /: "messages" /~ msg) mempty

  (CreateMessage chan msg) ->
      let content = ["content" .= msg]
          body = pure $ R.ReqBodyJson $ object content
      in Post (channels /~ chan /: "messages") body mempty

  (CreateMessageDetailed chan msgOpts) ->
    let fileUpload = messageDetailedFile msgOpts
        filePart =
          ( case fileUpload of
              Nothing -> []
              Just f ->
                [ partFileRequestBody
                    "file"
                    (T.unpack $ fst f)
                    (RequestBodyBS $ snd f)
                ]
          )
            ++ join (maybe [] (maybeEmbed . Just <$>) (messageDetailedEmbeds msgOpts))

        payloadData =  objectFromMaybes $
                        [ "content" .== messageDetailedContent msgOpts
                        , "tts"     .== messageDetailedTTS msgOpts ] ++
                        [ "embeds" .=? ((createEmbed <$>) <$> messageDetailedEmbeds msgOpts)
                        , "allowed_mentions" .=? messageDetailedAllowedMentions msgOpts
                        , "message_reference" .=? messageDetailedReference msgOpts
                        , "components" .=? messageDetailedComponents msgOpts
                        , "sticker_ids" .=? messageDetailedStickerIds msgOpts
                        ]
        payloadPart = partBS "payload_json" $ BL.toStrict $ encode payloadData

        body = R.reqBodyMultipart (payloadPart : filePart)
      in Post (channels /~ chan /: "messages") body mempty

  (CreateReaction (chan, msgid) emoji) ->
      let e = cleanupEmoji emoji
      in Put (channels /~ chan /: "messages" /~ msgid /: "reactions" /: e /: "@me" )
             R.NoReqBody mempty

  (DeleteOwnReaction (chan, msgid) emoji) ->
      let e = cleanupEmoji emoji
      in Delete (channels /~ chan /: "messages" /~ msgid /: "reactions" /: e /: "@me" ) mempty

  (DeleteUserReaction (chan, msgid) uID emoji) ->
      let e = cleanupEmoji emoji
      in Delete (channels /~ chan /: "messages" /~ msgid /: "reactions" /: e /~ uID ) mempty

  (DeleteSingleReaction (chan, msgid) emoji) ->
    let e = cleanupEmoji emoji
    in Delete (channels /~ chan /: "messages" /~ msgid /: "reactions" /: e) mempty

  (GetReactions (chan, msgid) emoji (n, timing)) ->
      let e = cleanupEmoji emoji
          n' = max 1 (min 100 n)
          options = "limit" R.=: n' <> reactionTimingToQuery timing
      in Get (channels /~ chan /: "messages" /~ msgid /: "reactions" /: e) options

  (DeleteAllReactions (chan, msgid)) ->
      Delete (channels /~ chan /: "messages" /~ msgid /: "reactions" ) mempty

  
  (EditMessage (chan, msg) msgOpts) ->
    let fileUpload = messageDetailedFile msgOpts
        filePart =
          ( case fileUpload of
              Nothing -> []
              Just f ->
                [ partFileRequestBody
                    "file"
                    (T.unpack $ fst f)
                    (RequestBodyBS $ snd f)
                ]
          )
            ++ join (maybe [] (maybeEmbed . Just <$>) (messageDetailedEmbeds msgOpts))

        payloadData =  objectFromMaybes $
                        [ "content" .== messageDetailedContent msgOpts
                        , "tts"     .== messageDetailedTTS msgOpts ] ++
                        [ "embeds" .=? ((createEmbed <$>) <$> messageDetailedEmbeds msgOpts)
                        , "allowed_mentions" .=? messageDetailedAllowedMentions msgOpts
                        , "message_reference" .=? messageDetailedReference msgOpts
                        , "components" .=? messageDetailedComponents msgOpts
                        , "sticker_ids" .=? messageDetailedStickerIds msgOpts
                        ]
        payloadPart = partBS "payload_json" $ BL.toStrict $ encode payloadData

        body = R.reqBodyMultipart (payloadPart : filePart)
      in Patch (channels /~ chan /: "messages" /~ msg) body mempty

  (DeleteMessage (chan, msg)) ->
      Delete (channels /~ chan /: "messages" /~ msg) mempty

  (BulkDeleteMessage (chan, msgs)) ->
      let body = pure . R.ReqBodyJson $ object ["messages" .= msgs]
      in Post (channels /~ chan /: "messages" /: "bulk-delete") body mempty

  (EditChannelPermissions chan overwriteId (ChannelPermissionsOpts a d)) ->
      let body = R.ReqBodyJson $ object [("type", toJSON (either (const 0) (const 1) overwriteId :: Int))
                                        ,("allow", toJSON a)
                                        ,("deny", toJSON d)]
      in Put (channels /~ chan /: "permissions" /~ either unId unId overwriteId) body mempty

  (GetChannelInvites chan) ->
      Get (channels /~ chan /: "invites") mempty

  (CreateChannelInvite chan patch) ->
      Post (channels /~ chan /: "invites") (pure (R.ReqBodyJson patch)) mempty

  (DeleteChannelPermission chan overwriteId) ->
      Delete (channels /~ chan /: "permissions" /~ either unId unId overwriteId) mempty

  (TriggerTypingIndicator chan) ->
      Post (channels /~ chan /: "typing") (pure R.NoReqBody) mempty

  (GetPinnedMessages chan) ->
      Get (channels /~ chan /: "pins") mempty

  (AddPinnedMessage (chan, msg)) ->
      Put (channels /~ chan /: "pins" /~ msg) R.NoReqBody mempty

  (DeletePinnedMessage (chan, msg)) ->
      Delete (channels /~ chan /: "pins" /~ msg) mempty

  (GroupDMAddRecipient chan (GroupDMAddRecipientOpts uid nick tok)) ->
      Put (channels /~ chan /~ chan /: "recipients" /~ uid)
          (R.ReqBodyJson (object [ ("access_token", toJSON tok)
                                 , ("nick", toJSON nick)]))
          mempty

  (GroupDMRemoveRecipient chan userid) ->
      Delete (channels /~ chan /~ chan /: "recipients" /~ userid) mempty

  (StartThreadFromMessage chan mid sto) ->
      Post (channels /~ chan /: "messages" /~ mid /: "threads")
           (pure $ R.ReqBodyJson $ toJSON sto)
           mempty

  (StartThreadNoMessage chan sto) ->
      Post (channels /~ chan /: "messages" /: "threads")
           (pure $ R.ReqBodyJson $ toJSON sto)
           mempty

  (JoinThread chan) ->
      Put (channels /~ chan /: "thread-members" /: "@me")
          R.NoReqBody mempty

  (AddThreadMember chan uid) ->
      Put (channels /~ chan /: "thread-members" /~ uid)
          R.NoReqBody mempty

  (LeaveThread chan) ->
      Delete (channels /~ chan /: "thread-members" /: "@me")
          mempty

  (RemoveThreadMember chan uid) ->
      Delete (channels /~ chan /: "thread-members" /~ uid)
          mempty

  (GetThreadMember chan uid) ->
      Get (channels /~ chan /: "thread-members" /~ uid)
          mempty

  (ListThreadMembers chan) ->
      Get (channels /~ chan /: "thread-members")
          mempty

  (ListPublicArchivedThreads chan (time, lim)) ->
      Get (channels /~ chan /: "threads" /: "archived" /: "public")
          (maybe mempty ("limit" R.=:) lim <> maybe mempty ("before" R.=:) time)

  (ListPrivateArchivedThreads chan (time, lim)) ->
      Get (channels /~ chan /: "threads" /: "archived" /: "private")
          (maybe mempty ("limit" R.=:) lim <> maybe mempty ("before" R.=:) time)

  (ListJoinedPrivateArchivedThreads chan (time, lim)) ->
      Get (channels /~ chan /: "users" /: "@me" /: "threads" /: "archived" /: "private")
          (maybe mempty ("limit" R.=:) lim <> maybe mempty ("before" R.=:) time)
