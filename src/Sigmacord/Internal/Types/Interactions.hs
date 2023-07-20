{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Sigmacord.Internal.Types.Interactions
  ( Interaction (..),
    ComponentData (..),
    ApplicationCommandData (..),
    OptionsData (..),
    OptionDataSubcommandOrGroup (..),
    OptionDataSubcommand (..),
    OptionDataValue (..),
    InteractionToken,
    ResolvedData (..),
    MemberOrUser (..),
    InteractionResponse (..),
    interactionResponseBasic,
    InteractionResponseAutocomplete (..),
    InteractionResponseMessage (..),
    interactionResponseMessageBasic,
    InteractionResponseMessageFlags (..),
    InteractionResponseMessageFlag (..),
    InteractionResponseModalData (..),
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (join)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Bits (Bits (shift, (.|.)))
import Data.Foldable (Foldable (toList))
import qualified Data.Text as T
import Sigmacord.Internal.Types.ApplicationCommands (Choice, Number)
import Sigmacord.Internal.Types.Channel (AllowedMentions, Attachment, Message)
import Sigmacord.Internal.Types.Components (ActionRow, TextInput)
import Sigmacord.Internal.Types.Embed (CreateEmbed, createEmbed)
import Sigmacord.Internal.Types.Prelude (ApplicationCommandId, ApplicationId, ChannelId, GuildId, InteractionId, InteractionToken, MessageId, RoleId, Snowflake, UserId, objectFromMaybes, (.=?))
import Sigmacord.Internal.Types.User (GuildMember, User)


data Interaction
  = InteractionComponent
      { 
        interactionId :: InteractionId,
        
        interactionApplicationId :: ApplicationId,
        
        componentData :: ComponentData,
        
        interactionGuildId :: Maybe GuildId,
        
        interactionChannelId :: Maybe ChannelId,
        
        interactionUser :: MemberOrUser,
        
        interactionToken :: InteractionToken,
        
        interactionVersion :: Int,
        
        interactionMessage :: Message,
        
        interactionPermissions :: Maybe T.Text,
        
        interactionLocale :: T.Text,
        
        interactionGuildLocale :: Maybe T.Text
      }
  | InteractionPing
      { 
        interactionId :: InteractionId,
        
        interactionApplicationId :: ApplicationId,
        
        interactionToken :: InteractionToken,
        
        interactionVersion :: Int,
        
        interactionPermissions :: Maybe T.Text
      }
  | InteractionApplicationCommand
      { 
        interactionId :: InteractionId,
        
        interactionApplicationId :: ApplicationId,
        
        applicationCommandData :: ApplicationCommandData,
        
        interactionGuildId :: Maybe GuildId,
        
        interactionChannelId :: Maybe ChannelId,
        
        interactionUser :: MemberOrUser,
        
        interactionToken :: InteractionToken,
        
        interactionVersion :: Int,
        
        interactionPermissions :: Maybe T.Text,
        
        interactionLocale :: T.Text,
        
        interactionGuildLocale :: Maybe T.Text
      }
  | InteractionApplicationCommandAutocomplete
      { 
        interactionId :: InteractionId,
        
        interactionApplicationId :: ApplicationId,
        
        applicationCommandData :: ApplicationCommandData,
        
        interactionGuildId :: Maybe GuildId,
        
        interactionChannelId :: Maybe ChannelId,
        
        interactionUser :: MemberOrUser,
        
        interactionToken :: InteractionToken,
        
        interactionVersion :: Int,
        
        interactionPermissions :: Maybe T.Text,
        
        interactionLocale :: T.Text,
        
        interactionGuildLocale :: Maybe T.Text
      }
  | InteractionModalSubmit
      { 
        interactionId :: InteractionId,
        
        interactionApplicationId :: ApplicationId,
        
        modalData :: ModalData,
        
        interactionGuildId :: Maybe GuildId,
        
        interactionChannelId :: Maybe ChannelId,
        
        interactionUser :: MemberOrUser,
        
        interactionToken :: InteractionToken,
        
        interactionVersion :: Int,
        
        interactionPermissions :: Maybe T.Text,
        
        interactionLocale :: T.Text,
        
        interactionGuildLocale :: Maybe T.Text
      }
  deriving (Show, Read, Eq, Ord)

instance FromJSON Interaction where
  parseJSON =
    withObject
      "Interaction"
      ( \v -> do
          iid <- v .: "id"
          aid <- v .: "application_id"
          gid <- v .:? "guild_id"
          cid <- v .:? "channel_id"
          tok <- v .: "token"
          version <- v .: "version"
          glocale <- v .:? "guild_locale"
          permissions <- v .:? "app_permissions"
          t <- v .: "type" :: Parser Int
          case t of
            1 -> return $ InteractionPing iid aid tok version permissions
            2 ->
              InteractionApplicationCommand iid aid
                <$> v .: "data"
                <*> return gid
                <*> return cid
                <*> parseJSON (Object v)
                <*> return tok
                <*> return version
                <*> return permissions
                <*> v .: "locale"
                <*> return glocale
            3 ->
              InteractionComponent iid aid
                <$> v .: "data"
                <*> return gid
                <*> return cid
                <*> parseJSON (Object v)
                <*> return tok
                <*> return version
                <*> v .: "message"
                <*> return permissions
                <*> v .: "locale"
                <*> return glocale
            4 ->
              InteractionApplicationCommandAutocomplete iid aid
                <$> v .: "data"
                <*> return gid
                <*> return cid
                <*> parseJSON (Object v)
                <*> return tok
                <*> return version
                <*> return permissions
                <*> v .: "locale"
                <*> return glocale
            5 ->
              InteractionModalSubmit iid aid
                <$> v .: "data"
                <*> return gid
                <*> return cid
                <*> parseJSON (Object v)
                <*> return tok
                <*> return version
                <*> return permissions
                <*> v .: "locale"
                <*> return glocale
            _ -> fail "unknown interaction type"
      )

newtype MemberOrUser = MemberOrUser (Either GuildMember User)
  deriving (Show, Read, Eq, Ord)

instance {-# OVERLAPPING #-} FromJSON MemberOrUser where
  parseJSON =
    withObject
      "MemberOrUser"
      ( \v -> MemberOrUser <$> (Left <$> v .: "member" <|> Right <$> v .: "user")
      )

data ComponentData
  = ButtonData
      { 
        componentDataCustomId :: T.Text
      }
  | SelectMenuData
      { 
        componentDataCustomId :: T.Text,
        
        componentDataValues :: SelectMenuData
      }
  deriving (Show, Read, Eq, Ord)

instance FromJSON ComponentData where
  parseJSON =
    withObject
      "ComponentData"
      ( \v -> do
          cid <- v .: "custom_id"
          t <- v .: "component_type" :: Parser Int
          case t of
            2 -> return $ ButtonData cid
            _ | t `elem` [3, 5, 6, 7, 8] ->
              SelectMenuData cid
                <$> parseJSON (toJSON v)
            _ -> fail $ "unknown interaction data component type: " <> show t
      )

data SelectMenuData
  = SelectMenuDataText [T.Text] 
  | SelectMenuDataUser [UserId] 
  | SelectMenuDataRole [RoleId] 
  | SelectMenuDataMentionable [Snowflake] 
  | SelectMenuDataChannels [ChannelId] 
  deriving (Show, Read, Eq, Ord)

instance FromJSON SelectMenuData where
  parseJSON =
    withObject
      "SelectMenuData"
      $ \v -> do
          t <- v .: "component_type" :: Parser Int
          let cons :: forall a. FromJSON a => ([a] -> SelectMenuData) -> Parser SelectMenuData
              cons f = f <$> v .: "values"
          case t of
            3 -> cons SelectMenuDataText
            5 -> cons SelectMenuDataUser
            6 -> cons SelectMenuDataRole
            7 -> cons SelectMenuDataMentionable
            8 -> cons SelectMenuDataChannels
            _ -> fail $ "unknown SelectMenuData type: " <> show t

data ApplicationCommandData
  = ApplicationCommandDataUser
      { 
        applicationCommandDataId :: ApplicationCommandId,
        
        applicationCommandDataName :: T.Text,
        
        resolvedData :: Maybe ResolvedData,
        
        applicationCommandDataTargetUserId :: UserId
      }
  | ApplicationCommandDataMessage
      { 
        applicationCommandDataId :: ApplicationCommandId,
        
        applicationCommandDataName :: T.Text,
        
        resolvedData :: Maybe ResolvedData,
        
        applicationCommandDataTargetMessageId :: MessageId
      }
  | ApplicationCommandDataChatInput
      { 
        applicationCommandDataId :: ApplicationCommandId,
        
        applicationCommandDataName :: T.Text,
        
        resolvedData :: Maybe ResolvedData,
        
        optionsData :: Maybe OptionsData
      }
  deriving (Show, Read, Eq, Ord)

instance FromJSON ApplicationCommandData where
  parseJSON =
    withObject
      "ApplicationCommandData"
      ( \v -> do
          aci <- v .: "id"
          name <- v .: "name"
          rd <- v .:? "resolved_data"
          t <- v .: "type" :: Parser Int
          case t of
            1 ->
              ApplicationCommandDataChatInput aci name rd
                <$> v .:? "options"
            2 ->
              ApplicationCommandDataUser aci name rd
                <$> v .: "target_id"
            3 ->
              ApplicationCommandDataMessage aci name rd
                <$> v .: "target_id"
            _ -> fail "unknown interaction data component type"
      )


data OptionsData
  = OptionsDataSubcommands [OptionDataSubcommandOrGroup]
  | OptionsDataValues [OptionDataValue]
  deriving (Show, Read, Eq, Ord)

instance FromJSON OptionsData where
  parseJSON =
    withArray
      "OptionsData"
      ( \a -> do
          let a' = toList a
          case a' of
            [] -> return $ OptionsDataValues []
            (v' : _) ->
              withObject
                "OptionsData item"
                ( \v -> do
                    t <- v .: "type" :: Parser Int
                    if t == 1 || t == 2
                      then OptionsDataSubcommands <$> mapM parseJSON a'
                      else OptionsDataValues <$> mapM parseJSON a'
                )
                v'
      )


data OptionDataSubcommandOrGroup
  = OptionDataSubcommandGroup
      { optionDataSubcommandGroupName :: T.Text,
        optionDataSubcommandGroupOptions :: [OptionDataSubcommand],
        optionDataSubcommandGroupFocused :: Bool
      }
  | OptionDataSubcommandOrGroupSubcommand OptionDataSubcommand
  deriving (Show, Read, Eq, Ord)

instance FromJSON OptionDataSubcommandOrGroup where
  parseJSON =
    withObject
      "OptionDataSubcommandOrGroup"
      ( \v -> do
          t <- v .: "type" :: Parser Int
          case t of
            2 ->
              OptionDataSubcommandGroup
                <$> v .: "name"
                <*> v .: "options"
                <*> v .:? "focused" .!= False
            1 -> OptionDataSubcommandOrGroupSubcommand <$> parseJSON (Object v)
            _ -> fail "unexpected subcommand group type"
      )


data OptionDataSubcommand = OptionDataSubcommand
  { optionDataSubcommandName :: T.Text,
    optionDataSubcommandOptions :: [OptionDataValue],
    optionDataSubcommandFocused :: Bool
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON OptionDataSubcommand where
  parseJSON =
    withObject
      "OptionDataSubcommand"
      ( \v -> do
          t <- v .: "type" :: Parser Int
          case t of
            1 ->
              OptionDataSubcommand
                <$> v .: "name"
                <*> v .:? "options" .!= []
                <*> v .:? "focused" .!= False
            _ -> fail "unexpected subcommand type"
      )


data OptionDataValue
  = OptionDataValueString
      { optionDataValueName :: T.Text,
        optionDataValueString :: Either T.Text T.Text
      }
  | OptionDataValueInteger
      { optionDataValueName :: T.Text,
        optionDataValueInteger :: Either T.Text Integer
      }
  | OptionDataValueBoolean
      { optionDataValueName :: T.Text,
        optionDataValueBoolean :: Bool
      }
  | OptionDataValueUser
      { optionDataValueName :: T.Text,
        optionDataValueUser :: UserId
      }
  | OptionDataValueChannel
      { optionDataValueName :: T.Text,
        optionDataValueChannel :: ChannelId
      }
  | OptionDataValueRole
      { optionDataValueName :: T.Text,
        optionDataValueRole :: RoleId
      }
  | OptionDataValueMentionable
      { optionDataValueName :: T.Text,
        optionDataValueMentionable :: Snowflake
      }
  | OptionDataValueNumber
      { optionDataValueName :: T.Text,
        optionDataValueNumber :: Either T.Text Number
      }
  deriving (Show, Read, Eq, Ord)

instance FromJSON OptionDataValue where
  parseJSON =
    withObject
      "OptionDataValue"
      ( \v -> do
          name <- v .: "name"
          focused <- v .:? "focused" .!= False
          t <- v .: "type" :: Parser Int
          case t of
            3 ->
              OptionDataValueString name
                <$> parseValue v focused
            4 ->
              OptionDataValueInteger name
                <$> parseValue v focused
            10 ->
              OptionDataValueNumber name
                <$> parseValue v focused
            5 ->
              OptionDataValueBoolean name
                <$> v .: "value"
            6 ->
              OptionDataValueUser name
                <$> v .: "value"
            7 ->
              OptionDataValueChannel name
                <$> v .: "value"
            8 ->
              OptionDataValueRole name
                <$> v .: "value"
            9 ->
              OptionDataValueMentionable name
                <$> v .: "value"
            _ -> fail $ "unexpected interaction data application command option value type: " ++ show t
      )

data ModalData = ModalData
  { 
    modalDataCustomId :: T.Text,
    
    modalDataComponents :: [TextInput]
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON ModalData where
  parseJSON =
    withObject
      "ModalData"
      ( \v ->
          ModalData <$> v .: "custom_id"
            <*> ((v .: "components") >>= (join <$>) . mapM getTextInput)
      )
    where
      getTextInput :: Value -> Parser [TextInput]
      getTextInput = withObject "ModalData.TextInput" $ \o -> do
        t <- o .: "type" :: Parser Int
        case t of
          1 -> o .: "components"
          _ -> fail $ "expected action row type (1), got: " ++ show t

parseValue :: (FromJSON a) => Object -> Bool -> Parser (Either T.Text a)
parseValue o True = Left <$> o .: "value"
parseValue o False = Right <$> o .: "value"










data ResolvedData = ResolvedData
  { resolvedDataUsers :: Maybe Value,
    resolvedDataMembers :: Maybe Value,
    resolvedDataRoles :: Maybe Value,
    resolvedDataChannels :: Maybe Value,
    resolvedDataMessages :: Maybe Value,
    resolvedDataAttachments :: Maybe Value
  }
  deriving (Show, Read, Eq, Ord)

instance ToJSON ResolvedData where
  toJSON ResolvedData {..} =
    objectFromMaybes
      [ "users" .=? resolvedDataUsers,
        "members" .=? resolvedDataMembers,
        "roles" .=? resolvedDataRoles,
        "channels" .=? resolvedDataChannels,
        "messages" .=? resolvedDataMessages,
        "attachments" .=? resolvedDataAttachments
      ]

instance FromJSON ResolvedData where
  parseJSON =
    withObject
      "ResolvedData"
      ( \v ->
          ResolvedData
            <$> v .:? "users"
            <*> v .:? "members"
            <*> v .:? "roles"
            <*> v .:? "channels"
            <*> v .:? "messages"
            <*> v .:? "attachments"
      )




data InteractionResponse
  = 
    InteractionResponsePong
  | 
    InteractionResponseChannelMessage InteractionResponseMessage
  | 
    InteractionResponseDeferChannelMessage
  | 
    InteractionResponseDeferUpdateMessage
  | 
    InteractionResponseUpdateMessage InteractionResponseMessage
  | 
    InteractionResponseAutocompleteResult InteractionResponseAutocomplete
  | 
    InteractionResponseModal InteractionResponseModalData
  deriving (Show, Read, Eq, Ord)


interactionResponseBasic :: T.Text -> InteractionResponse
interactionResponseBasic t = InteractionResponseChannelMessage (interactionResponseMessageBasic t)

instance ToJSON InteractionResponse where
  toJSON InteractionResponsePong = object [("type", Number 1)]
  toJSON InteractionResponseDeferChannelMessage = object [("type", Number 5)]
  toJSON InteractionResponseDeferUpdateMessage = object [("type", Number 6)]
  toJSON (InteractionResponseChannelMessage ms) = object [("type", Number 4), ("data", toJSON ms)]
  toJSON (InteractionResponseUpdateMessage ms) = object [("type", Number 7), ("data", toJSON ms)]
  toJSON (InteractionResponseAutocompleteResult ms) = object [("type", Number 8), ("data", toJSON ms)]
  toJSON (InteractionResponseModal ms) = object [("type", Number 9), ("data", toJSON ms)]

data InteractionResponseAutocomplete
  = InteractionResponseAutocompleteString [Choice T.Text]
  | InteractionResponseAutocompleteInteger [Choice Integer]
  | InteractionResponseAutocompleteNumber [Choice Number]
  deriving (Show, Read, Eq, Ord)

instance ToJSON InteractionResponseAutocomplete where
  toJSON (InteractionResponseAutocompleteString cs) = object [("choices", toJSON cs)]
  toJSON (InteractionResponseAutocompleteInteger cs) = object [("choices", toJSON cs)]
  toJSON (InteractionResponseAutocompleteNumber cs) = object [("choices", toJSON cs)]


data InteractionResponseMessage = InteractionResponseMessage
  { interactionResponseMessageTTS :: Maybe Bool,
    interactionResponseMessageContent :: Maybe T.Text,
    interactionResponseMessageEmbeds :: Maybe [CreateEmbed],
    interactionResponseMessageAllowedMentions :: Maybe AllowedMentions,
    interactionResponseMessageFlags :: Maybe InteractionResponseMessageFlags,
    interactionResponseMessageComponents :: Maybe [ActionRow],
    interactionResponseMessageAttachments :: Maybe [Attachment]
  }
  deriving (Show, Read, Eq, Ord)



interactionResponseMessageBasic :: T.Text -> InteractionResponseMessage
interactionResponseMessageBasic t = InteractionResponseMessage Nothing (Just t) Nothing Nothing Nothing Nothing Nothing

instance ToJSON InteractionResponseMessage where
  toJSON InteractionResponseMessage {..} =
    objectFromMaybes
      [ "tts" .=? interactionResponseMessageTTS,
        "content" .=? interactionResponseMessageContent,
        "embeds" .=? ((createEmbed <$>) <$> interactionResponseMessageEmbeds),
        "allowed_mentions" .=? interactionResponseMessageAllowedMentions,
        "flags" .=? interactionResponseMessageFlags,
        "components" .=? interactionResponseMessageComponents,
        "attachments" .=? interactionResponseMessageAttachments
      ]





data InteractionResponseMessageFlag = InteractionResponseMessageFlagEphermeral
  deriving (Show, Read, Eq, Ord)

newtype InteractionResponseMessageFlags = InteractionResponseMessageFlags [InteractionResponseMessageFlag]
  deriving (Show, Read, Eq, Ord)

instance Enum InteractionResponseMessageFlag where
  fromEnum InteractionResponseMessageFlagEphermeral = 1 `shift` 6
  toEnum i
    | i == 1 `shift` 6 = InteractionResponseMessageFlagEphermeral
    | otherwise = error $ "could not find InteractionCallbackDataFlag `" ++ show i ++ "`"

instance ToJSON InteractionResponseMessageFlags where
  toJSON (InteractionResponseMessageFlags fs) = Number $ fromInteger $ fromIntegral $ foldr (.|.) 0 (fromEnum <$> fs)

data InteractionResponseModalData = InteractionResponseModalData
  { interactionResponseModalCustomId :: T.Text,
    interactionResponseModalTitle :: T.Text,
    interactionResponseModalComponents :: [TextInput]
  }
  deriving (Show, Read, Eq, Ord)

instance ToJSON InteractionResponseModalData where
  toJSON InteractionResponseModalData {..} =
    object
      [ ("custom_id", toJSON interactionResponseModalCustomId),
        ("title", toJSON interactionResponseModalTitle),
        ("components", toJSON $ map (\ti -> object [("type", Number 1), ("components", toJSON [ti])]) interactionResponseModalComponents)
      ]
