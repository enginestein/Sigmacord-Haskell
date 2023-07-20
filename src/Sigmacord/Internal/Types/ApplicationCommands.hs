{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Sigmacord.Internal.Types.ApplicationCommands
  ( ApplicationCommand (..),
    Options (..),
    OptionSubcommandOrGroup (..),
    OptionSubcommand (..),
    OptionValue (..),
    createChatInput,
    createUser,
    createMessage,
    CreateApplicationCommand (..),
    EditApplicationCommand (..),
    defaultEditApplicationCommand,
    Choice (..),
    ChannelTypeOption (..),
    GuildApplicationCommandPermissions (..),
    ApplicationCommandPermissions (..),
    Number,
    AutocompleteOrChoice,
    LocalizedText,
    Locale
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Number, Object), object, withArray, withObject, (.!=), (.:), (.:!), (.:?))
import Data.Aeson.Types (Pair, Parser)
import Data.Foldable (Foldable (toList))
import Data.Scientific (Scientific)
import Data.Char (isLower, isNumber)
import Sigmacord.Internal.Types.Prelude (ApplicationCommandId, ApplicationId, GuildId, Snowflake, objectFromMaybes, (.==), (.=?))
import Data.Map.Strict (Map)
import Sigmacord.Internal.Types.Channel ( ChannelTypeOption(..) )

import qualified Data.Text as T

type Number = Scientific


data ApplicationCommand
  = ApplicationCommandUser
      { 
        applicationCommandId :: ApplicationCommandId,
        
        applicationCommandApplicationId :: ApplicationId,
        
        applicationCommandGuildId :: Maybe GuildId,
        
        applicationCommandName :: T.Text,
        
        applicationCommandLocalizedName :: Maybe LocalizedText,
        
        applicationCommandDefaultMemberPermissions :: Maybe T.Text,
        
        applicationCommandDMPermission :: Maybe Bool,
        
        applicationCommandVersion :: Snowflake
      }
  | ApplicationCommandMessage
      { 
        applicationCommandId :: ApplicationCommandId,
        
        applicationCommandApplicationId :: ApplicationId,
        
        applicationCommandGuildId :: Maybe GuildId,
        
        applicationCommandName :: T.Text,
        
        applicationCommandLocalizedName :: Maybe LocalizedText,
        
        applicationCommandDefaultMemberPermissions :: Maybe T.Text,
        
        applicationCommandDMPermission :: Maybe Bool,
        
        applicationCommandVersion :: Snowflake
      }
  | ApplicationCommandChatInput
      { 
        applicationCommandId :: ApplicationCommandId,
        
        applicationCommandApplicationId :: ApplicationId,
        
        applicationCommandGuildId :: Maybe GuildId,
        
        applicationCommandName :: T.Text,
        
        applicationCommandLocalizedName :: Maybe LocalizedText,
        
        applicationCommandDescription :: T.Text,
        
        applicationCommandLocalizedDescription :: Maybe LocalizedText,
        
        applicationCommandOptions :: Maybe Options,
        
        applicationCommandDefaultMemberPermissions :: Maybe T.Text,
        
        applicationCommandDMPermission :: Maybe Bool,
        
        applicationCommandVersion :: Snowflake
      }
  deriving (Show, Eq, Read)

instance FromJSON ApplicationCommand where
  parseJSON =
    withObject
      "ApplicationCommand"
      ( \v -> do
          acid <- v .: "id"
          aid <- v .: "application_id"
          gid <- v .:? "guild_id"
          name <- v .: "name"
          lname <- v .:? "name_localizations"
          defPerm <- v .:? "default_member_permissions"
          dmPerm <- v .:? "dm_permission"
          version <- v .: "version"
          t <- v .:? "type" :: Parser (Maybe Int)
          case t of
            (Just 2) -> return $ ApplicationCommandUser acid aid gid name lname defPerm dmPerm version
            (Just 3) -> return $ ApplicationCommandMessage acid aid gid name lname defPerm dmPerm version
            _ -> do
              desc <- v .: "description"
              options <- v .:? "options"
              ldesc <- v .:? "description_localizations"
              return $ ApplicationCommandChatInput acid aid gid name lname desc ldesc options defPerm dmPerm version
      )


data Options
  = OptionsSubcommands [OptionSubcommandOrGroup]
  | OptionsValues [OptionValue]
  deriving (Show, Eq, Read)

instance FromJSON Options where
  parseJSON =
    withArray
      "Options"
      ( \a -> do
          let a' = toList a
          case a' of
            [] -> return $ OptionsValues []
            (v' : _) ->
              withObject
                "Options item"
                ( \v -> do
                    t <- v .: "type" :: Parser Int
                    if t == 1 || t == 2
                      then OptionsSubcommands <$> mapM parseJSON a'
                      else OptionsValues <$> mapM parseJSON a'
                )
                v'
      )

instance ToJSON Options where
  toJSON (OptionsSubcommands o) = toJSON o
  toJSON (OptionsValues o) = toJSON o


data OptionSubcommandOrGroup
  = OptionSubcommandGroup
      { 
        optionSubcommandGroupName :: T.Text,
        
        optionSubcommandGroupLocalizedName :: Maybe LocalizedText,
        
        optionSubcommandGroupDescription :: T.Text,
        
        optionSubcommandGroupLocalizedDescription :: Maybe LocalizedText,
        
        optionSubcommandGroupOptions :: [OptionSubcommand]
      }
  | OptionSubcommandOrGroupSubcommand OptionSubcommand
  deriving (Show, Eq, Read)

instance FromJSON OptionSubcommandOrGroup where
  parseJSON =
    withObject
      "OptionSubcommandOrGroup"
      ( \v -> do
          t <- v .: "type" :: Parser Int
          case t of
            2 ->
              OptionSubcommandGroup
                <$> v .: "name"
                <*> v .:? "name_localizations"
                <*> v .: "description"
                <*> v .:? "description_localizations"
                <*> v .: "options"
            1 -> OptionSubcommandOrGroupSubcommand <$> parseJSON (Object v)
            _ -> fail "unexpected subcommand group type"
      )

instance ToJSON OptionSubcommandOrGroup where
  toJSON OptionSubcommandGroup {..} =
    object
      [ ("type", Number 2),
        ("name", toJSON optionSubcommandGroupName),
        ("name_localizations", toJSON optionSubcommandGroupLocalizedName),
        ("description", toJSON optionSubcommandGroupDescription),
        ("description_localizations", toJSON optionSubcommandGroupLocalizedDescription),
        ("options", toJSON optionSubcommandGroupOptions)
      ]
  toJSON (OptionSubcommandOrGroupSubcommand a) = toJSON a


data OptionSubcommand = OptionSubcommand
  { 
    optionSubcommandName :: T.Text,
    
    optionSubcommandLocalizedName :: Maybe LocalizedText,
    
    optionSubcommandDescription :: T.Text,
    
    optionSubcommandLocalizedDescription :: Maybe LocalizedText,
    
    optionSubcommandOptions :: [OptionValue]
  }
  deriving (Show, Eq, Read)

instance FromJSON OptionSubcommand where
  parseJSON =
    withObject
      "OptionSubcommand"
      ( \v -> do
          t <- v .: "type" :: Parser Int
          case t of
            1 ->
              OptionSubcommand
                <$> v .: "name"
                <*> v .:? "name_localizations"
                <*> v .: "description"
                <*> v .:? "description_localizations"
                <*> v .:? "options" .!= []
            _ -> fail "unexpected subcommand type"
      )

instance ToJSON OptionSubcommand where
  toJSON OptionSubcommand {..} =
    object
      [ ("type", Number 1),
        ("name", toJSON optionSubcommandName),
        ("name_localizations", toJSON optionSubcommandLocalizedName),
        ("description", toJSON optionSubcommandDescription),
        ("description_localizations", toJSON optionSubcommandLocalizedDescription),
        ("options", toJSON optionSubcommandOptions)
      ]


data OptionValue
  = OptionValueString
      { 
        optionValueName :: T.Text,
        
        optionValueLocalizedName :: Maybe LocalizedText,
        
        optionValueDescription :: T.Text,
        
        optionValueLocalizedDescription :: Maybe LocalizedText,
        
        optionValueRequired :: Bool,
        
        optionValueStringChoices :: AutocompleteOrChoice T.Text,
        
        optionValueStringMinLen :: Maybe Integer,
        
        optionValueStringMaxLen :: Maybe Integer
      }
  | OptionValueInteger
      { 
        optionValueName :: T.Text,
        
        optionValueLocalizedName :: Maybe LocalizedText,
        
        optionValueDescription :: T.Text,
        
        optionValueLocalizedDescription :: Maybe LocalizedText,
        
        optionValueRequired :: Bool,
        
        optionValueIntegerChoices :: AutocompleteOrChoice Integer,
        
        optionValueIntegerMinVal :: Maybe Integer,
        
        optionValueIntegerMaxVal :: Maybe Integer
      }
  | OptionValueBoolean
      { 
        optionValueName :: T.Text,
        
        optionValueLocalizedName :: Maybe LocalizedText,
        
        optionValueDescription :: T.Text,
        
        optionValueLocalizedDescription :: Maybe LocalizedText,
        
        optionValueRequired :: Bool
      }
  | OptionValueUser
      { 
        optionValueName :: T.Text,
        
        optionValueLocalizedName :: Maybe LocalizedText,
        
        optionValueDescription :: T.Text,
        
        optionValueLocalizedDescription :: Maybe LocalizedText,
        
        optionValueRequired :: Bool
      }
  | OptionValueChannel
      { 
        optionValueName :: T.Text,
        
        optionValueLocalizedName :: Maybe LocalizedText,
        
        optionValueDescription :: T.Text,
        
        optionValueLocalizedDescription :: Maybe LocalizedText,
        
        optionValueRequired :: Bool,
        
        optionValueChannelTypes :: Maybe [ChannelTypeOption]
      }
  | OptionValueRole
      { 
        optionValueName :: T.Text,
        
        optionValueLocalizedName :: Maybe LocalizedText,
        
        optionValueDescription :: T.Text,
        
        optionValueLocalizedDescription :: Maybe LocalizedText,
        
        optionValueRequired :: Bool
      }
  | OptionValueMentionable
      { 
        optionValueName :: T.Text,
        
        optionValueLocalizedName :: Maybe LocalizedText,
        
        optionValueDescription :: T.Text,
        
        optionValueLocalizedDescription :: Maybe LocalizedText,
        
        optionValueRequired :: Bool
      }
  | OptionValueNumber
      { 
        optionValueName :: T.Text,
        
        optionValueLocalizedName :: Maybe LocalizedText,
        
        optionValueDescription :: T.Text,
        
        optionValueLocalizedDescription :: Maybe LocalizedText,
        
        optionValueRequired :: Bool,
        
        optionValueNumberChoices :: AutocompleteOrChoice Number,
        
        optionValueNumberMinVal :: Maybe Number,
        
        optionValueNumberMaxVal :: Maybe Number
      }
  deriving (Show, Eq, Read)

instance FromJSON OptionValue where
  parseJSON =
    withObject
      "OptionValue"
      ( \v -> do
          name <- v .: "name"
          lname <- v .:? "name_localizations"
          desc <- v .: "description"
          ldesc <- v .:? "description_localizations"
          required <- v .:? "required" .!= False
          t <- v .: "type" :: Parser Int
          case t of
            3 ->
              OptionValueString name lname desc ldesc required
                <$> parseJSON (Object v)
                <*> v .:? "min_length"
                <*> v .:? "max_length"
            4 ->
              OptionValueInteger name lname desc ldesc required
                <$> parseJSON (Object v)
                <*> v .:? "min_value"
                <*> v .:? "max_value"
            10 ->
              OptionValueNumber name lname desc ldesc required
                <$> parseJSON (Object v)
                <*> v .:? "min_value"
                <*> v .:? "max_value"
            7 ->
              OptionValueChannel name lname desc ldesc required
                <$> v .:? "channel_types"
            5 -> return $ OptionValueBoolean name lname desc ldesc required
            6 -> return $ OptionValueUser name lname desc ldesc required
            8 -> return $ OptionValueRole name lname desc ldesc required
            9 -> return $ OptionValueMentionable name lname desc ldesc required
            _ -> fail "unknown application command option value type"
      )

instance ToJSON OptionValue where
  toJSON OptionValueString {..} =
    object
      [ ("type", Number 3),
        ("name", toJSON optionValueName),
        ("description", toJSON optionValueDescription),
        ("name_localizations", toJSON optionValueLocalizedName),
        ("description_localizations", toJSON optionValueLocalizedDescription),
        ("required", toJSON optionValueRequired),
        ("min_length", toJSON optionValueStringMinLen),
        ("max_length", toJSON optionValueStringMaxLen),
        choiceOrAutocompleteToJSON optionValueStringChoices
      ]
  toJSON OptionValueInteger {..} =
    object
      [ ("type", Number 4),
        ("name", toJSON optionValueName),
        ("description", toJSON optionValueDescription),
        ("name_localizations", toJSON optionValueLocalizedName),
        ("description_localizations", toJSON optionValueLocalizedDescription),
        ("required", toJSON optionValueRequired),
        ("min_value", toJSON optionValueIntegerMinVal),
        ("max_value", toJSON optionValueIntegerMaxVal),
        choiceOrAutocompleteToJSON optionValueIntegerChoices
      ]
  toJSON OptionValueNumber {..} =
    object
      [ ("type", Number 10),
        ("name", toJSON optionValueName),
        ("description", toJSON optionValueDescription),
        ("name_localizations", toJSON optionValueLocalizedName),
        ("description_localizations", toJSON optionValueLocalizedDescription),
        ("required", toJSON optionValueRequired),
        ("min_value", toJSON optionValueNumberMinVal),
        ("max_value", toJSON optionValueNumberMaxVal),
        choiceOrAutocompleteToJSON optionValueNumberChoices
      ]
  toJSON OptionValueChannel {..} =
    object
      [ ("type", Number 7),
        ("name", toJSON optionValueName),
        ("description", toJSON optionValueDescription),
        ("name_localizations", toJSON optionValueLocalizedName),
        ("description_localizations", toJSON optionValueLocalizedDescription),
        ("required", toJSON optionValueRequired),
        ("channel_types", toJSON optionValueChannelTypes)
      ]
  toJSON acov =
    object
      [ ("type", Number (t acov)),
        ("name", toJSON $ optionValueName acov),
        ("description", toJSON $ optionValueDescription acov),
        ("name_localizations", toJSON $ optionValueLocalizedName acov),
        ("description_localizations", toJSON $ optionValueLocalizedDescription acov),
        ("required", toJSON $ optionValueRequired acov)
      ]
    where
      t OptionValueBoolean {} = 5
      t OptionValueUser {} = 6
      t OptionValueRole {} = 8
      t OptionValueMentionable {} = 9
      t _ = -1

















data CreateApplicationCommand
  = CreateApplicationCommandChatInput
      { 
        createName :: T.Text,
        
        createLocalizedName :: Maybe LocalizedText,
        
        createDescription :: T.Text,
        
        createLocalizedDescription :: Maybe LocalizedText,
        
        createOptions :: Maybe Options,
        
        
        
        createDefaultMemberPermissions :: Maybe T.Text,
        
        createDMPermission :: Maybe Bool
      }
  | CreateApplicationCommandUser
      { 
        createName :: T.Text,
        
        createLocalizedName :: Maybe LocalizedText,
        
        
        
        createDefaultMemberPermissions :: Maybe T.Text,
        
        createDMPermission :: Maybe Bool
      }
  | CreateApplicationCommandMessage
      { 
        createName :: T.Text,
        
        createLocalizedName :: Maybe LocalizedText,
        
        
        
        createDefaultMemberPermissions :: Maybe T.Text,
        
        createDMPermission :: Maybe Bool
      }
  deriving (Show, Eq, Read)

instance ToJSON CreateApplicationCommand where
  toJSON CreateApplicationCommandChatInput {..} =
    objectFromMaybes
      [ "name" .== createName,
        "name_localizations" .=? createLocalizedName,
        "description" .== createDescription,
        "description_localizations" .=? createLocalizedDescription,
        "options" .=? createOptions,
        "default_member_permissions" .== createDefaultMemberPermissions,
        "dm_permission" .== createDMPermission,
        "type" .== Number 1
      ]
  toJSON CreateApplicationCommandUser {..} =
    objectFromMaybes
      [ "name" .== createName,
        "name_localizations" .=? createLocalizedName,
        "default_member_permissions" .== createDefaultMemberPermissions,
        "dm_permission" .== createDMPermission,
        "type" .== Number 2
      ]
  toJSON CreateApplicationCommandMessage {..} =
    objectFromMaybes
      [ "name" .== createName,
        "name_localizations" .=? createLocalizedName,
        "default_member_permissions" .== createDefaultMemberPermissions,
        "dm_permission" .== createDMPermission,
        "type" .== Number 3
      ]

nameIsValid :: Bool -> T.Text -> Bool
nameIsValid isChatInput name = l >= 1 && l <= 32 && isChatInput <= T.all validChar name
  where
    l = T.length name
    validChar c = c == '-' || c == '_' || isLower c || isNumber c





createChatInput :: T.Text -> T.Text -> Maybe CreateApplicationCommand
createChatInput name desc
  | nameIsValid True name && not (T.null desc) && T.length desc <= 100 = Just $ CreateApplicationCommandChatInput name Nothing desc Nothing Nothing Nothing Nothing
  | otherwise = Nothing



createUser :: T.Text -> Maybe CreateApplicationCommand
createUser name
  | nameIsValid False name = Just $ CreateApplicationCommandUser name Nothing Nothing Nothing
  | otherwise = Nothing



createMessage :: T.Text -> Maybe CreateApplicationCommand
createMessage name
  | nameIsValid False name = Just $ CreateApplicationCommandMessage name Nothing Nothing Nothing
  | otherwise = Nothing






data EditApplicationCommand
  = EditApplicationCommandChatInput
      { editName :: Maybe T.Text,
        editLocalizedName :: Maybe LocalizedText,
        editDescription :: Maybe T.Text,
        editLocalizedDescription :: Maybe LocalizedText,
        editOptions :: Maybe Options,
        editDefaultMemberPermissions :: Maybe T.Text,
        editDMPermission :: Maybe Bool
      }
  | EditApplicationCommandUser
      { editName :: Maybe T.Text,
        editLocalizedName :: Maybe LocalizedText,
        editDefaultMemberPermissions :: Maybe T.Text,
        editDMPermission :: Maybe Bool
      }
  | EditApplicationCommandMessage
      { editName :: Maybe T.Text,
        editLocalizedName :: Maybe LocalizedText,
        editDefaultMemberPermissions :: Maybe T.Text,
        editDMPermission :: Maybe Bool
      }

defaultEditApplicationCommand :: Int -> EditApplicationCommand
defaultEditApplicationCommand 2 = EditApplicationCommandUser Nothing Nothing Nothing Nothing
defaultEditApplicationCommand 3 = EditApplicationCommandMessage Nothing Nothing Nothing Nothing
defaultEditApplicationCommand _ = EditApplicationCommandChatInput Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance ToJSON EditApplicationCommand where
  toJSON EditApplicationCommandChatInput {..} =
    objectFromMaybes
      [ "name" .=? editName,
        "name_localization" .=? editLocalizedName,
        "description" .=? editDescription,
        "description_localization" .=? editLocalizedDescription,
        "options" .=? editOptions,
        "default_member_permissions" .=? editDefaultMemberPermissions,
        "dm_permission" .=? editDMPermission,
        "type" .== Number 1
      ]
  toJSON EditApplicationCommandUser {..} =
    objectFromMaybes
      [ "name" .=? editName,
        "name_localization" .=? editLocalizedName,
        "default_member_permissions" .=? editDefaultMemberPermissions,
        "dm_permission" .=? editDMPermission,
        "type" .== Number 2
      ]
  toJSON EditApplicationCommandMessage {..} =
    objectFromMaybes
      [ "name" .=? editName,
        "name_localization" .=? editLocalizedName,
        "default_member_permissions" .=? editDefaultMemberPermissions,
        "dm_permission" .=? editDMPermission,
        "type" .== Number 3
      ]

data Choice a = Choice
  { 
    choiceName :: T.Text,
    
    choiceLocalizedName :: Maybe LocalizedText,
    
    choiceValue :: a
  }
  deriving (Show, Read, Eq, Ord)

instance Functor Choice where
  fmap f (Choice s l a) = Choice s l (f a)

instance (ToJSON a) => ToJSON (Choice a) where
  toJSON Choice {..} =
    object
      [ ("name", toJSON choiceName),
        ("value", toJSON choiceValue),
        ("name_localizations", toJSON choiceLocalizedName)
      ]

instance (FromJSON a) => FromJSON (Choice a) where
  parseJSON =
    withObject
      "Choice"
      ( \v ->
          Choice
            <$> v .: "name"
            <*> v .:? "name_localizations"
            <*> v .: "value"
      )

type AutocompleteOrChoice a = Either Bool [Choice a]

instance {-# OVERLAPPING #-} (FromJSON a) => FromJSON (AutocompleteOrChoice a) where
  parseJSON =
    withObject
      "AutocompleteOrChoice"
      ( \v -> do
          mcs <- v .:! "choices"
          case mcs of
            Nothing -> Left <$> v .:? "autocomplete" .!= False
            Just cs -> return $ Right cs
      )

choiceOrAutocompleteToJSON :: (ToJSON a) => AutocompleteOrChoice a -> Pair
choiceOrAutocompleteToJSON (Left b) = ("autocomplete", toJSON b)
choiceOrAutocompleteToJSON (Right cs) = ("choices", toJSON cs)

data GuildApplicationCommandPermissions = GuildApplicationCommandPermissions
  { 
    guildApplicationCommandPermissionsId :: ApplicationCommandId,
    
    guildApplicationCommandPermissionsApplicationId :: ApplicationId,
    
    guildApplicationCommandPermissionsGuildId :: GuildId,
    
    guildApplicationCommandPermissionsPermissions :: [ApplicationCommandPermissions]
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON GuildApplicationCommandPermissions where
  parseJSON =
    withObject
      "GuildApplicationCommandPermissions"
      ( \v ->
          GuildApplicationCommandPermissions
            <$> v .: "id"
            <*> v .: "application_id"
            <*> v .: "guild_id"
            <*> v .: "permissions"
      )

instance ToJSON GuildApplicationCommandPermissions where
  toJSON GuildApplicationCommandPermissions {..} =
    objectFromMaybes
      [ "id" .== guildApplicationCommandPermissionsId,
        "application_id" .== guildApplicationCommandPermissionsApplicationId,
        "guild_id" .== guildApplicationCommandPermissionsGuildId,
        "permissions" .== guildApplicationCommandPermissionsPermissions
      ]



data ApplicationCommandPermissions = ApplicationCommandPermissions
  { 
    applicationCommandPermissionsId :: Snowflake,
    
    applicationCommandPermissionsType :: Integer,
    
    applicationCommandPermissionsPermission :: Bool
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON ApplicationCommandPermissions where
  parseJSON =
    withObject
      "ApplicationCommandPermissions"
      ( \v ->
          ApplicationCommandPermissions
            <$> v .: "id"
            <*> v .: "type"
            <*> v .: "permission"
      )

instance ToJSON ApplicationCommandPermissions where
  toJSON ApplicationCommandPermissions {..} =
    objectFromMaybes
      [ "id" .== applicationCommandPermissionsId,
        "type" .== applicationCommandPermissionsType,
        "permission" .== applicationCommandPermissionsPermission
      ]



type Locale = T.Text


type LocalizedText = Map Locale T.Text
