{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Sigmacord.Internal.Types.Components
  ( ActionRow (..),
    Button (..),
    ButtonStyle (..),
    mkButton,
    SelectMenu (..),
    mkSelectMenu,
    SelectMenuData (..),
    SelectOption (..),
    mkSelectOption,
    TextInput (..),
    mkTextInput,
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Foldable (Foldable (toList))
import Data.Scientific (Scientific)
import qualified Data.Text as T
import Sigmacord.Internal.Types.Emoji (Emoji)
import Sigmacord.Internal.Types.Prelude (objectFromMaybes, (.==), (.=?), ChannelTypeOption)


data ActionRow = ActionRowButtons [Button] | ActionRowSelectMenu SelectMenu
  deriving (Show, Read, Eq, Ord)

instance FromJSON ActionRow where
  parseJSON =
    withObject
      "ActionRow"
      ( \cs -> do
          t <- cs .: "type" :: Parser Int
          case t of
            1 -> do
              a <- cs .: "components" :: Parser Array
              let a' = toList a
              case a' of
                [] -> return $ ActionRowButtons []
                (c : _) ->
                  withObject
                    "ActionRow item"
                    ( \v -> do
                        t' <- v .: "type" :: Parser Int
                        case t' of
                          2 -> ActionRowButtons <$> mapM parseJSON a'
                          _ | t' `elem` [3, 5, 6, 7, 8] -> ActionRowSelectMenu <$> parseJSON c
                          _ -> fail $ "unknown component type: " ++ show t'
                    )
                    c
            _ -> fail $ "expected action row type (1), got: " ++ show t
      )

instance ToJSON ActionRow where
  toJSON (ActionRowButtons bs) = object [("type", Number 1), ("components", toJSON bs)]
  toJSON (ActionRowSelectMenu bs) = object [("type", Number 1), ("components", toJSON [bs])]




data Button
  = Button
      { 
        buttonCustomId :: T.Text,
        
        buttonDisabled :: Bool,
        
        buttonStyle :: ButtonStyle,
        
        buttonLabel :: Maybe T.Text,
        
        buttonEmoji :: Maybe Emoji
      }
  | ButtonUrl
      { 
        
        buttonUrl :: T.Text,
        
        buttonDisabled :: Bool,
        
        buttonLabel :: Maybe T.Text,
        
        buttonEmoji :: Maybe Emoji
      }
  deriving (Show, Read, Eq, Ord)


mkButton :: T.Text -> T.Text -> Button
mkButton label customId = Button customId False ButtonStyleSecondary (Just label) Nothing

instance FromJSON Button where
  parseJSON =
    withObject
      "Button"
      ( \v -> do
          t <- v .: "type" :: Parser Int
          case t of
            2 -> do
              disabled <- v .:? "disabled" .!= False
              label <- v .:? "label"
              partialEmoji <- v .:? "emoji"
              style <- v .: "style" :: Parser Scientific
              case style of
                5 ->
                  ButtonUrl
                    <$> v .: "url"
                    <*> return disabled
                    <*> return label
                    <*> return partialEmoji
                _ ->
                  Button
                    <$> v .: "custom_id"
                    <*> return disabled
                    <*> parseJSON (Number style)
                    <*> return label
                    <*> return partialEmoji
            _ -> fail "expected button type, got a different component"
      )

instance ToJSON Button where
  toJSON ButtonUrl {..} =
    objectFromMaybes
      [ "type" .== Number 2,
        "style" .== Number 5,
        "label" .=? buttonLabel,
        "disabled" .== buttonDisabled,
        "url" .== buttonUrl,
        "emoji" .=? buttonEmoji
      ]
  toJSON Button {..} =
    objectFromMaybes
      [ "type" .== Number 2,
        "style" .== buttonStyle,
        "label" .=? buttonLabel,
        "disabled" .== buttonDisabled,
        "custom_id" .== buttonCustomId,
        "emoji" .=? buttonEmoji
      ]


data ButtonStyle
  = 
    ButtonStylePrimary
  | 
    ButtonStyleSecondary
  | 
    ButtonStyleSuccess
  | 
    ButtonStyleDanger
  deriving (Show, Read, Eq, Ord)

instance FromJSON ButtonStyle where
  parseJSON =
    withScientific
      "ButtonStyle"
      ( \case
          1 -> return ButtonStylePrimary
          2 -> return ButtonStyleSecondary
          3 -> return ButtonStyleSuccess
          4 -> return ButtonStyleDanger
          _ -> fail "unrecognised non-url button style"
      )

instance ToJSON ButtonStyle where
  toJSON ButtonStylePrimary = Number 1
  toJSON ButtonStyleSecondary = Number 2
  toJSON ButtonStyleSuccess = Number 3
  toJSON ButtonStyleDanger = Number 4




data SelectMenu = SelectMenu
  { 
    selectMenuCustomId :: T.Text,
    
    selectMenuDisabled :: Bool,
    
    selectMenuData :: SelectMenuData,
    
    selectMenuPlaceholder :: Maybe T.Text,
    
    selectMenuMinValues :: Maybe Integer,
    
    selectMenuMaxValues :: Maybe Integer
  }
  deriving (Show, Read, Eq, Ord)



mkSelectMenu :: T.Text -> [SelectOption] -> SelectMenu
mkSelectMenu customId sos = SelectMenu customId False (SelectMenuDataText sos) Nothing Nothing Nothing

instance FromJSON SelectMenu where
  parseJSON =
    withObject
      "SelectMenu"
      $ \v ->
          do
                SelectMenu
                  <$> v .: "custom_id"
                  <*> v .:? "disabled" .!= False
                  <*> parseJSON (Object v)
                  <*> v .:? "placeholder"
                  <*> v .:? "min_values"
                  <*> v .:? "max_values"
      

instance ToJSON SelectMenu where
  toJSON SelectMenu {..} =
    objectFromMaybes $
      [ "custom_id" .== selectMenuCustomId,
        "disabled" .== selectMenuDisabled,
        "placeholder" .=? selectMenuPlaceholder,
        "min_values" .=? selectMenuMinValues,
        "max_values" .=? selectMenuMaxValues
      ] <> case selectMenuData of
            SelectMenuDataText sos -> ["type" .== Number 3, "options" .== sos]
            SelectMenuDataUser -> ["type" .== Number 5]
            SelectMenuDataRole -> ["type" .== Number 6]
            SelectMenuDataMentionable -> ["type" .== Number 7]
            SelectMenuDataChannels ctos -> ["type" .== Number 8, "channel_types" .== ctos]

data SelectMenuData = 
    SelectMenuDataText [SelectOption] 
  | SelectMenuDataUser 
  | SelectMenuDataRole 
  | SelectMenuDataMentionable 
  | SelectMenuDataChannels [ChannelTypeOption] 
  deriving (Show, Read, Eq, Ord)

instance FromJSON SelectMenuData where
  parseJSON =
    withObject "SelectMenuData" $ \v ->
      do
        t <- v .: "type"
        case t::Int of
          3 -> SelectMenuDataText <$> v .: "options"
          5 -> pure SelectMenuDataUser
          6 -> pure SelectMenuDataRole
          7 -> pure SelectMenuDataMentionable
          8 -> SelectMenuDataChannels <$> v .: "channel_types"
          _ -> fail ("unknown select menu data type: " <> show t)


data SelectOption = SelectOption
  { 
    selectOptionLabel :: T.Text,
    
    selectOptionValue :: T.Text,
    
    selectOptionDescription :: Maybe T.Text,
    
    selectOptionEmoji :: Maybe Emoji,
    
    selectOptionDefault :: Maybe Bool
  }
  deriving (Show, Read, Eq, Ord)


mkSelectOption :: T.Text -> T.Text -> SelectOption
mkSelectOption label value = SelectOption label value Nothing Nothing Nothing

instance FromJSON SelectOption where
  parseJSON = withObject "SelectOption" $ \o ->
    SelectOption <$> o .: "label"
      <*> o .: "value"
      <*> o .:? "description"
      <*> o .:? "emoji"
      <*> o .:? "default"

instance ToJSON SelectOption where
  toJSON SelectOption {..} =
    objectFromMaybes
      [ "label" .== selectOptionLabel,
        "value" .== selectOptionValue,
        "description" .=? selectOptionDescription,
        "emoji" .=? selectOptionEmoji,
        "default" .=? selectOptionDefault
      ]

data TextInput = TextInput
  { 
    textInputCustomId :: T.Text,
    
    textInputIsParagraph :: Bool,
    
    textInputLabel :: T.Text,
    
    textInputMinLength :: Maybe Integer,
    
    textInputMaxLength :: Maybe Integer,
    
    textInputRequired :: Bool,
    
    textInputValue :: T.Text,
    
    textInputPlaceholder :: T.Text
  }
  deriving (Show, Read, Eq, Ord)

instance ToJSON TextInput where
  toJSON TextInput {..} =
    objectFromMaybes
      [ "type" .== Number 4,
        "custom_id" .== textInputCustomId,
        "style" .== (1 + fromEnum textInputIsParagraph),
        "label" .== textInputLabel,
        "min_length" .=? textInputMinLength,
        "max_length" .=? textInputMaxLength,
        "required" .== textInputRequired,
        "value" .== textInputValue,
        "placeholder" .== textInputPlaceholder
      ]

instance FromJSON TextInput where
  parseJSON = withObject "TextInput" $ \o -> do
    t <- o .: "type" :: Parser Int
    case t of
      4 ->
        TextInput <$> o .: "custom_id"
          <*> fmap (== (2 :: Int)) (o .:? "style" .!= 1)
          <*> o .:? "label" .!= ""
          <*> o .:? "min_length"
          <*> o .:? "max_length"
          <*> o .:? "required" .!= False
          <*> o .:? "value" .!= ""
          <*> o .:? "placeholder" .!= ""
      _ -> fail "expected text input, found other type of component"


mkTextInput :: T.Text -> T.Text -> TextInput
mkTextInput cid label = TextInput cid False label Nothing Nothing True "" ""
