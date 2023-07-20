{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sigmacord.Internal.Types.Emoji where

import Data.Aeson
import Data.Data
import Data.Functor ((<&>))
import Data.Text as T
import Sigmacord.Internal.Types.Prelude
import Sigmacord.Internal.Types.User


data Emoji = Emoji
  { 
    emojiId :: Maybe EmojiId,
    
    emojiName :: T.Text,
    
    emojiRoles :: Maybe [RoleId],
    
    emojiUser :: Maybe User,
    
    emojiManaged :: Maybe Bool,
    
    emojiAnimated :: Maybe Bool
  }
  deriving (Show, Read, Eq, Ord)


mkEmoji :: T.Text -> Emoji
mkEmoji t = Emoji Nothing t Nothing Nothing Nothing Nothing

instance FromJSON Emoji where
  parseJSON = withObject "Emoji" $ \o ->
    Emoji <$> o .:? "id"
      <*> o .: "name"
      <*> o .:? "roles"
      <*> o .:? "user"
      <*> o .:? "managed"
      <*> o .:? "animated"

instance ToJSON Emoji where
  toJSON Emoji {..} =
    objectFromMaybes
            [ "id" .=? emojiId,
              "name" .== emojiName,
              "roles" .=? emojiRoles,
              "user" .=? emojiUser,
              "managed" .=? emojiManaged,
              "animated" .=? emojiAnimated
            ]


data StickerPack = StickerPack
  { 
    stickerPackId :: Snowflake,
    
    stickerPackStickers :: [Sticker],
    
    stickerPackName :: T.Text,
    
    stickerPackSKUId :: Snowflake,
    
    stickerPackCoverStickerId :: Maybe StickerId,
    
    stickerPackDescription :: T.Text,
    
    stickerPackBannerAssetId :: Maybe Snowflake
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON StickerPack where
  parseJSON = withObject "StickerPack" $ \o ->
    StickerPack <$> o .: "id"
      <*> o .: "stickers"
      <*> o .: "name"
      <*> o .: "sku_id"
      <*> o .:? "cover_sticker_id"
      <*> o .: "description"
      <*> o .:? "banner_asset_id"


data Sticker = Sticker
  { 
    stickerId :: StickerId,
    
    stickerStickerPackId :: Maybe Snowflake,
    
    stickerName :: T.Text,
    
    stickerDescription :: Maybe T.Text,
    
    stickerTags :: [T.Text],
    
    stickerIsStandardType :: Bool,
    
    stickerFormatType :: StickerFormatType,
    
    stickerAvailable :: Maybe Bool,
    
    stickerGuildId :: Maybe GuildId,
    
    stickerUser :: Maybe User,
    
    stickerSortValue :: Maybe Integer
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON Sticker where
  parseJSON = withObject "Sticker" $ \o ->
    Sticker <$> o .: "id"
      <*> o .:? "pack_id"
      <*> o .:  "name"
      <*> o .:? "description"
      <*> ((o .: "tags") <&> T.splitOn "\n")
      <*> ((o .: "type") <&> (== (1 :: Int)))
      <*> o .: "format_type"
      <*> o .:? "available"
      <*> o .:? "guild_id"
      <*> o .:? "user"
      <*> o .:? "sort_value"


data StickerItem = StickerItem
  { 
    stickerItemId :: StickerId,
    
    stickerItemName :: T.Text,
    
    stickerItemFormatType :: StickerFormatType
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON StickerItem where
  parseJSON = withObject "StickerItem" $ \o ->
    StickerItem <$> o .: "id"
      <*> o .: "name"
      <*> o .: "format_type"

instance ToJSON StickerItem where
  toJSON StickerItem {..} =
    object
      [ ("id", toJSON stickerItemId),
        ("name", toJSON stickerItemName),
        ("format_type", toJSON stickerItemFormatType)
      ]


data StickerFormatType
  = StickerFormatTypePNG
  | StickerFormatTypeAPNG
  | StickerFormatTypeLOTTIE
  deriving (Show, Read, Eq, Ord, Data)

instance InternalSigmacordEnum StickerFormatType where
  SigmacordTypeStartValue = StickerFormatTypePNG
  fromSigmacordType StickerFormatTypePNG = 1
  fromSigmacordType StickerFormatTypeAPNG = 2
  fromSigmacordType StickerFormatTypeLOTTIE = 3

instance ToJSON StickerFormatType where
  toJSON = toJSON . fromSigmacordType

instance FromJSON StickerFormatType where
  parseJSON = SigmacordTypeParseJSON "StickerFormatType"
