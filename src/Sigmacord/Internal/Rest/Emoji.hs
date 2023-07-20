{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Sigmacord.Internal.Rest.Emoji
  ( EmojiRequest (..),
    ModifyGuildEmojiOpts (..),
    parseEmojiImage,
    parseStickerImage,
    StickerRequest (..),
    CreateGuildStickerOpts (..),
    EditGuildStickerOpts (..)
  )
where

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Sigmacord.Internal.Rest.Prelude
import Sigmacord.Internal.Types
import Network.HTTP.Req ((/:), (/~))
import qualified Network.HTTP.Req as R

instance Request (EmojiRequest a) where
  majorRoute = emojiMajorRoute
  jsonRequest = emojiJsonRequest


data EmojiRequest a where
  
  ListGuildEmojis :: GuildId -> EmojiRequest [Emoji]
  
  GetGuildEmoji :: GuildId -> EmojiId -> EmojiRequest Emoji
  
  CreateGuildEmoji :: GuildId -> T.Text -> Base64Image Emoji -> EmojiRequest Emoji
  
  ModifyGuildEmoji :: GuildId -> EmojiId -> ModifyGuildEmojiOpts -> EmojiRequest Emoji
  
  DeleteGuildEmoji :: GuildId -> EmojiId -> EmojiRequest ()

data ModifyGuildEmojiOpts = ModifyGuildEmojiOpts
  { modifyGuildEmojiName :: T.Text,
    modifyGuildEmojiRoles :: [RoleId]
  }
  deriving (Show, Read, Eq, Ord)

instance ToJSON ModifyGuildEmojiOpts where
  toJSON (ModifyGuildEmojiOpts name roles) =
    object ["name" .= name, "roles" .= roles]










parseEmojiImage :: B.ByteString -> Either T.Text (Base64Image Emoji)
parseEmojiImage bs
  | B.length bs > 256000        = Left "Cannot create emoji - File is larger than 256kb"
  | Just mime <- getMimeType bs = Right (Base64Image mime (TE.decodeUtf8 (B64.encode bs)))
  | otherwise                   = Left "Unsupported image format provided"

emojiMajorRoute :: EmojiRequest a -> String
emojiMajorRoute c = case c of
  (ListGuildEmojis g) -> "emoji " <> show g
  (GetGuildEmoji g _) -> "emoji " <> show g
  (CreateGuildEmoji g _ _) -> "emoji " <> show g
  (ModifyGuildEmoji g _ _) -> "emoji " <> show g
  (DeleteGuildEmoji g _) -> "emoji " <> show g

guilds :: R.Url 'R.Https
guilds = baseUrl /: "guilds"

emojiJsonRequest :: EmojiRequest r -> JsonRequest
emojiJsonRequest c = case c of
  (ListGuildEmojis g) -> Get (guilds /~ g /: "emojis") mempty
  (GetGuildEmoji g e) -> Get (guilds /~ g /: "emojis" /~ e) mempty
  (CreateGuildEmoji g name b64im) ->
    Post
      (guilds /~ g /: "emojis")
      ( pure
          ( R.ReqBodyJson
              ( object
                  [ "name" .= name,
                    "image" .= b64im
                    
                  ]
              )
          )
      )
      mempty
  (ModifyGuildEmoji g e o) ->
    Patch
      (guilds /~ g /: "emojis" /~ e)
      (pure (R.ReqBodyJson o))
      mempty
  (DeleteGuildEmoji g e) -> Delete (guilds /~ g /: "emojis" /~ e) mempty






parseStickerImage :: B.ByteString -> Either T.Text (Base64Image Sticker)
parseStickerImage bs
  | B.length bs > 512000
  = Left "Cannot create sticker - File is larger than 512kb"
  | Just "image/png" <- getMimeType bs
  = Right (Base64Image "image/png" (TE.decodeUtf8 (B64.encode bs)))
  | not (B.null bs) && B.head bs == 0x7b 
  = Right (Base64Image "application/json" (TE.decodeUtf8 (B64.encode bs)))
  | otherwise
  = Left "Unsupported image format provided"


data CreateGuildStickerOpts = CreateGuildStickerOpts
  { guildStickerName :: T.Text,
    guildStickerDescription :: T.Text,
    guildStickerTags :: [T.Text],
    guildStickerFile :: Base64Image Sticker
  }
  deriving (Show, Read, Eq, Ord)

instance ToJSON CreateGuildStickerOpts where
  toJSON (CreateGuildStickerOpts name desc tags b64im) =
    object
      [ ("name", toJSON name),
        ("description", toJSON desc),
        ("tags", toJSON $ T.intercalate "," tags),
        ("file", toJSON b64im)
      ]


data EditGuildStickerOpts = EditGuildStickerOpts
  { editGuildStickerName :: Maybe T.Text,
    editGuildStickerDescription :: Maybe T.Text,
    editGuildStickerTags :: Maybe [T.Text]
  }
  deriving (Show, Read, Eq, Ord)

instance ToJSON EditGuildStickerOpts where
  toJSON EditGuildStickerOpts {..} =
    objectFromMaybes
      [ "name" .=? editGuildStickerName,
        "description" .=? editGuildStickerDescription,
        "tags" .=? fmap (T.intercalate ",") editGuildStickerTags
      ]

instance Request (StickerRequest a) where
  majorRoute = stickerMajorRoute
  jsonRequest = stickerJsonRequest





data StickerRequest a where
  
  GetSticker :: StickerId -> StickerRequest Sticker
  
  ListNitroStickerPacks :: StickerRequest [StickerPack]
  
  ListGuildStickers :: GuildId -> StickerRequest [Sticker]
  
  GetGuildSticker :: GuildId -> StickerId -> StickerRequest Sticker
  
  CreateGuildSticker :: GuildId -> CreateGuildStickerOpts -> StickerRequest Sticker
  
  ModifyGuildSticker :: GuildId -> StickerId -> EditGuildStickerOpts -> StickerRequest Sticker
  
  DeleteGuildSticker :: GuildId -> StickerId -> StickerRequest ()

stickerMajorRoute :: StickerRequest a -> String
stickerMajorRoute = \case
  GetSticker gid -> "sticker " <> show gid
  ListNitroStickerPacks -> "sticker"
  ListGuildStickers gid -> "sticker " <> show gid
  GetGuildSticker gid _ -> "sticker " <> show gid
  CreateGuildSticker gid _ -> "sticker " <> show gid
  ModifyGuildSticker gid _ _ -> "sticker " <> show gid
  DeleteGuildSticker gid _ -> "sticker " <> show gid

stickerJsonRequest :: StickerRequest a -> JsonRequest
stickerJsonRequest = \case
  GetSticker gid -> Get (baseUrl /: "stickers" /~ gid) mempty
  ListNitroStickerPacks -> Get (baseUrl /: "sticker-packs") mempty
  ListGuildStickers gid -> Get (stickersGuild gid) mempty
  GetGuildSticker gid sid -> Get (stickersGuild gid /~ sid) mempty
  CreateGuildSticker gid cgso -> Post (stickersGuild gid) (pure $ R.ReqBodyJson $ toJSON cgso) mempty
  ModifyGuildSticker gid sid egso -> Patch (stickersGuild gid /~ sid) (pure $ R.ReqBodyJson egso) mempty
  DeleteGuildSticker gid sid -> Delete (stickersGuild gid /~ sid) mempty
  where
    stickersGuild gid = baseUrl /: "guilds" /~ gid /: "stickers"
