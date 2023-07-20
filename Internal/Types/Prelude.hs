{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Sigmacord.Internal.Types.Prelude
  ( Auth (..)
  , authToken

  , Snowflake (..)
  , snowflakeCreationDate

  , RolePermissions (..)
  
  , SigmacordId (..)
  , ChannelId
  , StageId
  , GuildId
  , MessageId
  , AttachmentId
  , EmojiId
  , StickerId
  , UserId
  , RoleId
  , IntegrationId
  , WebhookId
  , ParentId
  , ApplicationId
  , ApplicationCommandId
  , InteractionId
  , ScheduledEventId
  , ScheduledEventEntityId

  , SigmacordToken (..)
  , InteractionToken
  , WebhookToken

  , Shard
  , epochTime

  , InternalSigmacordEnum (..)

  , Base64Image (..)
  , getMimeType

  , (.==)
  , (.=?)
  , AesonKey
  , objectFromMaybes

  , ChannelTypeOption (..)
  )

 where

import Data.Bifunctor (first)
import Data.Bits (Bits(shiftR))
import Data.Data (Data (dataTypeOf), dataTypeConstrs, fromConstr)
import Data.Word (Word64)
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)

import Data.Aeson.Types
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Web.Internal.HttpApiData

import qualified Data.ByteString as B
import qualified Data.Text as T

#if MIN_VERSION_aeson(2, 0, 0)
import qualified Data.Aeson.Key as Key
#endif


newtype Auth = Auth T.Text
  deriving (Show, Read, Eq, Ord)



authToken :: Auth -> T.Text
authToken (Auth tok) = let token = T.strip tok
                           bot = if "Bot " `T.isPrefixOf` token then "" else "Bot "
                       in bot <> token


newtype Snowflake = Snowflake { unSnowflake :: Word64 }
  deriving (Ord, Eq, Num, Integral, Enum, Real, Bits)

instance Show Snowflake where
  show (Snowflake a) = show a

instance Read Snowflake where
  readsPrec p = fmap (first Snowflake) . readsPrec p

instance ToJSON Snowflake where
  toJSON (Snowflake snowflake) = String . T.pack $ show snowflake

instance FromJSON Snowflake where
  parseJSON =
    withText
      "Snowflake"
      ( \snowflake ->
          case readMaybe (T.unpack snowflake) of
            Nothing -> fail "null snowflake"
            (Just i) -> pure i
      )

instance ToHttpApiData Snowflake where
  toUrlPiece = T.pack . show

newtype RolePermissions = RolePermissions { getRolePermissions :: Integer } 
  deriving (Eq, Ord, Num, Bits, Enum, Real, Integral)

instance Read RolePermissions where
  readsPrec p = fmap (first RolePermissions) . readsPrec p

instance ToJSON RolePermissions where
  toJSON = toJSON . getRolePermissions



instance FromJSON RolePermissions where
  parseJSON = withText "RolePermissions" $
      \text -> case readMaybe (T.unpack text) of
              Just perms -> pure $ RolePermissions perms
              Nothing    -> fail "invalid role permissions integer string"

instance Show RolePermissions where
  show = show . getRolePermissions

newtype SigmacordId a = SigmacordId { unId :: Snowflake }
  deriving (Ord, Eq, Num, Integral, Enum, Real, Bits)

instance Show (SigmacordId a) where
  show = show . unId

instance Read (SigmacordId a) where
  readsPrec p = fmap (first SigmacordId) . readsPrec p

instance ToJSON (SigmacordId a) where
  toJSON = toJSON . unId

instance FromJSON (SigmacordId a) where
  parseJSON = fmap SigmacordId . parseJSON

instance ToHttpApiData (SigmacordId a) where
  toUrlPiece = T.pack . show

data ChannelIdType
type ChannelId = SigmacordId ChannelIdType

data StageIdType
type StageId = SigmacordId StageIdType

data GuildIdType
type GuildId = SigmacordId GuildIdType

data MessageIdType
type MessageId = SigmacordId MessageIdType

data AttachmentIdType
type AttachmentId = SigmacordId AttachmentIdType

data EmojiIdType
type EmojiId = SigmacordId EmojiIdType

data StickerIdType
type StickerId = SigmacordId StickerIdType

data UserIdType
type UserId = SigmacordId UserIdType

data RoleIdType
type RoleId = SigmacordId RoleIdType

data IntegrationIdType
type IntegrationId = SigmacordId IntegrationIdType

data WebhookIdType
type WebhookId = SigmacordId WebhookIdType

data ParentIdType
type ParentId = SigmacordId ParentIdType

data ApplicationIdType
type ApplicationId = SigmacordId ApplicationIdType

data ApplicationCommandIdType
type ApplicationCommandId = SigmacordId ApplicationCommandIdType

data InteractionIdType
type InteractionId = SigmacordId InteractionIdType

data ScheduledEventIdType
type ScheduledEventId = SigmacordId ScheduledEventIdType

data ScheduledEventEntityIdType
type ScheduledEventEntityId = SigmacordId ScheduledEventEntityIdType

newtype SigmacordToken a = SigmacordToken { unToken :: T.Text }
  deriving (Ord, Eq)

instance Show (SigmacordToken a) where
  show = show . unToken

instance Read (SigmacordToken a) where
  readsPrec p = fmap (first SigmacordToken) . readsPrec p

instance ToJSON (SigmacordToken a) where
  toJSON = toJSON . unToken

instance FromJSON (SigmacordToken a) where
  parseJSON = fmap SigmacordToken . parseJSON

instance ToHttpApiData (SigmacordToken a) where
  toUrlPiece = unToken

type InteractionToken = SigmacordToken InteractionIdType

type WebhookToken = SigmacordToken WebhookIdType

type Shard = (Int, Int)


snowflakeCreationDate :: Snowflake -> UTCTime
snowflakeCreationDate x = posixSecondsToUTCTime . realToFrac
  $ 1420070400 + quot (shiftR x 22) 1000


epochTime :: UTCTime
epochTime = posixSecondsToUTCTime 0

{-

InternalSigmacordEnum is a hack-y typeclass, but it's the best solution overall.
The best we can do is prevent the end-user from seeing this.

typeclass Bounded (minBound + maxBound) could replace SigmacordTypeStartValue, but
it can't derive instances for types like SigmacordColor, which have simple sum types involved.

typeclass Enum (toEnum + fromEnum) requires defining both A->Int and Int->A.
If we handle both at once (with an inline map), it's no longer typesafe.

External packages exist, but bloat our dependencies

-}
class Data a => InternalSigmacordEnum a where
  SigmacordTypeStartValue :: a
  fromSigmacordType :: a -> Int
  SigmacordTypeTable :: [(Int, a)]
  SigmacordTypeTable =  map (\d -> (fromSigmacordType d, d)) (makeTable SigmacordTypeStartValue)
    where
      makeTable :: Data b => b -> [b]
      makeTable t = map fromConstr (dataTypeConstrs $ dataTypeOf t)

  SigmacordTypeParseJSON :: String -> Value -> Parser a
  SigmacordTypeParseJSON name =
    withScientific
      name
      ( \i -> do
          case maybeInt i >>= (`lookup` SigmacordTypeTable) of
            Nothing -> fail $ "could not parse type: " ++ show i
            Just d -> return d
      )
    where
      maybeInt i
        | fromIntegral (round i) == i = Just $ round i
        | otherwise = Nothing






#if MIN_VERSION_aeson(2, 0, 0)
type AesonKey = Key.Key
#else
type AesonKey = T.Text
#endif


(.==) :: ToJSON a => AesonKey -> a -> Maybe Pair
k .== v = Just (k .= v)

(.=?) :: ToJSON a => AesonKey -> Maybe a -> Maybe Pair
k .=? (Just v) = Just (k .= v)
_ .=? Nothing = Nothing

objectFromMaybes :: [Maybe Pair] -> Value
objectFromMaybes = object . catMaybes








data Base64Image a = Base64Image T.Text T.Text
  deriving (Show, Read, Eq, Ord)





instance ToJSON (Base64Image a) where
  toJSON (Base64Image mime im) = String $ "data:" <> mime <> ";base64," <> im












getMimeType :: B.ByteString -> Maybe T.Text
getMimeType bs
  | B.take 8 bs == "\x89\x50\x4E\x47\x0D\x0A\x1A\x0A"
  = Just "image/png"
  | B.take 3 bs == "\xff\xd8\xff" || B.take 4 (B.drop 6 bs) `elem` ["JFIF", "Exif"]
  = Just "image/jpeg"
  | B.take 6 bs == "\x47\x49\x46\x38\x37\x61" || B.take 6 bs == "\x47\x49\x46\x38\x39\x61"
  = Just "image/gif"
  | B.take 4 bs == "RIFF" && B.take 4 (B.drop 8 bs) == "WEBP"
  = Just "image/webp"
  | otherwise = Nothing




data ChannelTypeOption
  = 
    ChannelTypeOptionGuildText
  | 
    ChannelTypeOptionDM
  | 
    ChannelTypeOptionGuildVoice
  | 
    ChannelTypeOptionGroupDM
  | 
    ChannelTypeOptionGuildCategory
  | 
    ChannelTypeOptionGuildNews
  | 
    ChannelTypeOptionGuildStore
  | 
    ChannelTypeOptionGuildNewsThread
  | 
    ChannelTypeOptionGuildPublicThread
  | 
    
    ChannelTypeOptionGuildPrivateThread
  | 
    ChannelTypeOptionGuildStageVoice
  deriving (Show, Read, Data, Eq, Ord)

instance InternalSigmacordEnum ChannelTypeOption where
  SigmacordTypeStartValue = ChannelTypeOptionGuildText
  fromSigmacordType ChannelTypeOptionGuildText = 0
  fromSigmacordType ChannelTypeOptionDM = 1
  fromSigmacordType ChannelTypeOptionGuildVoice = 2
  fromSigmacordType ChannelTypeOptionGroupDM = 3
  fromSigmacordType ChannelTypeOptionGuildCategory = 4
  fromSigmacordType ChannelTypeOptionGuildNews = 5
  fromSigmacordType ChannelTypeOptionGuildStore = 6
  fromSigmacordType ChannelTypeOptionGuildNewsThread = 10
  fromSigmacordType ChannelTypeOptionGuildPublicThread = 11
  fromSigmacordType ChannelTypeOptionGuildPrivateThread = 12
  fromSigmacordType ChannelTypeOptionGuildStageVoice = 13

instance ToJSON ChannelTypeOption where
  toJSON = toJSON . fromSigmacordType

instance FromJSON ChannelTypeOption where
  parseJSON = SigmacordTypeParseJSON "ChannelTypeOption"
