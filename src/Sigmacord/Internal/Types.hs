module Sigmacord.Internal.Types
  ( module Sigmacord.Internal.Types.Prelude,
    module Sigmacord.Internal.Types.Channel,
    module Sigmacord.Internal.Types.Color,
    module Sigmacord.Internal.Types.Events,
    module Sigmacord.Internal.Types.Gateway,
    module Sigmacord.Internal.Types.Guild,
    module Sigmacord.Internal.Types.User,
    module Sigmacord.Internal.Types.Embed,
    module Sigmacord.Internal.Types.Components,
    module Sigmacord.Internal.Types.Emoji,
    module Sigmacord.Internal.Types.RolePermissions,
    module Data.Aeson,
    module Data.Time.Clock,
    userFacingEvent,
  )
where

import Data.Aeson (Object, ToJSON (toJSON))
import Data.Time.Clock (UTCTime (..))
import Sigmacord.Internal.Types.Channel
import Sigmacord.Internal.Types.Color
import Sigmacord.Internal.Types.Components
import Sigmacord.Internal.Types.Embed
import Sigmacord.Internal.Types.Emoji
import Sigmacord.Internal.Types.Events
import Sigmacord.Internal.Types.Gateway
import Sigmacord.Internal.Types.Guild
import Sigmacord.Internal.Types.Prelude
import Sigmacord.Internal.Types.User
import Sigmacord.Internal.Types.RolePermissions


userFacingEvent :: EventInternalParse -> Event
userFacingEvent event = case event of
  InternalReady a b c d e f g -> Ready a b c d e f g
  InternalResumed a -> Resumed a
  InternalChannelCreate a -> ChannelCreate a
  InternalChannelUpdate a -> ChannelUpdate a
  InternalChannelDelete a -> ChannelDelete a
  InternalThreadCreate a -> ThreadCreate a
  InternalThreadUpdate a -> ThreadUpdate a
  InternalThreadDelete a -> ThreadDelete a
  InternalThreadListSync a -> ThreadListSync a
  InternalThreadMembersUpdate a -> ThreadMembersUpdate a
  InternalChannelPinsUpdate a b -> ChannelPinsUpdate a b
  InternalGuildCreate a b -> GuildCreate a b
  InternalGuildUpdate a -> GuildUpdate a
  InternalGuildDelete a -> GuildDelete a
  InternalGuildBanAdd a b -> GuildBanAdd a b
  InternalGuildBanRemove a b -> GuildBanRemove a b
  InternalGuildEmojiUpdate a b -> GuildEmojiUpdate a b
  InternalGuildIntegrationsUpdate a -> GuildIntegrationsUpdate a
  InternalGuildMemberAdd a b -> GuildMemberAdd a b
  InternalGuildMemberRemove a b -> GuildMemberRemove a b
  InternalGuildMemberUpdate a b c d -> GuildMemberUpdate a b c d
  InternalGuildMemberChunk a b -> GuildMemberChunk a b
  InternalGuildRoleCreate a b -> GuildRoleCreate a b
  InternalGuildRoleUpdate a b -> GuildRoleUpdate a b
  InternalGuildRoleDelete a b -> GuildRoleDelete a b
  InternalMessageCreate a -> MessageCreate a
  InternalMessageUpdate a b -> MessageUpdate a b
  InternalMessageDelete a b -> MessageDelete a b
  InternalMessageDeleteBulk a b -> MessageDeleteBulk a b
  InternalMessageReactionAdd a -> MessageReactionAdd a
  InternalMessageReactionRemove a -> MessageReactionRemove a
  InternalMessageReactionRemoveAll a b -> MessageReactionRemoveAll a b
  InternalMessageReactionRemoveEmoji a -> MessageReactionRemoveEmoji a
  InternalPresenceUpdate a -> PresenceUpdate a
  InternalTypingStart a -> TypingStart a
  InternalUserUpdate a -> UserUpdate a
  InternalInteractionCreate a -> InteractionCreate a
  InternalUnknownEvent a b -> UnknownEvent a b
