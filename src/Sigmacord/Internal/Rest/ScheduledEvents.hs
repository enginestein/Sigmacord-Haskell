{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}


module Sigmacord.Internal.Rest.ScheduledEvents
    ( ScheduledEventRequest(..)
    ) where
import           Data.Aeson                     ( ToJSON(toJSON) )
import           Sigmacord.Internal.Rest.Prelude  ( JsonRequest(..)
                                                , Request
                                                    ( jsonRequest
                                                    , majorRoute
                                                    )
                                                , baseUrl
                                                )
import           Sigmacord.Internal.Types.Prelude ( GuildId
                                                , ScheduledEventId
                                                )
import           Sigmacord.Internal.Types.ScheduledEvents
                                                ( CreateScheduledEventData
                                                , ModifyScheduledEventData
                                                , ScheduledEvent
                                                , ScheduledEventUser
                                                )
import qualified Network.HTTP.Req              as R
import           Network.HTTP.Req               ( (/:), (/~) )



data ScheduledEventRequest a where
  
  ListScheduledEvents    ::GuildId
                         -> ScheduledEventRequest [ScheduledEvent]
  
  CreateScheduledEvent   ::GuildId
                         -> CreateScheduledEventData
                         -> ScheduledEventRequest ScheduledEvent
  
  GetScheduledEvent      ::GuildId
                         -> ScheduledEventId
                         -> ScheduledEventRequest ScheduledEvent
  
  ModifyScheduledEvent   ::GuildId
                         -> ScheduledEventId
                         -> ModifyScheduledEventData
                         -> ScheduledEventRequest ScheduledEvent
  
  DeleteScheduledEvent   ::GuildId
                         -> ScheduledEventId
                         -> ScheduledEventRequest ()
  
  GetScheduledEventUsers ::GuildId
                         -> ScheduledEventId
                         -> ScheduledEventRequest [ScheduledEventUser]

sevEndpoint :: GuildId -> R.Url 'R.Https
sevEndpoint gid = baseUrl /: "guilds" /~ gid /: "scheduled-events"

instance Request (ScheduledEventRequest a) where
    majorRoute = const "scheduledEvent"
    jsonRequest rq = case rq of
        ListScheduledEvents gid  -> Get (sevEndpoint gid) mempty
        GetScheduledEvent gid ev -> Get (sevEndpoint gid /~ ev) mempty
        CreateScheduledEvent gid ev ->
            Post (sevEndpoint gid) (pure $ R.ReqBodyJson $ toJSON ev) mempty
        ModifyScheduledEvent gid evi ev -> Patch
            (sevEndpoint gid /~ evi)
            (pure $ R.ReqBodyJson $ toJSON ev)
            mempty
        DeleteScheduledEvent gid evi -> Delete (sevEndpoint gid /~ evi) mempty
        GetScheduledEventUsers gid evi ->
            Get (sevEndpoint gid /~ evi /: "users") mempty
