{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text

import Control.Monad (when, void)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import UnliftIO (liftIO)
import UnliftIO.Concurrent

import Sigmacord
import Sigmacord.Types
import qualified Sigmacord.Requests as R

import ExampleUtils (getToken, getGuildId, actionWithChannelId)

-- Allows this code to be an executable. See Sigmacord-haskell.cabal
main :: IO ()
main = pingpongExample

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do
  tok <- getToken
  testserverid <- getGuildId

  -- open ghci and run  [[ :info RunSigmacordOpts ]] to see available fields
  err <- runSigmacord $ def { SigmacordToken = tok
                          , SigmacordOnStart = startHandler testserverid
                          , SigmacordOnEnd = liftIO $ threadDelay (round (0.4 * 10^6)) >>  putStrLn "Ended"
                          , SigmacordOnEvent = eventHandler
                          , SigmacordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
                          , SigmacordGatewayIntent = def {gatewayIntentMembers = True, gatewayIntentPresences =True}
                          }

  -- only reached on an unrecoverable error
  -- put normal 'cleanup' code in SigmacordOnEnd
  TIO.putStrLn err

-- If the start handler throws an exception, Sigmacord-haskell will gracefully shutdown
--     Use place to execute commands you know you want to complete
startHandler :: GuildId -> SigmacordHandler ()
startHandler testserverid = do
  liftIO $ putStrLn "Started ping-pong bot"

  let activity = def { activityName = "ping-pong"
                     , activityType = ActivityTypeGame
                     }
  let opts = UpdateStatusOpts { updateStatusOptsSince = Nothing
                              , updateStatusOptsGame = Just activity
                              , updateStatusOptsNewStatus = UpdateStatusOnline
                              , updateStatusOptsAFK = False
                              }
  sendCommand (UpdateStatus opts)

  actionWithChannelId testserverid $ \cid ->
    void $
      restCall $
        R.CreateMessage
          cid
          "Hello! I will reply to pings with pongs"


-- If an event handler throws an exception, Sigmacord-haskell will continue to run
eventHandler :: Event -> SigmacordHandler ()
eventHandler event = case event of
      MessageCreate m -> when (not (fromBot m) && isPing m) $ do
        void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
        threadDelay (2 * 10 ^ (6 :: Int))

        -- A very simple message.
        Right m' <- restCall (R.CreateMessage (messageChannelId m) "Pong")
        void $ restCall (R.EditMessage (messageChannelId m, messageId m') (def {R.messageDetailedContent=messageContent m' <> "!"}))

        latency <- getGatewayLatency
        mLatency <- measureLatency

        -- A more complex message. Text-to-speech, does not mention everyone nor
        -- the user, and uses Sigmacord native replies.
        -- Use ":info" in ghci to explore the type
        let opts :: R.MessageDetailedOpts
            opts = def { R.messageDetailedContent = "Here's a more complex message, but doesn't ping @everyone!. Here's the current gateway latency: " <> (T.pack . show) ([latency, mLatency])
                       , R.messageDetailedTTS = True
                       , R.messageDetailedAllowedMentions = Just $
                          def { R.mentionEveryone = False
                              , R.mentionRepliedUser = False
                              }
                       , R.messageDetailedReference = Just $
                          def { referenceMessageId = Just $ messageId m }
                       }
        void $ restCall (R.CreateMessageDetailed (messageChannelId m) opts)
      _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.toLower . messageContent
