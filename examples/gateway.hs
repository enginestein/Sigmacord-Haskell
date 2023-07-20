{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forever)
import Control.Concurrent (forkIO, killThread)
import UnliftIO (liftIO)
import Control.Concurrent.Chan
import qualified Data.Text.IO as TIO

import Sigmacord
import Sigmacord.Types

import ExampleUtils (getToken, getGuildId)

main :: IO ()
main = gatewayExample

-- | Prints every event as it happens
gatewayExample :: IO ()
gatewayExample = do
  tok <- getToken
  testserverid <- getGuildId

  outChan <- newChan :: IO (Chan String)

  -- Events are processed in new threads, but stdout isn't
  -- synchronized. We get ugly output when multiple threads
  -- write to stdout at the same time
  threadId <- forkIO $ forever $ readChan outChan >>= putStrLn

  err <- runSigmacord $ def { SigmacordToken = tok
                          , SigmacordOnStart = startHandler testserverid
                          , SigmacordOnEvent = eventHandler outChan
                          , SigmacordOnEnd = killThread threadId
                          }
  TIO.putStrLn err

-- Events are enumerated in the Sigmacord docs
-- https://Sigmacord.com/developers/docs/topics/gateway#commands-and-events-gateway-events
eventHandler :: Chan String -> Event -> SigmacordHandler ()
eventHandler out event = liftIO $ writeChan out (show event <> "\n")


startHandler :: GuildId -> SigmacordHandler ()
startHandler testserverid = do
  let opts = RequestGuildMembersOpts
        { requestGuildMembersOptsGuildId = testserverid
        , requestGuildMembersOptsLimit = 100
        , requestGuildMembersOptsNamesStartingWith = ""
        }

  -- gateway commands are enumerated in the Sigmacord docs
  -- https://Sigmacord.com/developers/docs/topics/gateway#commands-and-events-gateway-commands
  sendCommand (RequestGuildMembers opts)
