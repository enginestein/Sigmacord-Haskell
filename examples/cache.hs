{-# LANGUAGE OverloadedStrings #-}

import UnliftIO (liftIO)

import Sigmacord

import ExampleUtils (getToken)

main :: IO ()
main = cacheExample

cacheExample :: IO ()
cacheExample = do
  tok <- getToken

  _ <- runDiscord $ def { discordToken = tok
                        , discordOnStart = do
                               cache <- readCache
                               liftIO $ putStrLn ("Cached info from gateway: " <> show cache)
                               stopDiscord
                        }
  pure ()

