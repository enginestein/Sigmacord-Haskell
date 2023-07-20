# Sigmacord

Meet Sigmacord - the efficient Haskell framework for developing powerful Discord bots. Embrace Haskell's speed, safety, and reliability to create seamless user experiences with robust performance. 

# Installation

You can get Sigmacord latest version from [Hackage](https://hackage.haskell.org/package/sigmacord), below are various methods for installation:

#### Stack

For `stack.yaml`

```yaml
extra-deps:
- emoji-0.1.0.2
- sigmacord-1.0.0
```

For `project.cabal`

```cabal
executable sigmacord-bot
  main-is:             src/Main.hs
  default-language:    Haskell2010
  ghc-options:         -threaded
  build-depends:       base
                     , text
                     , sigmacord
```

#### Cabal

For `project.cabal`

```cabal
cabal-version:       2.0
name:                sigmacord-bot
version:             1.0.0
build-type:          Simple

executable haskell-bot
  main-is:             src/Main.hs
  default-language:    Haskell2010
  ghc-options:         -threaded
  build-depends:       base
                     , text
                     , sigmacord == 1.0.0
```

# Usage

Below is an example usage of how to use Sigmacord in your code.

```haskell

{-# LANGUAGE OverloadedStrings #-}  
import           Control.Monad (when, void)
import           UnliftIO.Concurrent
import           Data.Text (isPrefixOf, toLower, Text)
import qualified Data.Text.IO as TIO

import           Sigmacord
import           Sigmacord.Types
import qualified Sigmacord.Requests as R

-- | Replies "pong" to every message that starts with "ping"
pingPongExample :: IO ()
pingPongExample = do
    userFacingError <- runSigmacord $ def
        { SigmacordToken = "TOKEN"
        , SigmacordOnEvent = eventHandler
        , SigmacordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
        } -- if you see OnLog error, post in the discord / open an issue

    TIO.putStrLn userFacingError
    -- userFacingError is an unrecoverable error
    -- put normal 'cleanup' code in discordOnEnd (see examples)

eventHandler :: Event -> SigmacordHandler ()
eventHandler event = case event of
    MessageCreate msg -> when (isPing msg && not (fromBot msg)) $ do
        void $ restCall (R.CreateReaction (messageChannelId msg, messageId msg) "eyes")
        threadDelay (2 * 10^6)
        void $ restCall (R.CreateMessage (messageChannelId msg) "Pong!")
    _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `isPrefixOf`) . toLower . messageContent
```

# Error Handling

```haskell
example :: IO ()
example = do userFacingError <- Sigmacord $ def
                 { SigmacordToken = "TOKEN"

                 , SigmacordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
                 }

             TIO.putStrLn userFacingError
```

1. Always print the `userFacingError` Text returned from `runSigmacord`. This is used for errors that cannot be recovered from.

2. For debugging purposes, use the `SigmacordOnLog` handler to print information as it happens.

### Reporting Issues

If something else goes wrong with the library and it's not recoverable or debuggable on your end, please consider opening an issue on the project's repository. Attaching a log to the issue can be very helpful for the developers to diagnose the problem.

### Handling Logs with `SigmacordOnLog`

To enable real-time logging of information, assign a handler to the `SigmacordOnLog` function with the type signature `SigmacordOnLog :: Text -> IO ()`. This function will be called whenever there is a new log entry. Before posting any logs or error messages, make sure to remove any sensitive information to protect your privacy and security.

# Documentation

To read documentation you can go [here](https://enginestein.github.io/Sigmacord) or you can visit official [Discord documentation](https://discord.com/developers/docs/intro), Sigmacord and official Discord documentation are pretty much same (as it is just an API wrapper)

# Support

You can join [Discord server](https://discord.gg/GMeBhcvcq7) for support regarding Discord bot development in Sigmacord.
