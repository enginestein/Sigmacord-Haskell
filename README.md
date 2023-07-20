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
    userFacingError <- runDiscord $ def
        { discordToken = "TOKEN"
        , discordOnEvent = eventHandler
        , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
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

# Documentation

To read documentation you can go [here](https://enginestein.github.io/Sigmacord) or you can visit official [Discord documentation](https://discord.com/developers/docs/intro), Sigmacord and official Discord documentation are pretty much same (as it is just an API wrapper)

# Support

You can join [Discord server](https://discord.gg/GMeBhcvcq7) for support regarding Discord bot development in Sigmacord.
