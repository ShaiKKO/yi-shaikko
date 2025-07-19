{-# LANGUAGE OverloadedStrings #-}

-- | Example configuration for Acme keymap
-- Production-ready: Yes
-- Stability: Stable

import Yi
import Yi.Config.Default.Acme
import Yi.Keymap.Acme

main :: IO ()
main = yi $ myConfig

myConfig :: Config
myConfig = configureAcme >> defaultConfig
  { defaultKm = mkKeymapSet myAcmeConfig
  , configWindowFill = '░'
  , startActions = [printMsg "Acme mode - Use mouse chords!"]
  }

myAcmeConfig :: AcmeConfig
myAcmeConfig = defaultAcmeConfig
  { acmePlumbCommand = "plumb"  -- Use Plan 9 plumber if available
  , acmeAutoIndent = True
  , acmeTabSize = 4
  , acmePlumbRules = customPlumbRules
  }

-- | Custom plumbing rules for your environment
customPlumbRules :: [PlumbRule]
customPlumbRules = defaultPlumbRules ++
  [ PlumbRule isGitHash $ \hash -> 
      void $ startSubprocess ("git show " ++ hash) handleGitOutput
  , PlumbRule isTicket $ \ticket ->
      void $ startSubprocess ("xdg-open https://tracker.example.com/" ++ ticket) (const $ return ())
  ]
  where
    isGitHash s = length s == 40 && all isHexDigit s
    isTicket s = "TICKET-" `isPrefixOf` s
    
    handleGitOutput output = withEditor $ do
      -- Show git output in new buffer
      newBufferE (MemBuffer "*git*") output

-- | Example of runtime keymap switching with Acme
acmeWithKeymapSwitch :: Config
acmeWithKeymapSwitch = myConfig
  { defaultKm = mkKeymapSet $ myAcmeConfig
      { acmePlumbRules = defaultPlumbRules ++ 
          [ PlumbRule (== ":keymap") $ \_ -> do
              -- Special plumb rule to switch keymaps
              keymaps <- listKeymapsCmd
              km <- promptKeymap
              switchKeymapCmd km
          ]
      }
  }