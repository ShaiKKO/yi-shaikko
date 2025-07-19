{-# LANGUAGE OverloadedStrings #-}

-- | Example configuration for runtime keymap switching
-- Shows how to integrate dynamic keymap management

import Yi
import Yi.Config
import Yi.Keymap.Runtime
import qualified Yi.Keymap.Vim as Vim
import qualified Yi.Keymap.Emacs as Emacs
import qualified Yi.Keymap.Cua as Cua
import qualified Yi.Keymap.Acme as Acme
import qualified Yi.Keymap.Joe as Joe
import qualified Yi.Keymap.Mg as Mg
import qualified Yi.Keymap.Ee as Ee

main :: IO ()
main = yi $ myConfig

-- | Configuration with runtime keymap switching enabled
myConfig :: Config
myConfig = defaultConfig
  { defaultKm = Vim.keymapSet  -- Start with Vim
  , startActions = 
      [ registerRuntimeCommands
      , printMsg "Runtime keymap switching enabled - :set keymap <name>"
      , setupKeymapIndicator
      ]
  , configTheme = themeWithKeymapIndicator
  }

-- | Setup modeline to show current keymap
setupKeymapIndicator :: YiM ()
setupKeymapIndicator = do
  -- Add keymap indicator to modeline
  modifyModeLineFormat $ \fmt -> fmt ++ " %k"
  where
    modifyModeLineFormat f = return ()  -- Would modify modeline format

-- | Theme that highlights current keymap
themeWithKeymapIndicator :: Theme
themeWithKeymapIndicator = defaultTheme
  { modelineAttributes = emptyAttributes
      { foreground = white
      , background = darkBlue
      }
  , modelineCurrentBuffer = withBg brightBlue
  }
  where
    darkBlue = RGB 0 0 139
    brightBlue = RGB 30 144 255
    white = RGB 255 255 255

-- | Example of buffer-local keymap configuration
bufferLocalKeymaps :: Config
bufferLocalKeymaps = myConfig
  { modeTable = 
      [ -- Use Vim for code files
        AnyMode $ Mode 
          { modeName = "haskell"
          , modeKeymap = Vim.keymapSet
          , modeHL = haskellHighlighter
          }
        -- Use Emacs for org files
      , AnyMode $ Mode
          { modeName = "org"
          , modeKeymap = Emacs.keymapSet
          , modeHL = orgHighlighter
          }
        -- Use CUA for markdown
      , AnyMode $ Mode
          { modeName = "markdown"
          , modeKeymap = Cua.keymapSet
          , modeHL = markdownHighlighter
          }
      ]
  }

-- | Configuration with keymap auto-detection
autoDetectKeymap :: Config
autoDetectKeymap = myConfig
  { startActions = startActions myConfig ++
      [ detectUserPreference ]
  }
  where
    detectUserPreference = do
      -- Check for .vimrc, .emacs, etc.
      home <- io $ getHomeDirectory
      vimrc <- io $ doesFileExist (home </> ".vimrc")
      emacs <- io $ doesFileExist (home </> ".emacs")
      
      case (vimrc, emacs) of
        (True, False) -> switchKeymapCmd "vim"
        (False, True) -> switchKeymapCmd "emacs"
        _ -> switchKeymapCmd "cua"  -- Default to CUA

-- | Example of custom keymap switching bindings
keymapSwitchBindings :: Keymap
keymapSwitchBindings = choice
  [ -- Quick switch bindings
    ctrl (alt $ char '1') ?>>! switchKeymapCmd "vim"
  , ctrl (alt $ char '2') ?>>! switchKeymapCmd "emacs"
  , ctrl (alt $ char '3') ?>>! switchKeymapCmd "cua"
  , ctrl (alt $ char '4') ?>>! switchKeymapCmd "acme"
  , ctrl (alt $ char '5') ?>>! switchKeymapCmd "joe"
  , ctrl (alt $ char '6') ?>>! switchKeymapCmd "mg"
  , ctrl (alt $ char '7') ?>>! switchKeymapCmd "ee"
  
    -- Keymap management
  , ctrl (alt $ char 'k') ?>>! listKeymapsCmd
  , ctrl (alt $ char '?') ?>>! currentKeymapCmd
  ]

-- | Configuration with persistent keymap preference
persistentKeymapConfig :: Config
persistentKeymapConfig = myConfig
  { startActions = startActions myConfig ++
      [ loadKeymapPreference
      , onExit saveKeymapPreference
      ]
  }
  where
    prefFile = "~/.config/yi/keymap-preference"
    
    loadKeymapPreference = do
      exists <- io $ doesFileExist prefFile
      when exists $ do
        pref <- io $ readFile prefFile
        switchKeymapCmd (T.pack $ trim pref)
    
    saveKeymapPreference = do
      current <- getCurrentKeymapName
      io $ writeFile prefFile (T.unpack current)
    
    onExit action = return ()  -- Would register exit handler

-- | Example of dynamic keymap editing
dynamicKeymapConfig :: Config
dynamicKeymapConfig = myConfig
  { defaultKm = customizableKeymap Vim.keymapSet
  }
  where
    customizableKeymap base = base
      { topKeymap = userBindings >> topKeymap base
      }
    
    userBindings = do
      -- Load user's custom bindings from file
      bindings <- io $ loadUserBindings
      executeUserBindings bindings
    
    loadUserBindings = return []  -- Would load from ~/.yi/bindings.hs
    executeUserBindings _ = return ()

-- | Simulate runtime keymap switching in buffer
simulateKeymapSwitch :: YiM ()
simulateKeymapSwitch = do
  -- Create demo buffer
  newBufferE (MemBuffer "*keymap-demo*") ""
  
  -- Simulate switching through keymaps
  withCurrentBuffer $ do
    insertN "=== Keymap Switching Demo ===\n\n"
    
    -- Vim mode
    insertN "1. Vim mode active\n"
    insertN "   - Modal editing (Normal/Insert/Visual)\n"
    insertN "   - :set keymap emacs (to switch)\n\n"
    
    -- Switch to Emacs
    switchKeymapCmd "emacs"
    insertN "2. Emacs mode active\n"
    insertN "   - Modifier-based (C-x, M-x)\n"
    insertN "   - M-x switch-keymap (to switch)\n\n"
    
    -- Switch to CUA
    switchKeymapCmd "cua"
    insertN "3. CUA mode active\n"
    insertN "   - Standard shortcuts (Ctrl+C/V/X)\n"
    insertN "   - Familiar Windows/Linux bindings\n\n"
    
    insertN "Current keymap shown in modeline: "
    km <- keymapModeline
    insertN km

-- Helper functions for examples
doesFileExist :: FilePath -> IO Bool
doesFileExist = error "doesFileExist not implemented"

getHomeDirectory :: IO FilePath
getHomeDirectory = error "getHomeDirectory not implemented"

(</>) :: FilePath -> FilePath -> FilePath
(</>) = error "(</>) not implemented"

trim :: String -> String
trim = error "trim not implemented"

getCurrentKeymapName :: YiM Text
getCurrentKeymapName = error "getCurrentKeymapName not implemented"

haskellHighlighter :: Highlighter a b
haskellHighlighter = error "haskellHighlighter not implemented"

orgHighlighter :: Highlighter a b
orgHighlighter = error "orgHighlighter not implemented"

markdownHighlighter :: Highlighter a b
markdownHighlighter = error "markdownHighlighter not implemented"

data Mode = Mode
  { modeName :: String
  , modeKeymap :: KeymapSet
  , modeHL :: Highlighter () ()
  }

data AnyMode = AnyMode Mode