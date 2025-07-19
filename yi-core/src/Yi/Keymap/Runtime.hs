{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      :  Yi.Keymap.Runtime
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  stable
-- Portability :  portable
--
-- Runtime keymap switching and management.
-- Provides Ex commands for Vim and M-x commands for Emacs-style interfaces.

module Yi.Keymap.Runtime
  ( -- * Runtime commands
    switchKeymapCmd
  , listKeymapsCmd
  , currentKeymapCmd
  , describeKeymapCmd
  , reloadKeymapCmd
    -- * Ex commands (Vim-style)
  , exSwitchKeymap
  , exListKeymaps
  , exKeymapStatus
    -- * Extended commands (Emacs-style)
  , extendedSwitchKeymap
  , extendedDescribeKeymap
  , extendedCustomizeKeymap
    -- * Integration
  , registerRuntimeCommands
  , keymapModeline
  ) where

import           Control.Monad
import           Control.Concurrent.STM
import           Data.List (intercalate)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Lens.Micro.Platform
import           Yi.Buffer
import           Yi.Command
import           Yi.Editor
import           Yi.Event
import           Yi.Keymap
import           Yi.Keymap.Keys
import           Yi.Keymap.Registry
import           Yi.MiniBuffer
import           Yi.Types

-- | Global keymap registry (should be in editor state)
globalRegistry :: IORef (Maybe KeymapRegistry)
globalRegistry = unsafePerformIO $ newIORef Nothing

-- | Get or create the global registry
getRegistry :: YiM KeymapRegistry
getRegistry = io $ do
  mreg <- readIORef globalRegistry
  case mreg of
    Just reg -> return reg
    Nothing -> do
      reg <- mkKeymapRegistry
      -- Register default keymaps
      registerDefaultKeymaps reg
      writeIORef globalRegistry (Just reg)
      return reg

-- | Register all default keymaps
registerDefaultKeymaps :: KeymapRegistry -> IO ()
registerDefaultKeymaps reg = do
  -- Register built-in keymaps
  registerKeymap reg vimSpec
  registerKeymap reg emacsSpec
  registerKeymap reg cuaSpec
  registerKeymap reg acmeSpec
  registerKeymap reg joeSpec
  registerKeymap reg mgSpec
  registerKeymap reg eeSpec
  where
    vimSpec = KeymapSpec
      { keymapName = "vim"
      , keymapDescription = "Vi/Vim emulation (modal editing)"
      , keymapSet = error "vim keymap"  -- Would import actual keymap
      , keymapHelp = vimHelp
      , keymapEditable = True
      }
    
    emacsSpec = KeymapSpec
      { keymapName = "emacs"
      , keymapDescription = "GNU Emacs emulation"
      , keymapSet = error "emacs keymap"
      , keymapHelp = emacsHelp
      , keymapEditable = True
      }
    
    cuaSpec = KeymapSpec
      { keymapName = "cua"
      , keymapDescription = "Common User Access (Windows/Linux style)"
      , keymapSet = error "cua keymap"
      , keymapHelp = cuaHelp
      , keymapEditable = True
      }
    
    acmeSpec = KeymapSpec
      { keymapName = "acme"
      , keymapDescription = "Plan 9 Acme (mouse-centric)"
      , keymapSet = error "acme keymap"
      , keymapHelp = acmeHelp
      , keymapEditable = True
      }
    
    joeSpec = KeymapSpec
      { keymapName = "joe"
      , keymapDescription = "Joe's Own Editor (WordStar-compatible)"
      , keymapSet = error "joe keymap"
      , keymapHelp = joeHelp
      , keymapEditable = True
      }
    
    mgSpec = KeymapSpec
      { keymapName = "mg"
      , keymapDescription = "Micro GNU Emacs (minimal Emacs)"
      , keymapSet = error "mg keymap"
      , keymapHelp = mgHelp
      , keymapEditable = True
      }
    
    eeSpec = KeymapSpec
      { keymapName = "ee"
      , keymapDescription = "Easy Editor (beginner-friendly)"
      , keymapSet = error "ee keymap"
      , keymapHelp = eeHelp
      , keymapEditable = True
      }

-- | Switch to a different keymap
switchKeymapCmd :: Text -> YiM ()
switchKeymapCmd name = do
  reg <- getRegistry
  success <- switchKeymap reg name
  if success
    then do
      -- Update modeline
      withEditor $ modifyModeLineA $ \ml -> ml ++ " [" ++ T.unpack name ++ "]"
      withEditor $ printMsg $ "Switched to " ++ T.unpack name ++ " keymap"
    else
      withEditor $ printMsg $ "Unknown keymap: " ++ T.unpack name

-- | List available keymaps
listKeymapsCmd :: YiM ()
listKeymapsCmd = do
  reg <- getRegistry
  keymaps <- io $ listKeymaps reg
  current <- io $ getCurrentKeymap reg
  
  let formatKeymap (name, desc) = 
        let marker = if name == current then " *" else "  "
        in marker ++ T.unpack name ++ " - " ++ T.unpack desc
      
      output = unlines $ 
        ["Available keymaps:"] ++
        map formatKeymap keymaps ++
        ["", "Current: " ++ T.unpack current]
  
  -- Show in new buffer
  newBufferE (MemBuffer "*keymaps*") output

-- | Show current keymap
currentKeymapCmd :: YiM ()
currentKeymapCmd = do
  reg <- getRegistry
  current <- io $ getCurrentKeymap reg
  keymaps <- io $ listKeymaps reg
  
  case lookup current keymaps of
    Just desc -> withEditor $ printMsg $ 
      "Current keymap: " ++ T.unpack current ++ " - " ++ T.unpack desc
    Nothing -> withEditor $ printMsg $ "Current keymap: " ++ T.unpack current

-- | Describe a keymap
describeKeymapCmd :: Text -> YiM ()
describeKeymapCmd name = do
  reg <- getRegistry
  keymaps <- io $ atomically $ do
    km <- readTVar (registryKeymaps reg)
    return $ M.lookup name km
  
  case keymaps of
    Just spec -> do
      let output = unlines
            [ "Keymap: " ++ T.unpack (keymapName spec)
            , "Description: " ++ T.unpack (keymapDescription spec)
            , "Editable: " ++ show (keymapEditable spec)
            , ""
            , "Help:"
            , T.unpack (keymapHelp spec)
            ]
      newBufferE (MemBuffer $ "*help:" ++ T.unpack name ++ "*") output
    Nothing ->
      withEditor $ printMsg $ "Unknown keymap: " ++ T.unpack name

-- | Reload keymap configuration
reloadKeymapCmd :: YiM ()
reloadKeymapCmd = do
  reg <- getRegistry
  current <- io $ getCurrentKeymap reg
  switchKeymapCmd current
  withEditor $ printMsg "Keymap reloaded"

-- | Ex commands for Vim mode
exSwitchKeymap :: String -> YiM ()
exSwitchKeymap args = case words args of
  ["set", "keymap", name] -> switchKeymapCmd (T.pack name)
  ["setlocal", "keymap", name] -> do
    -- Buffer-local keymap (advanced feature)
    switchKeymapCmd (T.pack name)
  _ -> withEditor $ printMsg "Usage: :set keymap <name>"

exListKeymaps :: YiM ()
exListKeymaps = listKeymapsCmd

exKeymapStatus :: YiM ()
exKeymapStatus = currentKeymapCmd

-- | Extended commands for Emacs mode
extendedSwitchKeymap :: YiM ()
extendedSwitchKeymap = do
  reg <- getRegistry
  keymaps <- io $ listKeymaps reg
  let names = map (T.unpack . fst) keymaps
  
  name <- withMinibuffer "Switch to keymap: " names $ \input -> do
    if input `elem` names
      then return input
      else do
        printMsg $ "Unknown keymap: " ++ input
        return ""
  
  unless (null name) $ switchKeymapCmd (T.pack name)

extendedDescribeKeymap :: YiM ()
extendedDescribeKeymap = do
  reg <- getRegistry
  keymaps <- io $ listKeymaps reg
  let names = map (T.unpack . fst) keymaps
  
  name <- withMinibuffer "Describe keymap: " names return
  unless (null name) $ describeKeymapCmd (T.pack name)

extendedCustomizeKeymap :: YiM ()
extendedCustomizeKeymap = do
  reg <- getRegistry
  current <- io $ getCurrentKeymap reg
  withEditor $ printMsg $ "Customizing " ++ T.unpack current ++ " keymap..."
  -- Would open customization interface

-- | Register runtime commands with Yi
registerRuntimeCommands :: YiM ()
registerRuntimeCommands = do
  -- Register Ex commands
  registerExCommand "set" exSwitchKeymap
  registerExCommand "keymaps" (const exListKeymaps)
  registerExCommand "keymap" (const exKeymapStatus)
  
  -- Register extended commands
  registerExtendedCommand "switch-keymap" extendedSwitchKeymap
  registerExtendedCommand "describe-keymap" extendedDescribeKeymap
  registerExtendedCommand "customize-keymap" extendedCustomizeKeymap
  registerExtendedCommand "list-keymaps" listKeymapsCmd
  registerExtendedCommand "reload-keymap" reloadKeymapCmd

-- | Format keymap info for modeline
keymapModeline :: YiM String
keymapModeline = do
  reg <- getRegistry
  current <- io $ getCurrentKeymap reg
  return $ "[" ++ T.unpack current ++ "]"

-- | Help text for each keymap
vimHelp :: Text
vimHelp = T.unlines
  [ "Vim keymap - Modal editing"
  , ""
  , "Modes:"
  , "  Normal  - Navigation and commands"
  , "  Insert  - Text insertion (i, a, o)"
  , "  Visual  - Selection (v, V, Ctrl-V)"
  , "  Command - Ex commands (:)"
  , ""
  , "Basic commands:"
  , "  h,j,k,l - Move left/down/up/right"
  , "  i       - Insert mode"
  , "  :w      - Save"
  , "  :q      - Quit"
  , "  /       - Search"
  ]

emacsHelp :: Text
emacsHelp = T.unlines
  [ "Emacs keymap - Modifier-based"
  , ""
  , "Key conventions:"
  , "  C-x     - Ctrl+X"
  , "  M-x     - Alt+X (or ESC X)"
  , ""
  , "Basic commands:"
  , "  C-x C-s - Save"
  , "  C-x C-c - Quit"
  , "  C-x C-f - Find file"
  , "  C-s     - Search forward"
  , "  C-r     - Search backward"
  ]

cuaHelp :: Text
cuaHelp = T.unlines
  [ "CUA keymap - Standard shortcuts"
  , ""
  , "Common shortcuts:"
  , "  Ctrl+S  - Save"
  , "  Ctrl+O  - Open"
  , "  Ctrl+C  - Copy"
  , "  Ctrl+X  - Cut"
  , "  Ctrl+V  - Paste"
  , "  Ctrl+Z  - Undo"
  , "  Ctrl+F  - Find"
  ]

acmeHelp :: Text
acmeHelp = T.unlines
  [ "Acme keymap - Mouse-centric"
  , ""
  , "Mouse buttons:"
  , "  Button 1 - Select/position"
  , "  Button 2 - Execute/plumb"
  , "  Button 3 - Search/open"
  , ""
  , "Chords:"
  , "  1+2     - Cut"
  , "  1+3     - Paste"
  , "  2+3     - Copy (snarf)"
  ]

joeHelp :: Text
joeHelp = T.unlines
  [ "Joe keymap - WordStar-compatible"
  , ""
  , "Command prefix: Ctrl+K"
  , ""
  , "File commands:"
  , "  ^K D    - Save"
  , "  ^K X    - Save & exit"
  , "  ^K Q    - Quit"
  , ""
  , "Block commands:"
  , "  ^K B    - Begin block"
  , "  ^K K    - End block"
  , "  ^K C    - Copy block"
  ]

mgHelp :: Text
mgHelp = T.unlines
  [ "Mg keymap - Micro GNU Emacs"
  , ""
  , "Essential Emacs subset:"
  , "  C-x C-s - Save"
  , "  C-x C-c - Quit"
  , "  C-x C-f - Find file"
  , "  C-a     - Beginning of line"
  , "  C-e     - End of line"
  , "  C-k     - Kill line"
  ]

eeHelp :: Text
eeHelp = T.unlines
  [ "Easy Editor keymap - Beginner-friendly"
  , ""
  , "Function keys:"
  , "  F1      - Help"
  , "  F2      - Save"
  , "  F3      - Open"
  , "  ESC     - Menu"
  , ""
  , "Simple shortcuts:"
  , "  Arrows  - Move"
  , "  Ctrl+S  - Save"
  , "  Ctrl+Q  - Quit"
  ]

-- Helper functions
registerExCommand :: String -> (String -> YiM ()) -> YiM ()
registerExCommand = error "registerExCommand not implemented"

registerExtendedCommand :: String -> YiM () -> YiM ()
registerExtendedCommand = error "registerExtendedCommand not implemented"

modifyModeLineA :: (String -> String) -> EditorM ()
modifyModeLineA = error "modifyModeLineA not implemented"

newBufferE :: BufferRef -> String -> YiM ()
newBufferE = error "newBufferE not implemented"

data BufferRef = MemBuffer String

unsafePerformIO :: IO a -> a
unsafePerformIO = error "unsafePerformIO not implemented"