{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      :  Yi.Keymap.Registry
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Dynamic keymap registry for runtime switching and editing of keymaps.
-- Supports hot-swapping between different keymap implementations.

module Yi.Keymap.Registry
  ( KeymapRegistry
  , KeymapSpec(..)
  , mkKeymapRegistry
  , registerKeymap
  , unregisterKeymap
  , listKeymaps
  , switchKeymap
  , getCurrentKeymap
  , editKeymapBindings
  , addGlobalBinding
  , removeGlobalBinding
  ) where

import           Control.Concurrent.STM
import           Control.Monad
import           Data.Default
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Lens.Micro.Platform
import           Yi.Buffer
import           Yi.Editor
import           Yi.Event
import           Yi.Keymap
import           Yi.Keymap.Keys
import           Yi.Types

-- | Specification for a registered keymap
data KeymapSpec = KeymapSpec
  { keymapName        :: Text
  , keymapDescription :: Text
  , keymapSet         :: KeymapSet
  , keymapHelp        :: Text
  , keymapEditable    :: Bool  -- ^ Whether this keymap can be edited at runtime
  }

-- | Registry holding all available keymaps
data KeymapRegistry = KeymapRegistry
  { registryKeymaps :: TVar (M.Map Text KeymapSpec)
  , registryCurrent :: TVar Text
  , registryOverlay :: TVar (Maybe Keymap)  -- ^ Global overlay bindings
  }

-- | Create a new keymap registry with default keymaps
mkKeymapRegistry :: IO KeymapRegistry
mkKeymapRegistry = do
  keymaps <- newTVarIO M.empty
  current <- newTVarIO "vim"  -- Default to vim
  overlay <- newTVarIO Nothing
  return KeymapRegistry
    { registryKeymaps = keymaps
    , registryCurrent = current
    , registryOverlay = overlay
    }

-- | Register a new keymap in the registry
registerKeymap :: KeymapRegistry -> KeymapSpec -> IO Bool
registerKeymap KeymapRegistry{..} spec = atomically $ do
  keymaps <- readTVar registryKeymaps
  if M.member (keymapName spec) keymaps
    then return False
    else do
      writeTVar registryKeymaps $ M.insert (keymapName spec) spec keymaps
      return True

-- | Unregister a keymap from the registry
unregisterKeymap :: KeymapRegistry -> Text -> IO Bool
unregisterKeymap KeymapRegistry{..} name = atomically $ do
  current <- readTVar registryCurrent
  if current == name
    then return False  -- Can't unregister active keymap
    else do
      keymaps <- readTVar registryKeymaps
      writeTVar registryKeymaps $ M.delete name keymaps
      return True

-- | List all registered keymaps
listKeymaps :: KeymapRegistry -> IO [(Text, Text)]
listKeymaps KeymapRegistry{..} = atomically $ do
  keymaps <- readTVar registryKeymaps
  return [(keymapName spec, keymapDescription spec) | spec <- M.elems keymaps]

-- | Switch to a different keymap
switchKeymap :: KeymapRegistry -> Text -> YiM Bool
switchKeymap KeymapRegistry{..} name = do
  keymaps <- io $ readTVarIO registryKeymaps
  case M.lookup name keymaps of
    Nothing -> return False
    Just spec -> do
      -- Update the current keymap
      io $ atomically $ writeTVar registryCurrent name
      
      -- Apply the keymap to the config
      withEditor $ do
        configA . defaultKmA .= keymapSet spec
      
      -- Notify user
      withEditor $ printMsg $ "Switched to " <> T.unpack name <> " keymap"
      return True

-- | Get the name of the current keymap
getCurrentKeymap :: KeymapRegistry -> IO Text
getCurrentKeymap KeymapRegistry{..} = readTVarIO registryCurrent

-- | Edit keymap bindings at runtime (for editable keymaps only)
editKeymapBindings :: KeymapRegistry -> Text -> (KeymapSet -> KeymapSet) -> IO Bool
editKeymapBindings KeymapRegistry{..} name modifier = atomically $ do
  keymaps <- readTVar registryKeymaps
  case M.lookup name keymaps of
    Just spec | keymapEditable spec -> do
      let newSpec = spec { keymapSet = modifier (keymapSet spec) }
      writeTVar registryKeymaps $ M.insert name newSpec keymaps
      
      -- If this is the current keymap, update it immediately
      current <- readTVar registryCurrent
      when (current == name) $ do
        -- Note: actual config update needs to happen in YiM monad
        return ()
      return True
    _ -> return False

-- | Add a global key binding that overlays all keymaps
addGlobalBinding :: KeymapRegistry -> Event -> YiM () -> YiM ()
addGlobalBinding KeymapRegistry{..} event action = do
  let binding = event ?>>! action
  io $ atomically $ do
    current <- readTVar registryOverlay
    let newOverlay = case current of
          Nothing -> binding
          Just km -> km >> binding
    writeTVar registryOverlay (Just newOverlay)
  
  -- Apply the overlay
  applyOverlay KeymapRegistry{..}

-- | Remove a global key binding
removeGlobalBinding :: KeymapRegistry -> Event -> YiM ()
removeGlobalBinding KeymapRegistry{..} _event = do
  -- This is simplified - in practice we'd need more sophisticated
  -- tracking of individual bindings to support removal
  io $ atomically $ writeTVar registryOverlay Nothing
  applyOverlay KeymapRegistry{..}

-- | Apply the global overlay to the current keymap
applyOverlay :: KeymapRegistry -> YiM ()
applyOverlay KeymapRegistry{..} = do
  maybeOverlay <- io $ readTVarIO registryOverlay
  current <- io $ readTVarIO registryCurrent
  keymaps <- io $ readTVarIO registryKeymaps
  
  case M.lookup current keymaps of
    Just spec -> do
      let baseKm = keymapSet spec
          finalKm = case maybeOverlay of
            Nothing -> baseKm
            Just overlay -> baseKm { topKeymap = overlay >> topKeymap baseKm }
      
      withEditor $ configA . defaultKmA .= finalKm
    Nothing -> return ()