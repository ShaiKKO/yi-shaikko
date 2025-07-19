{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Yi.Keymap.Acme
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Acme-style keymap for Yi, implementing Plan 9 Acme editor bindings.
-- Features mouse chords, plumbing, and distinctive window management.

module Yi.Keymap.Acme
  ( keymapSet
  , mkKeymapSet
  , acmeKeymap
  , plumb
  , chord
  , AcmeConfig(..)
  , defaultAcmeConfig
  ) where

import           Control.Monad
import           Control.Monad.State
import           Data.Char (isAlphaNum)
import           Data.Default
import           Data.List (isPrefixOf)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Lens.Micro.Platform
import           System.FilePath
import           System.Process
import           Yi.Buffer
import           Yi.Editor
import           Yi.Event
import           Yi.File
import           Yi.Keymap
import           Yi.Keymap.Keys
import           Yi.MiniBuffer
import           Yi.Misc
import           Yi.Rectangle
import           Yi.Search
import           Yi.String

-- | Acme configuration
data AcmeConfig = AcmeConfig
  { acmePlumbCommand :: String           -- ^ External plumber command
  , acmePlumbRules   :: [PlumbRule]     -- ^ Internal plumbing rules
  , acmeAutoIndent   :: Bool
  , acmeTabSize      :: Int
  }

data PlumbRule = PlumbRule
  { plumbPattern :: String -> Bool
  , plumbAction  :: String -> YiM ()
  }

instance Default AcmeConfig where
  def = defaultAcmeConfig

defaultAcmeConfig :: AcmeConfig
defaultAcmeConfig = AcmeConfig
  { acmePlumbCommand = "plumb"  -- Plan 9 plumber
  , acmePlumbRules = defaultPlumbRules
  , acmeAutoIndent = True
  , acmeTabSize = 4
  }

-- | Default plumbing rules
defaultPlumbRules :: [PlumbRule]
defaultPlumbRules =
  [ PlumbRule isFilePath openFile
  , PlumbRule isURL openURL
  , PlumbRule isEmail composeEmail
  ]
  where
    isFilePath s = '/' `elem` s || '.' `elem` s
    isURL s = any (`isPrefixOf` s) ["http://", "https://", "ftp://"]
    isEmail s = '@' `elem` s && '.' `elem` dropWhile (/= '@') s
    
    openFile path = fnewE path
    openURL url = void $ startSubprocess ("xdg-open " ++ url) (const $ return ())
    composeEmail email = void $ startSubprocess ("mailto:" ++ email) (const $ return ())

-- | Create Acme keymap set with configuration
mkKeymapSet :: AcmeConfig -> KeymapSet
mkKeymapSet config = modelessKeymapSet $ acmeKeymap config

-- | Default Acme keymap set
keymapSet :: KeymapSet
keymapSet = mkKeymapSet defaultAcmeConfig

-- | Main Acme keymap
acmeKeymap :: AcmeConfig -> Keymap
acmeKeymap config = choice
  [ -- Mouse operations (Acme's primary interface)
    mouseBindings config
    
    -- Basic text operations
  , spec KEnter      ?>>! newlineAndIndentB
  , spec KTab        ?>>! insertTab config
  , spec KBS         ?>>! deleteCharB
  , spec KDel        ?>>! deleteCharB
  , spec KHome       ?>>! moveToSol
  , spec KEnd        ?>>! moveToEol
  , spec KPageUp     ?>>! scrollPageUp
  , spec KPageDown   ?>>! scrollPageDown
  
    -- Arrows
  , spec KUp         ?>>! moveUp
  , spec KDown       ?>>! moveDown
  , spec KLeft       ?>>! moveLeft
  , spec KRight      ?>>! moveRight
  
    -- Control sequences (minimal in Acme style)
  , ctrl (char 'a')  ?>>! moveToSol
  , ctrl (char 'e')  ?>>! moveToEol
  , ctrl (char 'u')  ?>>! killLineB
  , ctrl (char 'k')  ?>>! killToEOL
  , ctrl (char 'w')  ?>>! killWordB
  , ctrl (char 'y')  ?>>! yankB
  , ctrl (char 'f')  ?>>! searchForward
  , ctrl (char 'r')  ?>>! searchBackward
  , ctrl (char 's')  ?>>! fwriteE
  , ctrl (char 'q')  ?>>! askQuitEditor
  
    -- Escape (command mode)
  , spec KEsc        ?>>  commandMode config
  
    -- Self-inserting characters
  , anyEvent         >>= insertSelfB
  ]

-- | Mouse bindings implementing Acme's mouse chords
mouseBindings :: AcmeConfig -> Keymap
mouseBindings config = choice
  [ -- Button 1: Select and move cursor
    spec (MouseEvent (Just LeftButton) False _) >>= handleButton1
    
    -- Button 2: Execute (plumb)
  , spec (MouseEvent (Just MiddleButton) False _) >>= handleButton2 config
    
    -- Button 3: Search or open
  , spec (MouseEvent (Just RightButton) False _) >>= handleButton3
    
    -- Chords (multiple buttons)
  , chord [LeftButton, MiddleButton] >>= handleCut
  , chord [LeftButton, RightButton] >>= handlePaste
  , chord [MiddleButton, RightButton] >>= handleSnarf
  ]

-- | Handle button 1 (select/move)
handleButton1 :: EventType -> YiM ()
handleButton1 (MouseEvent (Just LeftButton) False (x, y)) = do
  -- Move to clicked position
  withCurrentBuffer $ do
    moveToLineColB y x
    setVisibleSelection False

-- | Handle button 2 (execute/plumb)
handleButton2 :: AcmeConfig -> EventType -> YiM ()
handleButton2 config (MouseEvent (Just MiddleButton) False pos) = do
  -- Get word or selection at position
  text <- withCurrentBuffer $ do
    moveToLineColB (snd pos) (fst pos)
    wordUnderCursor <|> getSelectionText
  
  case text of
    Just t -> plumb config (T.unpack t)
    Nothing -> return ()

-- | Handle button 3 (search/open)
handleButton3 :: EventType -> YiM ()
handleButton3 (MouseEvent (Just RightButton) False pos) = do
  -- Get word at position and search for it
  text <- withCurrentBuffer $ do
    moveToLineColB (snd pos) (fst pos)
    wordUnderCursor
  
  case text of
    Just t -> searchForwardText t
    Nothing -> return ()

-- | Get word under cursor
wordUnderCursor :: BufferM (Maybe T.Text)
wordUnderCursor = do
  p1 <- pointB
  moveToStartOfWordB
  start <- pointB
  moveToEndOfWordB
  end <- pointB
  moveTo p1
  if start == end
    then return Nothing
    else Just <$> readRegionB (mkRegion start end)

-- | Get selected text
getSelectionText :: BufferM (Maybe T.Text)
getSelectionText = do
  region <- getSelectRegionB
  if regionStart region == regionEnd region
    then return Nothing
    else Just <$> readRegionB region

-- | Mouse chord detection
chord :: [MouseButton] -> Keymap Event
chord buttons = do
  events <- many anyEvent
  let mouseEvents = [e | e@(Event _ (MouseEvent (Just b) _ _)) <- events, b `elem` buttons]
  if length mouseEvents == length buttons
    then return (last events)
    else empty

-- | Handle cut (chord 1+2)
handleCut :: Event -> YiM ()
handleCut _ = withCurrentBuffer $ do
  getSelectRegionB >>= deleteRegionB
  
-- | Handle paste (chord 1+3)  
handlePaste :: Event -> YiM ()
handlePaste _ = withCurrentBuffer $ do
  pointB >>= setSelectionMarkPointB
  getRegisterE >>= \case
    Nothing -> return ()
    Just text -> insertN text

-- | Handle snarf/copy (chord 2+3)
handleSnarf :: Event -> YiM ()
handleSnarf _ = withCurrentBuffer $ do
  getSelectRegionB >>= readRegionB >>= setRegisterE

-- | Plumb text according to rules
plumb :: AcmeConfig -> String -> YiM ()
plumb AcmeConfig{..} text = do
  -- Try internal rules first
  let matchedRule = listToMaybe [rule | rule <- acmePlumbRules, plumbPattern rule text]
  
  case matchedRule of
    Just rule -> plumbAction rule text
    Nothing -> 
      -- Fall back to external plumber
      void $ startSubprocess (acmePlumbCommand ++ " " ++ text) (const $ return ())

-- | Command mode (Escape commands)
commandMode :: AcmeConfig -> Keymap
commandMode config = do
  Event _ (KASCII c) <- anyEvent
  case c of
    'w' -> fwriteE
    'q' -> closeWindow
    'x' -> fwriteE >> closeWindow
    'n' -> newWindowE
    '/' -> searchForward
    '?' -> searchBackward
    _ -> return ()

-- | Insert tab with proper indentation
insertTab :: AcmeConfig -> YiM ()
insertTab AcmeConfig{..} = withCurrentBuffer $ do
  if acmeAutoIndent
    then insertN (replicate acmeTabSize ' ')
    else insertB '\t'

-- | Search forward for text
searchForwardText :: T.Text -> YiM ()
searchForwardText text = do
  setRegexE (T.unpack text)
  searchForward

-- | Move to start of word
moveToStartOfWordB :: BufferM ()
moveToStartOfWordB = do
  c <- readB
  if isAlphaNum c
    then moveWhileB (isAlphaNum) Backward >> moveB Character Forward
    else moveB Character Backward

-- | Move to end of word  
moveToEndOfWordB :: BufferM ()
moveToEndOfWordB = moveWhileB (isAlphaNum) Forward

-- Helper functions that would need proper implementation
startSubprocess :: String -> (String -> IO ()) -> YiM ()
startSubprocess = error "startSubprocess not implemented"

setRegisterE :: T.Text -> BufferM ()
setRegisterE = error "setRegisterE not implemented"

getRegisterE :: BufferM (Maybe T.Text)
getRegisterE = error "getRegisterE not implemented"

searchForward :: YiM ()
searchForward = error "searchForward not implemented"

searchBackward :: YiM ()  
searchBackward = error "searchBackward not implemented"

MouseButton :: EventType
pattern MouseButton = error "MouseButton pattern not defined"

LeftButton, MiddleButton, RightButton :: MouseButton
LeftButton = error "LeftButton not defined"
MiddleButton = error "MiddleButton not defined"  
RightButton = error "RightButton not defined"

MouseEvent :: Maybe MouseButton -> Bool -> (Int, Int) -> EventType
MouseEvent = error "MouseEvent constructor not defined"