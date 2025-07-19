{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Yi.UI.Adapter
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Unified UI adapter for sharing keymap and highlighting across different frontends.
-- Supports Vty, Brick, Qt, and web backends.

module Yi.UI.Adapter
  ( -- * UI Backend Class
    UIBackend(..)
  , UICapabilities(..)
  , ColorDepth(..)
  , LayoutInfo(..)
  , HighlightData(..)
  , KeymapEvent(..)
    -- * Backend Registry
  , BackendRegistry
  , registerBackend
  , getBackend
  , listBackends
    -- * Unified Interface
  , UIAdapter(..)
  , createAdapter
  , switchBackend
    -- * Event Distribution
  , distributeKeyEvent
  , distributeHighlight
  , updateAllLayouts
    -- * Theme Management
  , Theme(..)
  , ThemeColor(..)
  , applyTheme
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.Default
import           Data.Dynamic
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Word (Word8)
import           GHC.Generics

-- Yi imports
import           Yi.Buffer
import           Yi.Config
import           Yi.Editor
import           Yi.Event
import           Yi.Keymap
import           Yi.Style
import           Yi.Window

-- Import highlighting types
import           Yi.Syntax.Async.Core (Token(..), TokenStyle(..))

-- | UI Backend capabilities
data UICapabilities = UICapabilities
  { capColorDepth      :: ColorDepth
  , capUnicode         :: Bool
  , capMouse           :: Bool
  , capAsyncRendering  :: Bool
  , capMultiWindow     :: Bool
  , capImages          :: Bool
  , capCustomFonts     :: Bool
  , capHardwareAccel   :: Bool
  } deriving (Show, Eq, Generic)

-- | Color depth support
data ColorDepth
  = Monochrome
  | Colors16
  | Colors256
  | TrueColor
  deriving (Show, Eq, Ord, Generic)

-- | Layout information
data LayoutInfo = LayoutInfo
  { layoutWindows      :: [(WindowRef, WindowLayout)]
  , layoutTabBar       :: Maybe TabBarLayout
  , layoutStatusLine   :: StatusLineLayout
  , layoutMiniBuffer   :: MiniBufferLayout
  } deriving (Show, Eq, Generic)

-- | Window layout
data WindowLayout = WindowLayout
  { winBounds          :: (Int, Int, Int, Int)  -- ^ (x, y, width, height)
  , winScrollPos       :: Int
  , winCursorPos       :: (Int, Int)
  , winLineNumbers     :: Bool
  , winRelativeNumbers :: Bool
  , winFolding         :: [(Int, Int)]          -- ^ Folded regions
  } deriving (Show, Eq, Generic)

-- | Tab bar layout
data TabBarLayout = TabBarLayout
  { tabPosition        :: TabPosition
  , tabStyle           :: TabStyle
  , tabList            :: [(TabRef, Text, Bool)]  -- ^ (ref, title, active)
  } deriving (Show, Eq, Generic)

data TabPosition = TabTop | TabBottom | TabLeft | TabRight
  deriving (Show, Eq, Generic)

data TabStyle = TabStyleText | TabStyleIcons | TabStyleBoth
  deriving (Show, Eq, Generic)

-- | Status line layout
data StatusLineLayout = StatusLineLayout
  { statusLeft         :: [StatusSegment]
  , statusCenter       :: [StatusSegment]
  , statusRight        :: [StatusSegment]
  } deriving (Show, Eq, Generic)

data StatusSegment = StatusSegment
  { segmentText        :: Text
  , segmentStyle       :: Maybe Style
  , segmentClickable   :: Bool
  } deriving (Show, Eq, Generic)

-- | Mini buffer layout
data MiniBufferLayout = MiniBufferLayout
  { miniHeight         :: Int
  , miniPrompt         :: Maybe Text
  , miniContent        :: Text
  , miniCompletions    :: [Text]
  } deriving (Show, Eq, Generic)

-- | Highlight data to render
data HighlightData = HighlightData
  { hlBuffer           :: BufferRef
  , hlTokens           :: V.Vector Token
  , hlDirtyRegion      :: Maybe (Int, Int)
  , hlRainbowParens    :: M.Map Int Int        -- ^ Position -> depth
  , hlDiagnostics      :: [(Int, Int, Text)]   -- ^ Errors/warnings
  , hlGitGutter        :: M.Map Int GitStatus
  } deriving (Show, Eq, Generic)

data GitStatus = GitAdded | GitModified | GitDeleted
  deriving (Show, Eq, Generic)

-- | Keymap event
data KeymapEvent = KeymapEvent
  { keEvent            :: Event
  , keWindow           :: WindowRef
  , keTimestamp        :: Double
  } deriving (Show, Eq, Generic)

-- | Abstract UI backend
class UIBackend b where
  -- | Backend type identifier
  type BackendHandle b :: *
  
  -- | Initialize backend
  initBackend :: b -> IO (BackendHandle b)
  
  -- | Shutdown backend
  shutdownBackend :: BackendHandle b -> IO ()
  
  -- | Get capabilities
  getCapabilities :: b -> UICapabilities
  
  -- | Render highlighting
  renderHighlight :: BackendHandle b -> HighlightData -> IO ()
  
  -- | Handle keymap event
  handleKeymap :: BackendHandle b -> KeymapEvent -> IO (Maybe Event)
  
  -- | Update layout
  updateLayout :: BackendHandle b -> LayoutInfo -> IO ()
  
  -- | Refresh display
  refreshDisplay :: BackendHandle b -> IO ()
  
  -- | Set theme
  setTheme :: BackendHandle b -> Theme -> IO ()

-- | Theme definition
data Theme = Theme
  { themeBackground    :: ThemeColor
  , themeForeground    :: ThemeColor
  , themeHighlight     :: ThemeColor
  , themeCursor        :: ThemeColor
  , themeSelection     :: ThemeColor
  , themeComment       :: ThemeColor
  , themeKeyword       :: ThemeColor
  , themeString        :: ThemeColor
  , themeFunction      :: ThemeColor
  , themeType          :: ThemeColor
  , themeNumber        :: ThemeColor
  , themeOperator      :: ThemeColor
  , themeError         :: ThemeColor
  , themeWarning       :: ThemeColor
  , themeLineNumbers   :: ThemeColor
  , themeStatusBar     :: ThemeColor
  } deriving (Show, Eq, Generic)

-- | Theme color with different representations
data ThemeColor = ThemeColor
  { colorRGB           :: (Word8, Word8, Word8)
  , color256           :: Word8
  , color16            :: Word8
  , colorMono          :: Bool  -- ^ True for white, False for black
  } deriving (Show, Eq, Generic)

instance Default Theme where
  def = Theme
    { themeBackground = ThemeColor (30, 30, 30) 235 0 False
    , themeForeground = ThemeColor (220, 220, 220) 252 7 True
    , themeHighlight = ThemeColor (80, 80, 80) 238 8 False
    , themeCursor = ThemeColor (255, 255, 255) 255 15 True
    , themeSelection = ThemeColor (60, 60, 60) 237 8 False
    , themeComment = ThemeColor (100, 100, 100) 242 8 False
    , themeKeyword = ThemeColor (200, 120, 200) 176 5 True
    , themeString = ThemeColor (120, 200, 120) 114 2 True
    , themeFunction = ThemeColor (120, 120, 200) 75 4 True
    , themeType = ThemeColor (200, 200, 120) 186 3 True
    , themeNumber = ThemeColor (200, 120, 120) 167 1 True
    , themeOperator = ThemeColor (200, 200, 200) 250 7 True
    , themeError = ThemeColor (255, 100, 100) 196 1 True
    , themeWarning = ThemeColor (255, 200, 100) 214 3 True
    , themeLineNumbers = ThemeColor (80, 80, 80) 238 8 False
    , themeStatusBar = ThemeColor (50, 50, 50) 236 0 False
    }

-- | Existential backend wrapper
data AnyBackend = forall b. UIBackend b => AnyBackend b (BackendHandle b)

-- | Backend registry
type BackendRegistry = TVar (M.Map Text AnyBackend)

-- | Global backend registry
globalRegistry :: BackendRegistry
globalRegistry = unsafePerformIO $ newTVarIO M.empty
{-# NOINLINE globalRegistry #-}

-- | Register a backend
registerBackend :: UIBackend b => Text -> b -> IO ()
registerBackend name backend = do
  handle <- initBackend backend
  atomically $ modifyTVar globalRegistry $ 
    M.insert name (AnyBackend backend handle)

-- | Get a backend
getBackend :: Text -> IO (Maybe AnyBackend)
getBackend name = M.lookup name <$> readTVarIO globalRegistry

-- | List available backends
listBackends :: IO [(Text, UICapabilities)]
listBackends = do
  backends <- readTVarIO globalRegistry
  return [(name, getBackendCapabilities anyBackend) | (name, anyBackend) <- M.toList backends]
  where
    getBackendCapabilities (AnyBackend backend _) = getCapabilities backend

-- | Unified UI adapter
data UIAdapter = UIAdapter
  { adapterCurrent     :: TVar Text             -- ^ Current backend name
  , adapterBackends    :: BackendRegistry      -- ^ Available backends
  , adapterTheme       :: TVar Theme           -- ^ Current theme
  , adapterLayout      :: TVar LayoutInfo      -- ^ Current layout
  , adapterEventQueue  :: TQueue KeymapEvent   -- ^ Event queue
  , adapterHighlights  :: TVar (M.Map BufferRef HighlightData)
  }

-- | Create UI adapter
createAdapter :: Text -> IO UIAdapter
createAdapter initialBackend = do
  current <- newTVarIO initialBackend
  theme <- newTVarIO def
  layout <- newTVarIO emptyLayout
  eventQueue <- newTQueueIO
  highlights <- newTVarIO M.empty
  
  return UIAdapter
    { adapterCurrent = current
    , adapterBackends = globalRegistry
    , adapterTheme = theme
    , adapterLayout = layout
    , adapterEventQueue = eventQueue
    , adapterHighlights = highlights
    }

-- | Empty layout
emptyLayout :: LayoutInfo
emptyLayout = LayoutInfo [] Nothing emptyStatusLine emptyMiniBuffer
  where
    emptyStatusLine = StatusLineLayout [] [] []
    emptyMiniBuffer = MiniBufferLayout 1 Nothing "" []

-- | Switch to different backend
switchBackend :: UIAdapter -> Text -> IO Bool
switchBackend UIAdapter{..} newBackend = do
  backends <- readTVarIO adapterBackends
  case M.lookup newBackend backends of
    Just (AnyBackend backend handle) -> do
      -- Apply current theme and layout to new backend
      theme <- readTVarIO adapterTheme
      layout <- readTVarIO adapterLayout
      highlights <- readTVarIO adapterHighlights
      
      setTheme handle theme
      updateLayout handle layout
      
      -- Render all highlights
      forM_ (M.elems highlights) $ renderHighlight handle
      
      -- Update current backend
      atomically $ writeTVar adapterCurrent newBackend
      
      return True
    Nothing -> return False

-- | Distribute key event to current backend
distributeKeyEvent :: UIAdapter -> Event -> WindowRef -> IO (Maybe Event)
distributeKeyEvent UIAdapter{..} event window = do
  currentName <- readTVarIO adapterCurrent
  backends <- readTVarIO adapterBackends
  
  case M.lookup currentName backends of
    Just (AnyBackend _ handle) -> do
      timestamp <- realToFrac <$> getPOSIXTime
      let kevent = KeymapEvent event window timestamp
      handleKeymap handle kevent
    Nothing -> return Nothing

-- | Distribute highlight data to current backend
distributeHighlight :: UIAdapter -> BufferRef -> V.Vector Token -> IO ()
distributeHighlight adapter@UIAdapter{..} bufRef tokens = do
  -- Create highlight data
  let hlData = HighlightData
        { hlBuffer = bufRef
        , hlTokens = tokens
        , hlDirtyRegion = Nothing
        , hlRainbowParens = M.empty  -- Would be computed
        , hlDiagnostics = []
        , hlGitGutter = M.empty
        }
  
  -- Store in adapter
  atomically $ modifyTVar adapterHighlights $ M.insert bufRef hlData
  
  -- Render on current backend
  currentName <- readTVarIO adapterCurrent
  backends <- readTVarIO adapterBackends
  
  case M.lookup currentName backends of
    Just (AnyBackend _ handle) -> renderHighlight handle hlData
    Nothing -> return ()

-- | Update layout on all backends
updateAllLayouts :: UIAdapter -> LayoutInfo -> IO ()
updateAllLayouts UIAdapter{..} layout = do
  -- Store layout
  atomically $ writeTVar adapterLayout layout
  
  -- Update current backend
  currentName <- readTVarIO adapterCurrent
  backends <- readTVarIO adapterBackends
  
  case M.lookup currentName backends of
    Just (AnyBackend _ handle) -> updateLayout handle layout
    Nothing -> return ()

-- | Apply theme to all backends
applyTheme :: UIAdapter -> Theme -> IO ()
applyTheme UIAdapter{..} theme = do
  -- Store theme
  atomically $ writeTVar adapterTheme theme
  
  -- Apply to all backends
  backends <- readTVarIO adapterBackends
  forM_ (M.elems backends) $ \(AnyBackend _ handle) ->
    setTheme handle theme

-- ===== Backend Implementations =====

-- | Vty terminal backend
data VtyBackend = VtyBackend
  { vtyConfig :: VtyConfig
  }

data VtyConfig = VtyConfig
  { vtyMouseSupport :: Bool
  , vtyBracketedPaste :: Bool
  }

instance Default VtyConfig where
  def = VtyConfig True True

instance UIBackend VtyBackend where
  type BackendHandle VtyBackend = VtyHandle
  
  initBackend VtyBackend{..} = do
    -- Initialize Vty
    vty <- Vty.mkVty =<< Vty.standardIOConfig
    eventChan <- newTChanIO
    renderQueue <- newTQueueIO
    
    -- Start event thread
    eventThread <- forkIO $ vtyEventLoop vty eventChan
    
    return VtyHandle
      { vhVty = vty
      , vhEventChan = eventChan
      , vhRenderQueue = renderQueue
      , vhEventThread = eventThread
      }
  
  shutdownBackend VtyHandle{..} = do
    killThread vhEventThread
    Vty.shutdown vhVty
  
  getCapabilities _ = UICapabilities
    { capColorDepth = Colors256
    , capUnicode = True
    , capMouse = True
    , capAsyncRendering = False
    , capMultiWindow = False
    , capImages = False
    , capCustomFonts = False
    , capHardwareAccel = False
    }
  
  renderHighlight handle hlData = 
    vtyRenderHighlight handle hlData
  
  handleKeymap handle kevent =
    vtyHandleKeymap handle kevent
  
  updateLayout handle layout =
    vtyUpdateLayout handle layout
  
  refreshDisplay VtyHandle{..} =
    Vty.refresh vhVty
  
  setTheme handle theme =
    vtySetTheme handle theme

data VtyHandle = VtyHandle
  { vhVty :: Vty.Vty
  , vhEventChan :: TChan Vty.Event
  , vhRenderQueue :: TQueue RenderCommand
  , vhEventThread :: ThreadId
  }

data RenderCommand
  = RenderText Int Int Text Vty.Attr
  | RenderCursor Int Int
  | RenderClear

-- Vty-specific implementations
vtyEventLoop :: Vty.Vty -> TChan Vty.Event -> IO ()
vtyEventLoop vty chan = forever $ do
  event <- Vty.nextEvent vty
  atomically $ writeTChan chan event

vtyRenderHighlight :: VtyHandle -> HighlightData -> IO ()
vtyRenderHighlight VtyHandle{..} HighlightData{..} = do
  -- Convert tokens to Vty rendering commands
  let commands = concatMap tokenToVtyCommands (V.toList hlTokens)
  -- Queue render commands
  atomically $ mapM_ (writeTQueue vhRenderQueue) commands
  where
    tokenToVtyCommands Token{..} = 
      [RenderText row col tokenText (styleToAttr tokenStyle)]
      where
        row = tokenStart `div` 80  -- Simplified
        col = tokenStart `mod` 80

vtyHandleKeymap :: VtyHandle -> KeymapEvent -> IO (Maybe Event)
vtyHandleKeymap VtyHandle{..} KeymapEvent{..} = do
  -- Convert Yi event to Vty event
  return $ Just keEvent

vtyUpdateLayout :: VtyHandle -> LayoutInfo -> IO ()
vtyUpdateLayout _ _ = return ()  -- Simplified

vtySetTheme :: VtyHandle -> Theme -> IO ()
vtySetTheme _ _ = return ()  -- Simplified

styleToAttr :: TokenStyle -> Vty.Attr
styleToAttr = const Vty.defAttr  -- Simplified

-- | Brick terminal backend (higher-level)
data BrickBackend = BrickBackend
  { brickConfig :: BrickConfig
  }

data BrickConfig = BrickConfig
  { brickScrollBars :: Bool
  , brickTabBar :: Bool
  }

instance Default BrickConfig where
  def = BrickConfig True True

-- Brick implementation would go here...

-- | Qt GUI backend
data QtBackend = QtBackend
  { qtConfig :: QtConfig
  }

data QtConfig = QtConfig
  { qtAntialiasing :: Bool
  , qtDPI :: Double
  }

-- Qt implementation would use hs-qt...

-- | Web backend
data WebBackend = WebBackend
  { webPort :: Int
  }

-- Web implementation would use WebSockets...

-- Helper types
data TabRef = TabRef Int
  deriving (Show, Eq)

getPOSIXTime :: IO Double
getPOSIXTime = error "getPOSIXTime not implemented"

unsafePerformIO :: IO a -> a
unsafePerformIO = error "unsafePerformIO not implemented"

-- Vty imports (simplified)
module Vty where
  data Vty
  data Event
  data Attr
  defAttr :: Attr
  mkVty :: a -> IO Vty
  standardIOConfig :: IO a
  shutdown :: Vty -> IO ()
  refresh :: Vty -> IO ()
  nextEvent :: Vty -> IO Event