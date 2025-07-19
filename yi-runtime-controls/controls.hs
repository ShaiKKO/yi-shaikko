{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Yi.Syntax.Runtime.Controls
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Runtime controls for syntax highlighting with auto-detection and performance management.

module Yi.Syntax.Runtime.Controls
  ( -- * Types
    HighlightMode(..)
  , HighlightConfig(..)
  , AutoDetectConfig(..)
  , PerformanceMetrics(..)
  , RuntimeState(..)
    -- * Commands
  , highlightCommand
  , parseHighlightCommand
    -- * Auto-detection
  , shouldEnableHighlighting
  , detectOptimalMode
    -- * Runtime Control
  , toggleHighlighting
  , setHighlightMode
  , updateThresholds
    -- * Integration
  , registerHighlightCommands
  , initRuntimeControls
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.Default
import           Data.IORef
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           GHC.Generics
import           System.CPUTime
import           System.Directory
import           System.FilePath
import           System.Info (os)
import           System.IO
import           System.Mem
import           System.Process
import           Text.Printf
import           Text.Read (readMaybe)

-- Yi imports
import           Yi.Buffer
import           Yi.Config
import           Yi.Editor
import           Yi.Keymap
import           Yi.Types

-- | Highlighting modes
data HighlightMode
  = Full      -- ^ Full async highlighting with all features
  | Simple    -- ^ Basic lexer-only highlighting
  | Off       -- ^ No highlighting
  deriving (Show, Eq, Read, Generic)

instance Default HighlightMode where
  def = Full

-- | Runtime highlight configuration
data HighlightConfig = HighlightConfig
  { hlMode              :: HighlightMode
  , hlFileSizeThreshold :: Int           -- ^ MB threshold for auto-disable
  , hlCpuLoadThreshold  :: Double        -- ^ CPU load threshold (0.0-1.0)
  , hlMemoryThreshold   :: Int           -- ^ Free memory MB threshold
  , hlBatteryThreshold  :: Double        -- ^ Battery level threshold (0.0-1.0)
  , hlAutoDetect        :: Bool          -- ^ Enable auto-detection
  , hlRespectUser       :: Bool          -- ^ User overrides auto-detect
  } deriving (Show, Eq, Generic)

instance Default HighlightConfig where
  def = HighlightConfig
    { hlMode = Full
    , hlFileSizeThreshold = 20      -- 20MB
    , hlCpuLoadThreshold = 0.8      -- 80% CPU
    , hlMemoryThreshold = 500       -- 500MB free
    , hlBatteryThreshold = 0.2      -- 20% battery
    , hlAutoDetect = True
    , hlRespectUser = True
    }

-- | Auto-detection configuration
data AutoDetectConfig = AutoDetectConfig
  { adCheckInterval     :: Int           -- ^ Seconds between checks
  , adSmoothingFactor   :: Double        -- ^ Smooth metrics over time
  , adHysteresis        :: Double        -- ^ Prevent mode flapping
  } deriving (Show, Eq, Generic)

instance Default AutoDetectConfig where
  def = AutoDetectConfig
    { adCheckInterval = 5          -- Check every 5 seconds
    , adSmoothingFactor = 0.7      -- 70% weight to previous
    , adHysteresis = 0.1           -- 10% hysteresis band
    }

-- | Performance metrics
data PerformanceMetrics = PerformanceMetrics
  { pmCpuLoad       :: Double
  , pmMemoryFree    :: Int        -- ^ MB
  , pmBatteryLevel  :: Maybe Double
  , pmFileSize      :: Int        -- ^ MB
  , pmTimestamp     :: UTCTime
  } deriving (Show, Eq, Generic)

-- | Runtime state
data RuntimeState = RuntimeState
  { rsConfig        :: TVar HighlightConfig
  , rsAutoConfig    :: TVar AutoDetectConfig
  , rsMetrics       :: TVar PerformanceMetrics
  , rsMonitorThread :: TVar (Maybe ThreadId)
  , rsUserOverride  :: TVar Bool
  }

-- | Global runtime state
globalRuntimeState :: IORef (Maybe RuntimeState)
globalRuntimeState = unsafePerformIO $ newIORef Nothing
{-# NOINLINE globalRuntimeState #-}

-- | Initialize runtime controls
initRuntimeControls :: IO RuntimeState
initRuntimeControls = do
  config <- newTVarIO def
  autoConfig <- newTVarIO def
  metrics <- getCurrentMetrics >>= newTVarIO
  monitor <- newTVarIO Nothing
  userOverride <- newTVarIO False
  
  let state = RuntimeState config autoConfig metrics monitor userOverride
  
  -- Start monitoring thread
  tid <- forkIO $ monitoringLoop state
  atomically $ writeTVar monitor (Just tid)
  
  -- Store globally
  writeIORef globalRuntimeState (Just state)
  
  return state

-- | Get or create runtime state
getRuntimeState :: IO RuntimeState
getRuntimeState = do
  mstate <- readIORef globalRuntimeState
  case mstate of
    Just state -> return state
    Nothing -> initRuntimeControls

-- | Parse highlight command
parseHighlightCommand :: String -> Either String HighlightCommand
parseHighlightCommand cmd = case words cmd of
  ["highlight", "off"] -> Right HlOff
  ["highlight", "on"] -> Right HlOn
  ["highlight", "status"] -> Right HlStatus
  ["highlight", "mode", mode] -> case readMaybe mode of
    Just m -> Right (HlMode m)
    Nothing -> Left $ "Invalid mode: " ++ mode
  ["highlight", "threshold", "size", n] -> case readMaybe n of
    Just size -> Right (HlThreshold (ThresholdSize size))
    Nothing -> Left $ "Invalid size: " ++ n
  ["highlight", "threshold", "cpu", n] -> case readMaybe n of
    Just cpu -> Right (HlThreshold (ThresholdCpu cpu))
    Nothing -> Left $ "Invalid CPU threshold: " ++ n
  ["highlight", "auto"] -> Right HlAuto
  ["highlight", "manual"] -> Right HlManual
  _ -> Left "Unknown highlight command"

-- | Highlight commands
data HighlightCommand
  = HlOff
  | HlOn
  | HlStatus
  | HlMode HighlightMode
  | HlThreshold ThresholdType
  | HlAuto
  | HlManual
  deriving (Show, Eq)

data ThresholdType
  = ThresholdSize Int      -- ^ MB
  | ThresholdCpu Double    -- ^ 0.0-1.0
  | ThresholdMemory Int    -- ^ MB
  | ThresholdBattery Double -- ^ 0.0-1.0
  deriving (Show, Eq)

-- | Execute highlight command
highlightCommand :: HighlightCommand -> YiM ()
highlightCommand cmd = do
  state <- io getRuntimeState
  case cmd of
    HlOff -> do
      io $ setHighlightMode state Off
      withEditor $ printMsg "Syntax highlighting disabled"
    
    HlOn -> do
      io $ setHighlightMode state Full
      withEditor $ printMsg "Syntax highlighting enabled"
    
    HlStatus -> do
      status <- io $ getHighlightStatus state
      withEditor $ printMsg status
    
    HlMode mode -> do
      io $ setHighlightMode state mode
      withEditor $ printMsg $ "Highlight mode: " ++ show mode
    
    HlThreshold threshold -> do
      io $ updateThreshold state threshold
      withEditor $ printMsg "Threshold updated"
    
    HlAuto -> do
      io $ enableAutoDetect state True
      withEditor $ printMsg "Auto-detection enabled"
    
    HlManual -> do
      io $ enableAutoDetect state False
      withEditor $ printMsg "Manual mode enabled"

-- | Get highlight status
getHighlightStatus :: RuntimeState -> IO String
getHighlightStatus RuntimeState{..} = do
  config <- readTVarIO rsConfig
  metrics <- readTVarIO rsMetrics
  autoDetect <- readTVarIO rsAutoConfig
  userOverride <- readTVarIO rsUserOverride
  
  return $ unlines
    [ "Syntax Highlighting Status:"
    , "  Mode: " ++ show (hlMode config)
    , "  Auto-detect: " ++ if hlAutoDetect config then "enabled" else "disabled"
    , "  User override: " ++ if userOverride then "yes" else "no"
    , ""
    , "Thresholds:"
    , "  File size: " ++ show (hlFileSizeThreshold config) ++ " MB"
    , "  CPU load: " ++ printf "%.1f%%" (hlCpuLoadThreshold config * 100)
    , "  Free memory: " ++ show (hlMemoryThreshold config) ++ " MB"
    , "  Battery: " ++ printf "%.0f%%" (hlBatteryThreshold config * 100)
    , ""
    , "Current Metrics:"
    , "  CPU load: " ++ printf "%.1f%%" (pmCpuLoad metrics * 100)
    , "  Free memory: " ++ show (pmMemoryFree metrics) ++ " MB"
    , "  Battery: " ++ maybe "N/A" (printf "%.0f%%") (fmap (*100) (pmBatteryLevel metrics))
    , "  File size: " ++ show (pmFileSize metrics) ++ " MB"
    ]

-- | Set highlight mode
setHighlightMode :: RuntimeState -> HighlightMode -> IO ()
setHighlightMode RuntimeState{..} mode = do
  atomically $ do
    modifyTVar rsConfig $ \c -> c { hlMode = mode }
    writeTVar rsUserOverride True  -- User explicitly set mode

-- | Update threshold
updateThreshold :: RuntimeState -> ThresholdType -> IO ()
updateThreshold RuntimeState{..} threshold = atomically $ modifyTVar rsConfig $ \c ->
  case threshold of
    ThresholdSize mb -> c { hlFileSizeThreshold = mb }
    ThresholdCpu cpu -> c { hlCpuLoadThreshold = cpu }
    ThresholdMemory mb -> c { hlMemoryThreshold = mb }
    ThresholdBattery pct -> c { hlBatteryThreshold = pct }

-- | Enable/disable auto-detection
enableAutoDetect :: RuntimeState -> Bool -> IO ()
enableAutoDetect RuntimeState{..} enable = atomically $ do
  modifyTVar rsConfig $ \c -> c { hlAutoDetect = enable }
  when (not enable) $ writeTVar rsUserOverride False

-- | Toggle highlighting on/off
toggleHighlighting :: RuntimeState -> IO ()
toggleHighlighting state@RuntimeState{..} = do
  current <- hlMode <$> readTVarIO rsConfig
  let newMode = if current == Off then Full else Off
  setHighlightMode state newMode

-- | Should enable highlighting based on current conditions
shouldEnableHighlighting :: RuntimeState -> FilePath -> IO Bool
shouldEnableHighlighting RuntimeState{..} filepath = do
  config <- readTVarIO rsConfig
  
  -- Check user override first
  userOverride <- readTVarIO rsUserOverride
  if userOverride && hlRespectUser config
    then return (hlMode config /= Off)
    else if not (hlAutoDetect config)
      then return (hlMode config /= Off)
      else do
        -- Auto-detect based on metrics
        mode <- detectOptimalMode RuntimeState{..} filepath
        return (mode /= Off)

-- | Detect optimal highlighting mode
detectOptimalMode :: RuntimeState -> FilePath -> IO HighlightMode
detectOptimalMode RuntimeState{..} filepath = do
  config <- readTVarIO rsConfig
  metrics <- readTVarIO rsMetrics
  
  -- Update file size in metrics
  fileSize <- getFileSize filepath
  now <- getCurrentTime
  let updatedMetrics = metrics { pmFileSize = fileSize `div` (1024 * 1024), pmTimestamp = now }
  atomically $ writeTVar rsMetrics updatedMetrics
  
  -- Check thresholds
  let fileSizeOk = pmFileSize updatedMetrics < hlFileSizeThreshold config
      cpuOk = pmCpuLoad updatedMetrics < hlCpuLoadThreshold config
      memoryOk = pmMemoryFree updatedMetrics > hlMemoryThreshold config
      batteryOk = case pmBatteryLevel updatedMetrics of
        Nothing -> True
        Just level -> level > hlBatteryThreshold config
  
  -- Decide mode based on conditions
  return $ case (fileSizeOk, cpuOk && memoryOk && batteryOk) of
    (False, _) -> Off      -- File too large
    (_, False) -> Simple   -- System under load
    (True, True) -> Full   -- All good

-- | Get current performance metrics
getCurrentMetrics :: IO PerformanceMetrics
getCurrentMetrics = do
  cpu <- getCPULoad
  mem <- getFreeMemory
  battery <- getBatteryLevel
  now <- getCurrentTime
  
  return PerformanceMetrics
    { pmCpuLoad = cpu
    , pmMemoryFree = mem
    , pmBatteryLevel = battery
    , pmFileSize = 0  -- Will be updated per file
    , pmTimestamp = now
    }

-- | Monitoring loop
monitoringLoop :: RuntimeState -> IO ()
monitoringLoop state@RuntimeState{..} = forever $ do
  config <- readTVarIO rsAutoConfig
  
  -- Get current metrics
  newMetrics <- getCurrentMetrics
  
  -- Smooth with previous metrics
  oldMetrics <- readTVarIO rsMetrics
  let smoothed = smoothMetrics (adSmoothingFactor config) oldMetrics newMetrics
  
  atomically $ writeTVar rsMetrics smoothed
  
  -- Sleep until next check
  threadDelay (adCheckInterval config * 1000000)

-- | Smooth metrics over time
smoothMetrics :: Double -> PerformanceMetrics -> PerformanceMetrics -> PerformanceMetrics
smoothMetrics factor old new = new
  { pmCpuLoad = smooth (pmCpuLoad old) (pmCpuLoad new)
  , pmMemoryFree = round $ smooth (fromIntegral $ pmMemoryFree old) (fromIntegral $ pmMemoryFree new)
  }
  where
    smooth oldVal newVal = factor * oldVal + (1 - factor) * newVal

-- | Get CPU load (0.0-1.0)
getCPULoad :: IO Double
getCPULoad = do
  -- Platform-specific implementation
  case os of
    "linux" -> getCPULoadLinux
    "darwin" -> getCPULoadMacOS
    "mingw32" -> getCPULoadWindows
    _ -> return 0.5  -- Default

-- | Linux CPU load
getCPULoadLinux :: IO Double
getCPULoadLinux = do
  -- Read from /proc/loadavg
  handle (\(_ :: IOException) -> return 0.5) $ do
    contents <- readFile "/proc/loadavg"
    case words contents of
      (load1m:_) -> return $ min 1.0 (read load1m / 4.0)  -- Normalize by 4 cores
      _ -> return 0.5

-- | macOS CPU load
getCPULoadMacOS :: IO Double
getCPULoadMacOS = do
  -- Use sysctl or top command
  handle (\(_ :: IOException) -> return 0.5) $ do
    output <- readProcess "sysctl" ["-n", "vm.loadavg"] ""
    case words output of
      (_:load1m:_) -> return $ min 1.0 (read load1m / 4.0)
      _ -> return 0.5

-- | Windows CPU load
getCPULoadWindows :: IO Double
getCPULoadWindows = return 0.5  -- Simplified

-- | Get free memory in MB
getFreeMemory :: IO Int
getFreeMemory = do
  -- Platform-specific
  case os of
    "linux" -> getFreeMemoryLinux
    "darwin" -> getFreeMemoryMacOS
    _ -> return 1000  -- Default 1GB

-- | Linux free memory
getFreeMemoryLinux :: IO Int
getFreeMemoryLinux = do
  handle (\(_ :: IOException) -> return 1000) $ do
    contents <- readFile "/proc/meminfo"
    let memLines = lines contents
        findValue prefix = case filter (prefix `isPrefixOf`) memLines of
          (line:_) -> read $ takeWhile isDigit $ dropWhile (not . isDigit) line
          _ -> 0
        available = findValue "MemAvailable:"
    return (available `div` 1024)  -- Convert KB to MB
  where
    isPrefixOf = T.isPrefixOf . T.pack
    isDigit c = c >= '0' && c <= '9'

-- | macOS free memory  
getFreeMemoryMacOS :: IO Int
getFreeMemoryMacOS = do
  handle (\(_ :: IOException) -> return 1000) $ do
    output <- readProcess "vm_stat" [] ""
    -- Parse vm_stat output
    return 1000  -- Simplified

-- | Get battery level (0.0-1.0)
getBatteryLevel :: IO (Maybe Double)
getBatteryLevel = do
  -- Platform-specific
  case os of
    "linux" -> getBatteryLinux
    "darwin" -> getBatteryMacOS
    _ -> return Nothing

-- | Linux battery level
getBatteryLinux :: IO (Maybe Double)
getBatteryLinux = do
  handle (\(_ :: IOException) -> return Nothing) $ do
    let batteryPath = "/sys/class/power_supply/BAT0"
    exists <- doesDirectoryExist batteryPath
    if exists
      then do
        capacity <- readFile (batteryPath </> "capacity")
        return $ Just (read (trim capacity) / 100.0)
      else return Nothing
  where
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

-- | macOS battery level
getBatteryMacOS :: IO (Maybe Double)
getBatteryMacOS = do
  handle (\(_ :: IOException) -> return Nothing) $ do
    output <- readProcess "pmset" ["-g", "batt"] ""
    -- Parse pmset output for percentage
    return Nothing  -- Simplified

-- | Get file size
getFileSize :: FilePath -> IO Integer
getFileSize path = do
  handle (\(_ :: IOException) -> return 0) $ do
    h <- openFile path ReadMode
    size <- hFileSize h
    hClose h
    return size

-- | Register highlight commands with Yi
registerHighlightCommands :: YiM ()
registerHighlightCommands = do
  -- Ex commands for Vim mode
  registerExCommand "highlight" $ \args -> case parseHighlightCommand ("highlight " ++ args) of
    Right cmd -> highlightCommand cmd
    Left err -> withEditor $ printMsg $ "Error: " ++ err
  
  -- M-x commands for Emacs mode
  registerExtendedCommand "toggle-highlighting" toggleHighlightingCmd
  registerExtendedCommand "set-highlight-mode" setHighlightModeCmd
  registerExtendedCommand "highlight-status" highlightStatusCmd

-- | Command implementations
toggleHighlightingCmd :: YiM ()
toggleHighlightingCmd = do
  state <- io getRuntimeState
  io $ toggleHighlighting state
  mode <- io $ hlMode <$> readTVarIO (rsConfig state)
  withEditor $ printMsg $ "Highlighting: " ++ show mode

setHighlightModeCmd :: YiM ()
setHighlightModeCmd = do
  mode <- promptRead "Highlight mode (Full/Simple/Off): "
  case readMaybe mode of
    Just m -> highlightCommand (HlMode m)
    Nothing -> withEditor $ printMsg "Invalid mode"

highlightStatusCmd :: YiM ()
highlightStatusCmd = highlightCommand HlStatus

-- Helper functions
registerExCommand :: String -> (String -> YiM ()) -> YiM ()
registerExCommand = error "registerExCommand not implemented"

registerExtendedCommand :: String -> YiM () -> YiM ()
registerExtendedCommand = error "registerExtendedCommand not implemented"

promptRead :: String -> YiM String
promptRead = error "promptRead not implemented"

unsafePerformIO :: IO a -> a
unsafePerformIO = error "unsafePerformIO not implemented"