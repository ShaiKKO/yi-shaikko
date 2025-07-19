{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Yi.Syntax.Async.Core
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Core async syntax highlighting with thread management and priority queues.
-- Ensures zero UI blocking through careful concurrency design.

module Yi.Syntax.Async.Core
  ( -- * Types
    AsyncHighlighter(..)
  , ThreadPool(..)
  , ParseTask(..)
  , ParsePriority(..)
  , HighlightResult(..)
  , IncrementalState(..)
  , HighlightStats(..)
    -- * Thread Management
  , createThreadPool
  , shutdownThreadPool
  , submitTask
  , cancelTask
    -- * Highlighter Creation
  , createAsyncHighlighter
  , updateHighlighter
    -- * Incremental Updates
  , markDirty
  , processIncrementalUpdate
    -- * Benchmarking
  , measureHighlightPerformance
  ) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.Foldable (for_)
import           Data.IORef
import qualified Data.IntMap.Strict as IM
import qualified Data.IntervalMap.Generic.Strict as IvMap
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import qualified Data.Vector as V
import           GHC.Generics
import           System.CPUTime
import           System.Mem

-- | Parse priority levels
data ParsePriority
  = Immediate  -- ^ Currently visible region (highest priority)
  | Visible    -- ^ Near visible region
  | Background -- ^ Rest of file
  deriving (Eq, Ord, Show, Generic)

-- | A parsing task
data ParseTask = ParseTask
  { taskId       :: !Int
  , taskPriority :: !ParsePriority
  , taskRegion   :: !(Int, Int)  -- ^ (start, end) offsets
  , taskContent  :: !Text
  , taskVersion  :: !Int         -- ^ Document version for cancellation
  , taskCallback :: HighlightResult -> IO ()
  }

-- | Result of highlighting
data HighlightResult = HighlightResult
  { resultId      :: !Int
  , resultTokens  :: !(V.Vector Token)
  , resultRegion  :: !(Int, Int)
  , resultVersion :: !Int
  , resultTime    :: !NominalDiffTime  -- ^ Parse time
  } deriving (Show, Generic)

-- | Simple token representation
data Token = Token
  { tokenStart :: !Int
  , tokenEnd   :: !Int
  , tokenStyle :: !TokenStyle
  , tokenText  :: !Text
  } deriving (Show, Eq, Generic)

data TokenStyle
  = Keyword
  | String
  | Comment
  | Function
  | Type
  | Number
  | Operator
  | Error
  | Normal
  deriving (Show, Eq, Generic)

-- | Thread pool for background parsing
data ThreadPool = ThreadPool
  { poolWorkers    :: !(TVar [Async ()])
  , poolTaskQueue  :: !(TBQueue ParseTask)
  , poolNumWorkers :: !Int
  , poolShutdown   :: !(TVar Bool)
  , poolStats      :: !(TVar HighlightStats)
  }

-- | Statistics for performance monitoring
data HighlightStats = HighlightStats
  { statsTotalTasks     :: !Int
  , statsCompletedTasks :: !Int
  , statsCancelledTasks :: !Int
  , statsAverageTime    :: !NominalDiffTime
  , statsPeakMemory     :: !Int
  } deriving (Show, Generic)

-- | Incremental parsing state
data IncrementalState = IncrementalState
  { incrDirtyRegions :: !(TVar (IvMap.IntervalMap Int ()))
  , incrCachedTokens :: !(TVar (V.Vector Token))
  , incrVersion      :: !(TVar Int)
  , incrLastParse    :: !(TVar UTCTime)
  }

-- | Main async highlighter
data AsyncHighlighter = AsyncHighlighter
  { highlightPool        :: !ThreadPool
  , highlightIncremental :: !IncrementalState
  , highlightParser      :: Text -> V.Vector Token  -- ^ The actual parser
  , highlightActive      :: !(TVar (M.Map Int (Async ())))
  }

-- | Create a new thread pool
createThreadPool :: Int -> IO ThreadPool
createThreadPool numWorkers = do
  workers <- newTVarIO []
  queue <- newTBQueueIO 1000  -- Bounded queue to prevent memory issues
  shutdown <- newTVarIO False
  stats <- newTVarIO $ HighlightStats 0 0 0 0 0
  
  let pool = ThreadPool workers queue numWorkers shutdown stats
  
  -- Start worker threads
  workerList <- forM [1..numWorkers] $ \workerId ->
    async $ workerThread pool workerId
  
  atomically $ writeTVar workers workerList
  return pool

-- | Worker thread main loop
workerThread :: ThreadPool -> Int -> IO ()
workerThread ThreadPool{..} workerId = forever $ do
  -- Get next task
  task <- atomically $ do
    shutdown <- readTVar poolShutdown
    if shutdown
      then retry  -- Exit via async cancellation
      else readTBQueue poolTaskQueue
  
  -- Process task
  startTime <- getCurrentTime
  result <- try $ processTask task
  endTime <- getCurrentTime
  
  let elapsed = diffUTCTime endTime startTime
  
  case result of
    Left (e :: SomeException) -> do
      -- Log error and update stats
      putStrLn $ "Worker " ++ show workerId ++ " error: " ++ show e
      atomically $ modifyTVar poolStats $ \s ->
        s { statsCancelledTasks = statsCancelledTasks s + 1 }
        
    Right highlightResult -> do
      -- Call callback and update stats
      taskCallback task highlightResult
      atomically $ modifyTVar poolStats $ \s ->
        s { statsCompletedTasks = statsCompletedTasks s + 1
          , statsAverageTime = updateAverage (statsAverageTime s) elapsed (statsCompletedTasks s)
          }

-- | Process a single parse task
processTask :: ParseTask -> IO HighlightResult
processTask ParseTask{..} = do
  -- Simulate parsing with configurable delay
  let tokens = parseText taskContent
  
  -- For benchmarking, add slight delay based on text size
  when (taskPriority == Background) $
    threadDelay (min 10000 (T.length taskContent))  -- Max 10ms
  
  return HighlightResult
    { resultId = taskId
    , resultTokens = tokens
    , resultRegion = taskRegion
    , resultVersion = taskVersion
    , resultTime = 0  -- Would measure actual time
    }

-- | Simple lexer for demonstration
parseText :: Text -> V.Vector Token
parseText text = V.fromList $ go 0 (T.unpack text)
  where
    go :: Int -> String -> [Token]
    go _ [] = []
    go pos (c:cs)
      | c == '"' = 
          let (str, rest) = span (/= '"') cs
              len = length str + 2
          in Token pos (pos + len) String (T.pack (c : str ++ "\"")) : go (pos + len) (drop 1 rest)
      | c == '-' && take 1 cs == ['-'] =
          let (comment, rest) = span (/= '\n') cs
              len = length comment + 2
          in Token pos (pos + len) Comment (T.pack ("--" ++ comment)) : go (pos + len) rest
      | otherwise = go (pos + 1) cs

-- | Submit a task to the thread pool
submitTask :: ThreadPool -> ParseTask -> IO ()
submitTask ThreadPool{..} task = do
  success <- atomically $ do
    shutdown <- readTVar poolShutdown
    if shutdown
      then return False
      else do
        writeTBQueue poolTaskQueue task
        modifyTVar poolStats $ \s ->
          s { statsTotalTasks = statsTotalTasks s + 1 }
        return True
  
  unless success $
    putStrLn "Failed to submit task: pool shutting down"

-- | Cancel tasks for a specific version
cancelTask :: AsyncHighlighter -> Int -> IO ()
cancelTask AsyncHighlighter{..} version = do
  -- Cancel active async tasks
  activeTasks <- atomically $ do
    tasks <- readTVar highlightActive
    writeTVar highlightActive M.empty
    return tasks
  
  -- Cancel all active tasks
  forM_ (M.elems activeTasks) cancel

-- | Shutdown the thread pool gracefully
shutdownThreadPool :: ThreadPool -> IO ()
shutdownThreadPool ThreadPool{..} = do
  -- Signal shutdown
  atomically $ writeTVar poolShutdown True
  
  -- Wait for workers to finish
  workers <- readTVarIO poolWorkers
  forM_ workers $ \worker -> do
    cancel worker  -- This will interrupt the retry in workerThread
    void $ waitCatch worker
  
  -- Print final stats
  stats <- readTVarIO poolStats
  putStrLn $ "Thread pool shutdown. Stats: " ++ show stats

-- | Create a new async highlighter
createAsyncHighlighter :: Int -> (Text -> V.Vector Token) -> IO AsyncHighlighter
createAsyncHighlighter numWorkers parser = do
  pool <- createThreadPool numWorkers
  
  -- Incremental state
  dirtyRegions <- newTVarIO IvMap.empty
  cachedTokens <- newTVarIO V.empty
  version <- newTVarIO 0
  lastParse <- getCurrentTime >>= newTVarIO
  
  let incremental = IncrementalState dirtyRegions cachedTokens version lastParse
  
  active <- newTVarIO M.empty
  
  return AsyncHighlighter
    { highlightPool = pool
    , highlightIncremental = incremental
    , highlightParser = parser
    , highlightActive = active
    }

-- | Update highlighter with new content
updateHighlighter :: AsyncHighlighter -> Text -> (Int, Int) -> IO ()
updateHighlighter hl@AsyncHighlighter{..} content visibleRegion = do
  -- Increment version (cancels old tasks)
  newVersion <- atomically $ do
    modifyTVar (incrVersion highlightIncremental) (+1)
    readTVar (incrVersion highlightIncremental)
  
  -- Cancel old tasks
  cancelTask hl (newVersion - 1)
  
  -- Mark visible region as dirty
  markDirty highlightIncremental visibleRegion
  
  -- Create tasks with priority
  let (visStart, visEnd) = visibleRegion
      chunkSize = 1000  -- Parse in 1KB chunks
      
      -- High priority: visible region
      visibleTask = ParseTask
        { taskId = newVersion * 1000
        , taskPriority = Immediate
        , taskRegion = visibleRegion
        , taskContent = T.take (visEnd - visStart) $ T.drop visStart content
        , taskVersion = newVersion
        , taskCallback = handleResult hl
        }
      
      -- Medium priority: regions near visible
      nearbyTasks = 
        [ ParseTask
            { taskId = newVersion * 1000 + i
            , taskPriority = Visible
            , taskRegion = (start, end)
            , taskContent = T.take (end - start) $ T.drop start content
            , taskVersion = newVersion
            , taskCallback = handleResult hl
            }
        | i <- [1..4]
        , let offset = i * chunkSize
        , let start = max 0 (visStart - offset)
        , let end = min (T.length content) (visEnd + offset)
        ]
      
      -- Low priority: rest of file
      backgroundTasks =
        [ ParseTask
            { taskId = newVersion * 1000 + 100 + i
            , taskPriority = Background
            , taskRegion = (start, end)
            , taskContent = T.take chunkSize $ T.drop start content
            , taskVersion = newVersion
            , taskCallback = handleResult hl
            }
        | i <- [0..T.length content `div` chunkSize]
        , let start = i * chunkSize
        , let end = min (T.length content) ((i + 1) * chunkSize)
        , start < visStart - 4 * chunkSize || end > visEnd + 4 * chunkSize
        ]
  
  -- Submit all tasks
  submitTask highlightPool visibleTask
  mapM_ (submitTask highlightPool) nearbyTasks
  mapM_ (submitTask highlightPool) backgroundTasks

-- | Handle parse result
handleResult :: AsyncHighlighter -> HighlightResult -> IO ()
handleResult AsyncHighlighter{..} result = do
  currentVersion <- readTVarIO (incrVersion highlightIncremental)
  
  -- Only process if result is for current version
  when (resultVersion result == currentVersion) $ do
    atomically $ do
      -- Update cached tokens
      cached <- readTVar (incrCachedTokens highlightIncremental)
      let merged = mergeTokens cached (resultTokens result) (resultRegion result)
      writeTVar (incrCachedTokens highlightIncremental) merged
      
      -- Clear dirty region
      modifyTVar (incrDirtyRegions highlightIncremental) $
        \ivmap -> IvMap.delete (uncurry IvMap.ClosedInterval (resultRegion result)) ivmap

-- | Merge new tokens into cached tokens
mergeTokens :: V.Vector Token -> V.Vector Token -> (Int, Int) -> V.Vector Token
mergeTokens cached new (start, end) =
  let before = V.filter (\t -> tokenEnd t <= start) cached
      after = V.filter (\t -> tokenStart t >= end) cached
  in V.concat [before, new, after]

-- | Mark a region as dirty
markDirty :: IncrementalState -> (Int, Int) -> IO ()
markDirty IncrementalState{..} (start, end) =
  atomically $ modifyTVar incrDirtyRegions $
    IvMap.insert (IvMap.ClosedInterval start end) ()

-- | Process incremental update
processIncrementalUpdate :: AsyncHighlighter -> Int -> Text -> IO ()
processIncrementalUpdate hl@AsyncHighlighter{..} offset newText = do
  -- Mark affected region as dirty
  markDirty highlightIncremental (offset, offset + T.length newText)
  
  -- Trigger reparse of dirty regions
  dirtyList <- atomically $ do
    dirty <- readTVar (incrDirtyRegions highlightIncremental)
    return $ IvMap.toAscList dirty
  
  -- Submit tasks for dirty regions
  forM_ dirtyList $ \(IvMap.ClosedInterval start end, _) ->
    updateHighlighter hl newText (start, end)

-- | Update running average
updateAverage :: NominalDiffTime -> NominalDiffTime -> Int -> NominalDiffTime
updateAverage oldAvg newTime count =
  let countD = fromIntegral count
      oldAvgD = realToFrac oldAvg
      newTimeD = realToFrac newTime
  in realToFrac $ (oldAvgD * countD + newTimeD) / (countD + 1)

-- | Benchmark highlighting performance
measureHighlightPerformance :: AsyncHighlighter -> Text -> IO HighlightStats
measureHighlightPerformance hl content = do
  -- Reset stats
  atomically $ writeTVar (poolStats $ highlightPool hl) $
    HighlightStats 0 0 0 0 0
  
  -- Measure initial memory
  performGC
  mem0 <- getCurrentMemoryUsage
  
  -- Start timing
  start <- getCPUTime
  
  -- Trigger full parse
  updateHighlighter hl content (0, min 10000 (T.length content))
  
  -- Wait for completion (simplified - would track actual completion)
  threadDelay 1000000  -- 1 second
  
  -- End timing
  end <- getCPUTime
  let elapsed = fromIntegral (end - start) / (10^12) :: Double
  
  -- Measure final memory
  performGC
  mem1 <- getCurrentMemoryUsage
  
  -- Get final stats
  stats <- readTVarIO (poolStats $ highlightPool hl)
  
  return stats
    { statsPeakMemory = mem1 - mem0
    , statsAverageTime = realToFrac elapsed
    }

-- | Get current memory usage in bytes
getCurrentMemoryUsage :: IO Int
getCurrentMemoryUsage = do
  performGC
  -- Simplified - would use actual memory stats
  return 1000000  -- 1MB placeholder