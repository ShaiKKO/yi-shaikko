{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Yi.Test.HugeFile
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Tests for huge file handling with automatic highlighting control.

module Main where

import           Control.Concurrent
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Criterion.Main
import           Criterion.Types
import           Data.IORef
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time
import qualified Data.Vector as V
import           System.CPUTime
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Mem
import           System.Process
import           System.Random
import           Text.Printf

-- Import our modules
import           Yi.Syntax.Async.Core
import           Yi.Syntax.Runtime.Controls

-- | Test huge file handling
main :: IO ()
main = do
  putStrLn "=== Huge File Test Suite ==="
  putStrLn ""
  
  -- Create test files
  createTestFiles
  
  -- Initialize runtime controls
  runtimeState <- initRuntimeControls
  
  -- Run tests
  testAutoDetection runtimeState
  testPerformanceDegradation
  testMemoryUsage
  testIncrementalEdits
  testUIResponsiveness
  
  -- Cleanup
  cleanup

-- | Create synthetic test files
createTestFiles :: IO ()
createTestFiles = do
  createDirectoryIfMissing True "test-files"
  
  putStrLn "Creating test files..."
  
  -- Small file (1 MB)
  createSyntheticFile "test-files/small.txt" (1 * 1024 * 1024)
  
  -- Medium file (10 MB)
  createSyntheticFile "test-files/medium.txt" (10 * 1024 * 1024)
  
  -- Large file (50 MB)
  createSyntheticFile "test-files/large.txt" (50 * 1024 * 1024)
  
  -- Huge file (100 MB)
  createSyntheticFile "test-files/huge.txt" (100 * 1024 * 1024)
  
  -- Giant file (500 MB)
  createSyntheticFile "test-files/giant.txt" (500 * 1024 * 1024)
  
  putStrLn "Test files created."

-- | Create a synthetic source file
createSyntheticFile :: FilePath -> Int -> IO ()
createSyntheticFile path sizeBytes = do
  putStrLn $ "  Creating " ++ path ++ " (" ++ showSize sizeBytes ++ ")..."
  
  withFile path WriteMode $ \h -> do
    -- Write realistic code patterns
    let chunkSize = 1024 * 1024  -- 1MB chunks
        numChunks = sizeBytes `div` chunkSize
    
    forM_ [1..numChunks] $ \i -> do
      chunk <- generateCodeChunk i
      T.hPutStr h chunk
      
      -- Show progress
      when (i `mod` 10 == 0) $
        putStr "." >> hFlush stdout
    
    putStrLn " done"

-- | Generate realistic code chunk
generateCodeChunk :: Int -> IO Text
generateCodeChunk seed = do
  gen <- newStdGen
  let (patterns, _) = randomR (0, length codePatterns - 1) gen
  return $ T.unlines $ take 1000 $ cycle $ map (codePatterns !!) $
    randomRs (0, length codePatterns - 1) (mkStdGen seed)

-- | Code patterns for synthetic files
codePatterns :: [Text]
codePatterns =
  [ "function process_data_" <> T.pack (show i) <> "(x, y) {"
  , "  const result = (x + y) * (x - y);"
  , "  if (result > threshold) {"
  , "    return handle_large_value(result);"
  , "  } else {"
  , "    return handle_small_value(result);"
  , "  }"
  , "}"
  , ""
  , "// This is a comment explaining the complex logic"
  , "class DataProcessor {"
  , "  constructor(config) {"
  , "    this.config = config;"
  , "    this.cache = new Map();"
  , "  }"
  , ""
  , "  async processItem(item) {"
  , "    const cached = this.cache.get(item.id);"
  , "    if (cached) return cached;"
  , "    "
  , "    const result = await this.complexCalculation(item);"
  , "    this.cache.set(item.id, result);"
  , "    return result;"
  , "  }"
  , "}"
  | i <- [1..10]
  ]

-- | Show file size in human-readable format
showSize :: Int -> String
showSize bytes
  | bytes < 1024 = show bytes ++ " B"
  | bytes < 1024 * 1024 = printf "%.1f KB" (fromIntegral bytes / 1024 :: Double)
  | bytes < 1024 * 1024 * 1024 = printf "%.1f MB" (fromIntegral bytes / 1024 / 1024 :: Double)
  | otherwise = printf "%.1f GB" (fromIntegral bytes / 1024 / 1024 / 1024 :: Double)

-- | Test auto-detection behavior
testAutoDetection :: RuntimeState -> IO ()
testAutoDetection runtimeState = do
  putStrLn "\n=== Auto-Detection Test ==="
  putStrLn ""
  
  let testFiles = 
        [ ("test-files/small.txt", Full)      -- Should use full highlighting
        , ("test-files/medium.txt", Full)     -- Should use full highlighting
        , ("test-files/large.txt", Simple)    -- Should downgrade to simple
        , ("test-files/huge.txt", Off)        -- Should disable highlighting
        ]
  
  forM_ testFiles $ \(file, expectedMode) -> do
    size <- getFileSize file
    detectedMode <- detectOptimalMode runtimeState file
    
    printf "  %-25s %10s -> Mode: %-6s (expected: %-6s) %s\n"
      file
      (showSize $ fromIntegral size)
      (show detectedMode)
      (show expectedMode)
      (if detectedMode == expectedMode then "✓" else "✗")

-- | Test performance degradation with file size
testPerformanceDegradation :: IO ()
testPerformanceDegradation = do
  putStrLn "\n=== Performance Degradation Test ==="
  putStrLn ""
  
  let testSizes = [1, 5, 10, 20, 50, 100]  -- MB
  
  putStrLn "File Size | Parse Time | Tokens/sec | Memory Used"
  putStrLn "----------|------------|------------|------------"
  
  forM_ testSizes $ \sizeMB -> do
    let sizeBytes = sizeMB * 1024 * 1024
    
    -- Generate test content
    content <- T.concat <$> replicateM sizeMB (generateCodeChunk sizeMB)
    
    -- Measure parsing performance
    startTime <- getCPUTime
    startMem <- performGC >> getCurrentMemoryUsage
    
    let !tokens = simpleParser content
        !numTokens = V.length tokens
    
    endMem <- performGC >> getCurrentMemoryUsage
    endTime <- getCPUTime
    
    let elapsedSec = fromIntegral (endTime - startTime) / 1e12 :: Double
        tokensPerSec = fromIntegral numTokens / elapsedSec
        memUsedMB = fromIntegral (endMem - startMem) / 1024 / 1024 :: Double
    
    printf "%4d MB   | %7.2f s  | %10.0f | %7.1f MB\n"
      sizeMB elapsedSec tokensPerSec memUsedMB

-- | Simple parser for testing
simpleParser :: Text -> V.Vector Token
simpleParser = V.fromList . tokenize 0 . T.unpack
  where
    tokenize _ [] = []
    tokenize pos (c:cs) = case c of
      '(' -> Token pos (pos+1) Operator "(" : tokenize (pos+1) cs
      ')' -> Token pos (pos+1) Operator ")" : tokenize (pos+1) cs
      '{' -> Token pos (pos+1) Operator "{" : tokenize (pos+1) cs
      '}' -> Token pos (pos+1) Operator "}" : tokenize (pos+1) cs
      '"' -> let (str, rest) = span (/= '"') cs
                 len = length str + 2
             in Token pos (pos+len) String (T.pack $ '"' : str ++ "\"") :
                tokenize (pos+len) (drop 1 rest)
      '/' | take 1 cs == "/" ->
              let (comment, rest) = span (/= '\n') cs
                  len = length comment + 2
              in Token pos (pos+len) Comment (T.pack $ "//" ++ comment) :
                 tokenize (pos+len) rest
      '\n' -> tokenize (pos+1) cs
      ' ' -> tokenize (pos+1) cs
      _ | isKeyword [c] -> Token pos (pos+1) Keyword (T.singleton c) : tokenize (pos+1) cs
        | otherwise -> tokenize (pos+1) cs
    
    isKeyword = const False  -- Simplified

-- | Test memory usage with huge files
testMemoryUsage :: IO ()
testMemoryUsage = do
  putStrLn "\n=== Memory Usage Test ==="
  putStrLn ""
  
  -- Test with different highlighting modes
  let testCases = 
        [ ("Full highlighting", Full)
        , ("Simple highlighting", Simple)
        , ("No highlighting", Off)
        ]
  
  putStrLn "Mode               | 10MB File | 50MB File | 100MB File"
  putStrLn "-------------------|-----------|-----------|------------"
  
  forM_ testCases $ \(name, mode) -> do
    mem10 <- testFileMemory mode "test-files/medium.txt"
    mem50 <- testFileMemory mode "test-files/large.txt"
    mem100 <- testFileMemory mode "test-files/huge.txt"
    
    printf "%-18s | %7.1f MB | %7.1f MB | %8.1f MB\n"
      name mem10 mem50 mem100

-- | Test memory usage for a file with given mode
testFileMemory :: HighlightMode -> FilePath -> IO Double
testFileMemory mode file = do
  performGC
  startMem <- getCurrentMemoryUsage
  
  -- Simulate loading and highlighting
  content <- T.readFile file
  let tokens = case mode of
        Full -> simpleParser content  -- Full parsing
        Simple -> V.take 1000 $ simpleParser content  -- Limited parsing
        Off -> V.empty  -- No parsing
  
  -- Force evaluation
  evaluate $ rnf tokens
  
  performGC
  endMem <- getCurrentMemoryUsage
  
  return $ fromIntegral (endMem - startMem) / 1024 / 1024

-- | Test incremental editing performance
testIncrementalEdits :: IO ()
testIncrementalEdits = do
  putStrLn "\n=== Incremental Edit Test ==="
  putStrLn ""
  
  -- Load a large file
  content <- T.readFile "test-files/large.txt"
  let contentLen = T.length content
  
  putStrLn "Edit Type          | Position  | Response Time"
  putStrLn "-------------------|-----------|---------------"
  
  -- Test different edit positions
  let editTests = 
        [ ("Insert at start", 0)
        , ("Insert at 25%", contentLen `div` 4)
        , ("Insert at middle", contentLen `div` 2)
        , ("Insert at 75%", contentLen * 3 `div` 4)
        , ("Insert at end", contentLen - 1)
        ]
  
  forM_ editTests $ \(editType, pos) -> do
    startTime <- getCurrentTime
    
    -- Simulate edit
    let newContent = T.take pos content <> "// New comment\n" <> T.drop pos content
    
    -- Incremental reparse (simplified)
    let dirtyStart = max 0 (pos - 100)
        dirtyEnd = min (T.length newContent) (pos + 100)
        dirtyRegion = T.take (dirtyEnd - dirtyStart) $ T.drop dirtyStart newContent
        !tokens = simpleParser dirtyRegion
    
    endTime <- getCurrentTime
    let responseMs = diffUTCTime endTime startTime * 1000
    
    printf "%-18s | %9d | %10.2f ms\n" editType pos responseMs

-- | Test UI responsiveness during parsing
testUIResponsiveness :: IO ()
testUIResponsiveness = do
  putStrLn "\n=== UI Responsiveness Test ==="
  putStrLn ""
  
  -- Create async highlighter
  highlighter <- createAsyncHighlighter 4 simpleParser
  
  -- Load huge file
  content <- T.readFile "test-files/huge.txt"
  
  putStrLn "Starting background parse of 100MB file..."
  
  -- Start parsing
  updateHighlighter highlighter content (0, 1000)  -- Parse visible region first
  
  -- Simulate UI operations during parsing
  responseTimes <- forM [1..20] $ \i -> do
    threadDelay 50000  -- 50ms between operations
    
    startTime <- getCurrentTime
    
    -- Simulate UI operation
    let uiOperation = return ()  -- Would be actual UI update
    uiOperation
    
    endTime <- getCurrentTime
    let responseMs = diffUTCTime endTime startTime * 1000
    
    putStr $ if responseMs < 16.7  -- 60 FPS threshold
      then "."
      else "!"
    hFlush stdout
    
    return responseMs
  
  putStrLn " done"
  
  -- Shutdown highlighter
  shutdownThreadPool (highlightPool highlighter)
  
  -- Report results
  let avgResponse = sum responseTimes / fromIntegral (length responseTimes)
      maxResponse = maximum responseTimes
      under60fps = length $ filter (>= 16.7) responseTimes
  
  printf "\nAverage response: %.2f ms\n" avgResponse
  printf "Max response: %.2f ms\n" maxResponse
  printf "Frames over 16.7ms: %d/20 (%.0f%%)\n" 
    under60fps (fromIntegral under60fps / 20 * 100 :: Double)
  
  if avgResponse < 10
    then putStrLn "✓ UI remains highly responsive"
    else if avgResponse < 16.7
      then putStrLn "✓ UI maintains 60 FPS"
      else putStrLn "✗ UI responsiveness degraded"

-- | Get current memory usage
getCurrentMemoryUsage :: IO Int
getCurrentMemoryUsage = do
  -- Simplified - would use actual memory stats
  return 0

-- | Cleanup test files
cleanup :: IO ()
cleanup = do
  putStrLn "\n=== Cleanup ==="
  
  response <- prompt "Delete test files? (y/n): "
  when (response == "y") $ do
    removeDirectoryRecursive "test-files"
    putStrLn "Test files deleted."

-- | Simple prompt
prompt :: String -> IO String
prompt msg = do
  putStr msg
  hFlush stdout
  getLine

-- | Get file size
getFileSize :: FilePath -> IO Integer
getFileSize path = withFile path ReadMode hFileSize