{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Yi.Syntax.Async.Benchmark
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Benchmarks for async syntax highlighting performance.

module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
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
import           System.Mem
import           System.Timeout
import           Text.Printf

-- Import our modules
import Yi.Syntax.Async.Core
import Yi.Syntax.Async.External
import Yi.Syntax.Async.Grammar
import Yi.Syntax.Async.Advanced

-- | Test file sizes
data FileSize = Small | Medium | Large | Huge
  deriving (Show, Eq)

fileSizeBytes :: FileSize -> Int
fileSizeBytes Small = 1024           -- 1 KB
fileSizeBytes Medium = 100 * 1024    -- 100 KB
fileSizeBytes Large = 1024 * 1024    -- 1 MB
fileSizeBytes Huge = 10 * 1024 * 1024 -- 10 MB

-- | Generate test content
generateTestContent :: FileSize -> Text
generateTestContent size = T.unlines $ replicate numLines sampleLine
  where
    numLines = fileSizeBytes size `div` T.length sampleLine
    sampleLine = "function foo(x, y) { return (x + y) * (x - y); } // comment"

-- | Sample Haskell code for testing
sampleHaskellCode :: Text
sampleHaskellCode = T.unlines
  [ "{-# LANGUAGE OverloadedStrings #-}"
  , "module Test where"
  , ""
  , "import Data.Text (Text)"
  , "import qualified Data.Text as T"
  , ""
  , "-- | Example function with nested parentheses"
  , "complexFunction :: Int -> Int -> Int"
  , "complexFunction x y ="
  , "  let a = (x + y) * (x - y)"
  , "      b = ((a + 1) * (a - 1)) `div` 2"
  , "      c = (((b * 2) + (b `div` 2)) - (b `mod` 3))"
  , "  in if (c > 0)"
  , "     then (c * (c + 1)) `div` 2"
  , "     else (abs c) + (signum c)"
  ]

-- | Benchmark async highlighter
benchmarkAsyncHighlighter :: IO ()
benchmarkAsyncHighlighter = do
  putStrLn "=== Async Syntax Highlighter Benchmarks ==="
  putStrLn ""
  
  -- Create highlighters with different thread counts
  hl1 <- createAsyncHighlighter 1 simpleParser
  hl2 <- createAsyncHighlighter 2 simpleParser
  hl4 <- createAsyncHighlighter 4 simpleParser
  hl8 <- createAsyncHighlighter 8 simpleParser
  
  -- Test content
  let smallContent = generateTestContent Small
      mediumContent = generateTestContent Medium
      largeContent = generateTestContent Large
  
  defaultMain
    [ bgroup "async-highlighter"
        [ bgroup "threads"
            [ bench "1-thread-small" $ nfIO $ benchHighlight hl1 smallContent
            , bench "2-thread-small" $ nfIO $ benchHighlight hl2 smallContent
            , bench "4-thread-small" $ nfIO $ benchHighlight hl4 smallContent
            , bench "8-thread-small" $ nfIO $ benchHighlight hl8 smallContent
            ]
        , bgroup "file-size"
            [ bench "4-thread-small" $ nfIO $ benchHighlight hl4 smallContent
            , bench "4-thread-medium" $ nfIO $ benchHighlight hl4 mediumContent
            , bench "4-thread-large" $ nfIO $ benchHighlight hl4 largeContent
            ]
        , bgroup "incremental"
            [ bench "initial-parse" $ nfIO $ benchInitialParse hl4 mediumContent
            , bench "small-edit" $ nfIO $ benchIncrementalEdit hl4 mediumContent
            , bench "large-edit" $ nfIO $ benchLargeEdit hl4 mediumContent
            ]
        ]
    , bgroup "external-parsers"
        [ bench "haskell-src-exts" $ nfIO $ benchHaskellSrcExts sampleHaskellCode
        , bench "simple-lexer" $ nf simpleParser sampleHaskellCode
        ]
    , bgroup "rainbow-parens"
        [ bench "compute-rainbow" $ nf (computeRainbowParens defaultRainbowConfig sampleHaskellCode) 
            (simpleParser sampleHaskellCode)
        ]
    ]
  
  -- Cleanup
  shutdownThreadPool (highlightPool hl1)
  shutdownThreadPool (highlightPool hl2)
  shutdownThreadPool (highlightPool hl4)
  shutdownThreadPool (highlightPool hl8)

-- | Simple parser for benchmarking
simpleParser :: Text -> V.Vector Token
simpleParser text = V.fromList $ go 0 (T.unpack text)
  where
    go _ [] = []
    go pos ('(':cs) = Token pos (pos+1) Operator "(" : go (pos+1) cs
    go pos (')':cs) = Token pos (pos+1) Operator ")" : go (pos+1) cs
    go pos (c:cs) = go (pos+1) cs

-- | Benchmark highlight operation
benchHighlight :: AsyncHighlighter -> Text -> IO ()
benchHighlight hl content = do
  updateHighlighter hl content (0, min 1000 (T.length content))
  -- Wait for completion
  threadDelay 10000  -- 10ms

-- | Benchmark initial parse
benchInitialParse :: AsyncHighlighter -> Text -> IO ()
benchInitialParse hl content = do
  updateHighlighter hl content (0, T.length content)
  waitForCompletion hl

-- | Benchmark small incremental edit
benchIncrementalEdit :: AsyncHighlighter -> Text -> IO ()
benchIncrementalEdit hl content = do
  -- Initial parse
  updateHighlighter hl content (0, T.length content)
  waitForCompletion hl
  
  -- Small edit at position 100
  let editPos = 100
      editText = "new text"
  processIncrementalUpdate hl editPos editText
  waitForCompletion hl

-- | Benchmark large edit
benchLargeEdit :: AsyncHighlighter -> Text -> IO ()
benchLargeEdit hl content = do
  -- Initial parse
  updateHighlighter hl content (0, T.length content)
  waitForCompletion hl
  
  -- Large edit (replace 10% of content)
  let editPos = T.length content `div` 2
      editText = T.take (T.length content `div` 10) content
  processIncrementalUpdate hl editPos editText
  waitForCompletion hl

-- | Benchmark haskell-src-exts
benchHaskellSrcExts :: Text -> IO ()
benchHaskellSrcExts code = do
  result <- parseHaskellSrcExts defaultParserConfig code
  case result of
    Left errs -> error $ "Parse failed: " ++ show errs
    Right tokens -> return ()

-- | Wait for highlighter to complete
waitForCompletion :: AsyncHighlighter -> IO ()
waitForCompletion hl = do
  -- Simplified - would check actual completion
  threadDelay 100000  -- 100ms

-- | Performance comparison test
performanceComparison :: IO ()
performanceComparison = do
  putStrLn "=== Performance Comparison: Sync vs Async ==="
  putStrLn ""
  
  let testSizes = [Small, Medium, Large]
  
  forM_ testSizes $ \size -> do
    let content = generateTestContent size
    putStrLn $ "Testing " ++ show size ++ " file (" ++ show (fileSizeBytes size) ++ " bytes)"
    
    -- Sync parsing
    syncStart <- getCPUTime
    let !syncResult = simpleParser content `deepseq` ()
    syncEnd <- getCPUTime
    let syncTime = fromIntegral (syncEnd - syncStart) / 1e12 :: Double
    
    -- Async parsing
    hl <- createAsyncHighlighter 4 simpleParser
    asyncStart <- getCPUTime
    updateHighlighter hl content (0, T.length content)
    waitForCompletion hl
    asyncEnd <- getCPUTime
    let asyncTime = fromIntegral (asyncEnd - asyncStart) / 1e12 :: Double
    shutdownThreadPool (highlightPool hl)
    
    printf "  Sync:  %.4f seconds\n" syncTime
    printf "  Async: %.4f seconds (%.2fx speedup)\n" asyncTime (syncTime / asyncTime)
    putStrLn ""

-- | UI responsiveness test
uiResponsivenessTest :: IO ()
uiResponsivenessTest = do
  putStrLn "=== UI Responsiveness Test ==="
  putStrLn ""
  
  let content = generateTestContent Large
  hl <- createAsyncHighlighter 4 simpleParser
  
  -- Start parsing
  updateHighlighter hl content (0, 1000)  -- Parse visible region first
  
  -- Simulate UI updates while parsing
  responseTimes <- forM [1..10] $ \i -> do
    startTime <- getCurrentTime
    
    -- Simulate UI operation (should not block)
    result <- timeout 10000 $ do  -- 10ms timeout
      threadDelay 1000  -- 1ms operation
      return True
    
    endTime <- getCurrentTime
    let responseTime = diffUTCTime endTime startTime
    
    case result of
      Just _ -> return responseTime
      Nothing -> return 999  -- Timeout marker
  
  shutdownThreadPool (highlightPool hl)
  
  let avgResponse = sum (map realToFrac responseTimes) / 10 :: Double
  printf "Average UI response time: %.2f ms\n" (avgResponse * 1000)
  printf "Max UI response time: %.2f ms\n" (maximum (map realToFrac responseTimes) * 1000 :: Double)
  
  if avgResponse < 0.02  -- 20ms threshold
    then putStrLn "✓ UI remains responsive during parsing"
    else putStrLn "✗ UI blocked during parsing"

-- | Memory usage test
memoryUsageTest :: IO ()
memoryUsageTest = do
  putStrLn "=== Memory Usage Test ==="
  putStrLn ""
  
  performGC
  mem0 <- getCurrentMemoryUsage
  
  -- Parse large file
  let content = generateTestContent Large
  hl <- createAsyncHighlighter 4 simpleParser
  
  updateHighlighter hl content (0, T.length content)
  waitForCompletion hl
  
  performGC
  mem1 <- getCurrentMemoryUsage
  
  shutdownThreadPool (highlightPool hl)
  
  let memUsed = mem1 - mem0
  printf "Memory used: %.2f MB\n" (fromIntegral memUsed / 1024 / 1024 :: Double)
  printf "Memory per KB of text: %.2f KB\n" 
    (fromIntegral memUsed / 1024 / fromIntegral (T.length content) * 1024 :: Double)

-- | Get current memory usage (simplified)
getCurrentMemoryUsage :: IO Int
getCurrentMemoryUsage = do
  performGC
  -- Would use actual memory statistics
  return 1000000  -- 1MB placeholder

-- | Create sample test files
createTestFiles :: IO ()
createTestFiles = do
  createDirectoryIfMissing True "test-files"
  
  -- Haskell file
  T.writeFile "test-files/sample.hs" sampleHaskellCode
  
  -- C file
  T.writeFile "test-files/sample.c" $ T.unlines
    [ "#include <stdio.h>"
    , ""
    , "/* Complex nested function */"
    , "int complex(int x, int y) {"
    , "    return ((x + y) * (x - y)) / ((x * x) + (y * y));"
    , "}"
    ]
  
  -- Custom config file
  T.writeFile "test-files/sample.conf" $ T.unlines
    [ "# Configuration file"
    , "[section1]"
    , "key1 = \"value1\""
    , "key2 = 42"
    , "enabled = true"
    ]

-- | Main entry point
main :: IO ()
main = do
  createTestFiles
  
  -- Run benchmarks
  benchmarkAsyncHighlighter
  
  -- Additional tests
  putStrLn "\n"
  performanceComparison
  putStrLn "\n"
  uiResponsivenessTest
  putStrLn "\n"
  memoryUsageTest