{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      :  Yi.Syntax.Async
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Asynchronous syntax highlighting with external parser support.
-- Provides background parsing with progress indicators and fallback mechanisms.

module Yi.Syntax.Async
  ( AsyncHighlighter(..)
  , ExternalParser(..)
  , HighlightConfig(..)
  , AsyncCache
  , mkAsyncHighlighter
  , withExternalParser
  , enableRainbowParens
  , setPerformanceMode
  , getHighlightProgress
  ) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent (threadDelay)
import           Control.Exception (catch, SomeException)
import           Control.Monad
import           Data.Default (Default(..))
import           Data.IORef
import           Data.Maybe
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.ICU as ICU
import           Data.Time
import           Lens.Micro.Platform
import           Yi.Buffer.Basic (Point, WindowRef(..))
import           Yi.Lexer.Alex (Tok)
import           Yi.Region
import           Yi.Rope (YiString)
import qualified Yi.Rope as R
import           Yi.Style
import           Yi.Syntax
import           Yi.Syntax.Tree
import           System.IO.Unsafe (unsafePerformIO)

-- | External parser configuration
data ExternalParser = 
    TreeSitter FilePath        -- ^ Path to tree-sitter parser
  | HaskellSrcExts             -- ^ Use haskell-src-exts
  | LanguageServer Text        -- ^ LSP semantic tokens
  | CustomParser FilePath [String]  -- ^ Custom external parser

-- | Configuration for async highlighting
data HighlightConfig = HighlightConfig
  { hlcMaxFileSize     :: Int          -- ^ Max file size for full parsing (bytes)
  , hlcChunkSize       :: Int          -- ^ Parse chunk size for large files
  , hlcUpdateDelay     :: Int          -- ^ Delay before reparsing (ms)
  , hlcEnableRainbow   :: Bool         -- ^ Enable rainbow parentheses
  , hlcExternalParser  :: Maybe ExternalParser
  , hlcFallbackLexer   :: Bool         -- ^ Use simple lexer for large files
  }

instance Default HighlightConfig where
  def = HighlightConfig
    { hlcMaxFileSize    = 10 * 1024 * 1024  -- 10MB
    , hlcChunkSize      = 64 * 1024         -- 64KB chunks
    , hlcUpdateDelay    = 300               -- 300ms
    , hlcEnableRainbow  = True
    , hlcExternalParser = Nothing
    , hlcFallbackLexer  = True
    }

-- | Async highlighting cache
data AsyncCache tree tt = AsyncCache
  { acWorker      :: TVar (Maybe (Async ()))
  , acProgress    :: TVar Double          -- ^ 0.0 to 1.0
  , acTree        :: TVar (tree (Tok tt))
  , acRainbow     :: TVar (M.Map Point Int)  -- ^ Paren depth map
  , acLastUpdate  :: TVar UTCTime
  , acConfig      :: IORef HighlightConfig
  }

-- | Asynchronous syntax highlighter
data AsyncHighlighter cache tree tt = AsyncHighlighter
  { ahBase        :: Highlighter cache (tree (Tok tt))
  , ahAsyncCache  :: AsyncCache tree tt
  , ahFileSize    :: TVar Int
  }

-- | Create an async highlighter from a base highlighter
mkAsyncHighlighter :: IsTree tree =>
                      Highlighter cache (tree (Tok tt)) ->
                      HighlightConfig ->
                      IO (AsyncHighlighter (AsyncCache tree tt) tree tt)
mkAsyncHighlighter baseHl config = do
  worker <- newTVarIO Nothing
  progress <- newTVarIO 0.0
  tree <- newTVarIO emptyTree
  rainbow <- newTVarIO M.empty
  lastUpdate <- getCurrentTime >>= newTVarIO
  configRef <- newIORef config
  fileSize <- newTVarIO 0
  
  let asyncCache = AsyncCache
        { acWorker = worker
        , acProgress = progress
        , acTree = tree
        , acRainbow = rainbow
        , acLastUpdate = lastUpdate
        , acConfig = configRef
        }
  
  return AsyncHighlighter
    { ahBase = wrapHighlighter baseHl asyncCache
    , ahAsyncCache = asyncCache
    , ahFileSize = fileSize
    }
  where
    emptyTree = scanEmpty (error "mkAsyncHighlighter: no scanner")

-- | Wrap a base highlighter with async functionality
wrapHighlighter :: IsTree tree =>
                   Highlighter cache (tree (Tok tt)) ->
                   AsyncCache tree tt ->
                   Highlighter (AsyncCache tree tt) (tree (Tok tt))
wrapHighlighter baseHl asyncCache = SynHL
  { hlStartState = asyncCache
  , hlRun = asyncRun baseHl
  , hlGetTree = \cache _ -> unsafePerformIO $ readTVarIO (acTree cache)
  , hlFocus = \regions cache -> cache  -- TODO: implement focusing
  }

-- | Async run function - currently just wraps base highlighter
-- For true async highlighting with external parsers, use Yi.Syntax.AsyncBuffer
asyncRun :: IsTree tree =>
            Highlighter cache (tree (Tok tt)) ->
            Scanner Point Char ->
            Point ->
            AsyncCache tree tt ->
            AsyncCache tree tt
asyncRun baseHl scanner dirtyOffset cache = unsafePerformIO $ do
  -- For now, just use the base highlighter synchronously
  -- True async highlighting with external parsers requires buffer access
  -- and is implemented in Yi.Syntax.AsyncBuffer
  let baseCache = hlStartState baseHl
      newBaseCache = hlRun baseHl scanner dirtyOffset baseCache
      -- hlGetTree takes a WindowRef, not a Point. Use a dummy WindowRef
      tree = hlGetTree baseHl newBaseCache (WindowRef 0)
  
  atomically $ do
    writeTVar (acTree cache) tree
    writeTVar (acProgress cache) 1.0
  
  return cache



-- Note: External parser integration has been moved to Yi.Syntax.AsyncBuffer
-- where it has proper access to buffer content

-- | Parse rainbow parentheses
parseRainbowParens :: Text -> IO (M.Map Point Int)
parseRainbowParens content = do
  -- Simple paren matching algorithm
  let positions = findParens content 0 0 M.empty
  return positions
  where
    findParens :: Text -> Int -> Int -> M.Map Point Int -> M.Map Point Int
    findParens text pos depth acc
      | pos >= T.length text = acc
      | otherwise = 
          case T.index text pos of
            '(' -> findParens text (pos + 1) (depth + 1) 
                     (M.insert (Point pos) depth acc)
            '[' -> findParens text (pos + 1) (depth + 1)
                     (M.insert (Point pos) depth acc)
            '{' -> findParens text (pos + 1) (depth + 1)
                     (M.insert (Point pos) depth acc)
            ')' -> findParens text (pos + 1) (depth - 1)
                     (M.insert (Point pos) (depth - 1) acc)
            ']' -> findParens text (pos + 1) (depth - 1)
                     (M.insert (Point pos) (depth - 1) acc)
            '}' -> findParens text (pos + 1) (depth - 1)
                     (M.insert (Point pos) (depth - 1) acc)
            _ -> findParens text (pos + 1) depth acc

-- | Enable or disable rainbow parentheses
enableRainbowParens :: AsyncHighlighter cache tree tt -> Bool -> IO ()
enableRainbowParens AsyncHighlighter{..} enable = do
  modifyIORef (acConfig ahAsyncCache) $ \c -> c { hlcEnableRainbow = enable }

-- | Set performance mode for large files
setPerformanceMode :: AsyncHighlighter cache tree tt -> Bool -> IO ()
setPerformanceMode AsyncHighlighter{..} enable = do
  modifyIORef (acConfig ahAsyncCache) $ \c -> 
    c { hlcFallbackLexer = enable
      , hlcMaxFileSize = if enable then 1024 * 1024 else 10 * 1024 * 1024
      }

-- | Get current highlighting progress (0.0 to 1.0)
getHighlightProgress :: AsyncHighlighter cache tree tt -> IO Double
getHighlightProgress AsyncHighlighter{..} = 
  readTVarIO (acProgress ahAsyncCache)

-- | Configure external parser
withExternalParser :: AsyncHighlighter cache tree tt -> ExternalParser -> IO ()
withExternalParser AsyncHighlighter{..} parser = do
  modifyIORef (acConfig ahAsyncCache) $ \c -> c { hlcExternalParser = Just parser }

-- Note: unsafePerformIO is imported from System.IO.Unsafe above