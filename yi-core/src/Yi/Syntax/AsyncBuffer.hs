{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  Yi.Syntax.AsyncBuffer
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Buffer-level asynchronous syntax highlighting with external parser support.
-- This module provides async highlighting that has full access to buffer content.

module Yi.Syntax.AsyncBuffer
  ( AsyncHighlighter(..)
  , mkAsyncHighlighter
  , runAsyncHighlight
  , cancelAsyncHighlight
  , AsyncHighlightState(..)
  , ExternalParser(..)
  ) where

import           Control.Concurrent (ThreadId, killThread)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception (catch, SomeException)
import           Control.Monad
import           Data.Binary
import           Data.Default (Default(..))
import           Data.IORef
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import           Data.Typeable
import           GHC.Generics
import           Lens.Micro.Platform
import           System.Process
import           System.Exit (ExitCode(..))
import           Yi.Buffer
import           Yi.Buffer.Misc (getBufferDyn, putBufferDyn)
import           Yi.Core
import           Yi.Rope (YiString)
import qualified Yi.Rope as R
import           Yi.Style
import           Yi.Syntax
import           Yi.Types

-- | External parser configuration (reuse from Async.hs)
data ExternalParser = 
    TreeSitter FilePath        -- ^ Path to tree-sitter parser
  | HaskellSrcExts             -- ^ Use haskell-src-exts
  | LanguageServer Text        -- ^ LSP semantic tokens
  | CustomParser FilePath [String]  -- ^ Custom external parser
  deriving (Show, Eq)

-- | State of async highlighting for a buffer
data AsyncHighlightState = AsyncHighlightState
  { ahsWorkerThread :: Maybe ThreadId  -- Use ThreadId since Async isn't serializable
  , ahsProgress :: Double  -- ^ 0.0 to 1.0
  , ahsLastUpdate :: UTCTime
  , ahsExternalParser :: Maybe ExternalParser
  } deriving (Typeable, Generic)

-- | Create initial async highlight state
instance Default AsyncHighlightState where
  def = AsyncHighlightState Nothing 0.0 (UTCTime (ModifiedJulianDay 0) 0) Nothing

-- Binary instance for serialization
instance Binary AsyncHighlightState where
  put AsyncHighlightState{..} = do
    put (Nothing :: Maybe ThreadId)  -- Don't serialize thread
    put ahsProgress
    put ahsLastUpdate
    put ahsExternalParser
  get = do
    _ :: Maybe ThreadId <- get
    progress <- get
    lastUpdate <- get
    parser <- get
    return $ AsyncHighlightState Nothing progress lastUpdate parser

-- Make ExternalParser serializable
instance Binary ExternalParser where
  put (TreeSitter path) = putWord8 0 >> put path
  put HaskellSrcExts = putWord8 1
  put (LanguageServer cmd) = putWord8 2 >> put cmd
  put (CustomParser cmd args) = putWord8 3 >> put cmd >> put args
  get = do
    tag <- getWord8
    case tag of
      0 -> TreeSitter <$> get
      1 -> return HaskellSrcExts
      2 -> LanguageServer <$> get
      3 -> CustomParser <$> get <*> get
      _ -> fail "Invalid ExternalParser tag"

-- Make it a YiVariable so it can be stored in buffer
instance YiVariable AsyncHighlightState

-- | Async highlighter for a specific mode
data AsyncHighlighter = AsyncHighlighter
  { ahName :: Text
  , ahApplies :: FilePath -> Bool
  , ahParser :: ExternalParser
  , ahParseBuffer :: BufferRef -> YiString -> YiM ()
  }

-- | Create an async highlighter for a mode
mkAsyncHighlighter :: Text -> (FilePath -> Bool) -> ExternalParser -> AsyncHighlighter
mkAsyncHighlighter name applies parser = AsyncHighlighter
  { ahName = name
  , ahApplies = applies
  , ahParser = parser
  , ahParseBuffer = parseBufferWithExternal parser
  }

-- | Run async highlighting on a buffer
runAsyncHighlight :: AsyncHighlighter -> BufferRef -> YiM ()
runAsyncHighlight AsyncHighlighter{..} bufRef = do
  -- Get buffer content
  content <- withGivenBuffer bufRef $ do
    size <- sizeB
    if size > 0
      then readRegionB (mkRegion 0 size)
      else return R.empty
  
  -- Cancel any existing worker
  cancelAsyncHighlight bufRef
  
  -- Start new async worker
  worker <- io $ async $ do
    -- Run external parser
    result <- parseWithExternal ahParser (R.toText content)
    
    -- Update buffer with results (would need to be implemented)
    -- For now, just log progress
    return ()
  
  -- Store worker reference
  storeAsyncWorker bufRef worker

-- | Cancel async highlighting for a buffer
cancelAsyncHighlight :: BufferRef -> YiM ()
cancelAsyncHighlight bufRef = withGivenBuffer bufRef $ do
  state <- getBufferDyn
  case ahsWorkerThread state of
    Just tid -> io $ killThread tid
    Nothing -> return ()
  putBufferDyn $ state { ahsWorkerThread = Nothing }

-- | Store async worker reference for a buffer
storeAsyncWorker :: BufferRef -> Async () -> YiM ()
storeAsyncWorker bufRef worker = withGivenBuffer bufRef $ do
  tid <- io $ asyncThreadId worker
  state <- getBufferDyn
  putBufferDyn $ state { ahsWorkerThread = Just tid }

-- | Parse buffer content with external parser
parseBufferWithExternal :: ExternalParser -> BufferRef -> YiString -> YiM ()
parseBufferWithExternal parser bufRef content = do
  -- Start async parsing
  worker <- io $ async $ do
    result <- parseWithExternal parser (R.toText content)
    case result of
      Just tokens -> do
        -- Convert token positions to Spans/Strokes
        let strokes = map tokenToStroke tokens
            tokenToStroke (start, end, tokType) = 
              Span (Point start) (tokenTypeToStyle tokType) (Point end)
        
        -- Apply syntax highlighting results to buffer
        -- Note: We can't directly update the syntax tree from here,
        -- but we can store the results in the AsyncHighlightState
        -- and have the UI render them on next refresh
        return ()
      Nothing -> return ()
  
  -- Store the worker
  storeAsyncWorker bufRef worker

-- | Run external parser on content
parseWithExternal :: ExternalParser -> Text -> IO (Maybe [(Int, Int, Text)])
parseWithExternal (TreeSitter path) content = do
  -- Call tree-sitter parser
  (exitCode, stdout, stderr) <- readProcessWithExitCode path 
    ["parse", "--json", "-"] (T.unpack content)
  
  case exitCode of
    ExitSuccess -> do
      -- Parse JSON output from tree-sitter
      -- Return token positions and types
      return $ Just []
    _ -> return Nothing

parseWithExternal HaskellSrcExts content = do
  -- Use haskell-src-exts to parse Haskell code
  -- Would need proper integration
  return Nothing

parseWithExternal (LanguageServer lspCmd) content = do
  -- Use LSP semantic tokens
  -- Would need LSP client implementation
  return Nothing

parseWithExternal (CustomParser cmd args) content = do
  -- Run custom parser
  (exitCode, stdout, stderr) <- readProcessWithExitCode cmd args (T.unpack content)
  case exitCode of
    ExitSuccess -> do
      -- Parse output format: START END TYPE
      let parseLines = map parseLine . lines $ stdout
          parseLine line = case words line of
            [start, end, tokType] -> 
              Just (read start, read end, T.pack tokType)
            _ -> Nothing
      return $ Just $ catMaybes parseLines
    _ -> return Nothing
  where
    catMaybes :: [Maybe a] -> [a]
    catMaybes = foldr (\x acc -> case x of Just v -> v:acc; Nothing -> acc) []

-- | Convert token type string to StyleName
tokenTypeToStyle :: Text -> StyleName
tokenTypeToStyle tokType = case T.toLower tokType of
  "keyword" -> keywordStyle
  "function" -> variableStyle  -- Use variableStyle for functions
  "variable" -> variableStyle
  "string" -> stringStyle
  "comment" -> commentStyle
  "number" -> numberStyle
  "operator" -> operatorStyle
  "type" -> typeStyle
  "constant" -> numberStyle  -- Use numberStyle for constants
  "builtin" -> builtinStyle
  _ -> const mempty  -- defaultStyle is mempty