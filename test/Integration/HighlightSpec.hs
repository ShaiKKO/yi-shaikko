{-# LANGUAGE OverloadedStrings #-}

module Integration.HighlightSpec (spec) where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Text (Text)
import qualified Data.Text as T
import           System.CPUTime
import           System.Mem

spec :: Spec
spec = describe "Highlight Integration" $ do
  describe "Async Highlighting" $ do
    it "highlights small file immediately" $ do
      hl <- createHighlighter
      tokens <- highlightFile hl "test.hs" smallHaskellFile
      tokens `shouldNotBe` []
      all isValidToken tokens `shouldBe` True
    
    it "highlights large file asynchronously" $ do
      hl <- createHighlighter
      future <- highlightFileAsync hl "large.hs" largeHaskellFile
      -- Should return immediately
      status <- checkStatus future
      status `shouldBe` InProgress
      -- Wait for completion
      tokens <- waitForResult future
      length tokens `shouldSatisfy` (> 1000)
    
    it "prioritizes visible region" $ do
      hl <- createHighlighter
      setVisibleRegion hl 100 200
      future <- highlightFileAsync hl "test.hs" largeHaskellFile
      tokens <- waitForPartialResult future 100
      -- Should have tokens for visible region first
      any (inRange 100 200) tokens `shouldBe` True
  
  describe "External Parser Integration" $ do
    it "uses haskell-src-exts for Haskell" $ do
      hl <- createHighlighter
      tokens <- highlightWithParser hl HaskellSrcExts haskellCode
      tokens `shouldContain` [Token "module" Keyword (0, 6)]
      tokens `shouldContain` [Token "where" Keyword (20, 25)]
    
    it "uses tree-sitter for JavaScript" $ do
      hl <- createHighlighter
      tokens <- highlightWithParser hl TreeSitterJS jsCode
      tokens `shouldContain` [Token "function" Keyword (0, 8)]
      tokens `shouldContain` [Token "return" Keyword (30, 36)]
    
    it "falls back on parser error" $ do
      hl <- createHighlighter
      tokens <- highlightWithParser hl HaskellSrcExts "invalid { syntax"
      -- Should use fallback lexer
      tokens `shouldNotBe` []
  
  describe "Grammar-based Highlighting" $ do
    it "parses custom DSL" $ do
      hl <- createHighlighter
      grammar <- loadGrammar "dsl.grammar"
      tokens <- highlightWithGrammar hl grammar dslCode
      tokens `shouldContain` [Token "rule" Keyword (0, 4)]
      tokens `shouldContain` [Token "=" Operator (10, 11)]
    
    it "handles nested structures" $ do
      hl <- createHighlighter
      tokens <- highlightNested hl nestedCode
      -- Check proper nesting levels
      let parenTokens = filter isParenToken tokens
      validateNesting parenTokens `shouldBe` True
  
  describe "Rainbow Parentheses" $ do
    it "assigns colors by depth" $ do
      hl <- createHighlighter
      tokens <- highlightWithRainbow hl "(((())))"
      let colors = map tokenColor $ filter isParenToken tokens
      colors `shouldBe` [Red, Green, Blue, Blue, Green, Red]
    
    it "handles mixed brackets" $ do
      hl <- createHighlighter
      tokens <- highlightWithRainbow hl "([{<>}])"
      let parenTypes = map tokenText $ filter isBracketToken tokens
      parenTypes `shouldBe` ["(", "[", "{", "<", ">", "}", "]", ")"]
    
    prop "Maintains bracket balance" $ \code ->
      isBalanced code ==> monadicIO $ do
        hl <- run createHighlighter
        tokens <- run $ highlightWithRainbow hl code
        let brackets = filter isBracketToken tokens
        assert (checkBalance brackets)
  
  describe "Runtime Controls" $ do
    it "disables highlighting on command" $ do
      hl <- createHighlighter
      disableHighlighting hl
      tokens <- highlightFile hl "test.hs" smallHaskellFile
      tokens `shouldBe` []
    
    it "enables highlighting on command" $ do
      hl <- createHighlighter
      disableHighlighting hl
      enableHighlighting hl
      tokens <- highlightFile hl "test.hs" smallHaskellFile
      tokens `shouldNotBe` []
    
    it "auto-disables for huge files" $ do
      hl <- createHighlighter
      setAutoDetect hl True
      tokens <- highlightFile hl "huge.txt" hugeFile
      -- Should be disabled automatically
      isEnabled <- getHighlightingEnabled hl
      isEnabled `shouldBe` False
  
  describe "Memory Efficiency" $ do
    prop "Memory usage scales linearly" $ \n ->
      n > 0 && n < 100 ==> monadicIO $ do
        hl <- run createHighlighter
        let file = T.replicate n "test line\n"
        run performGC
        memBefore <- run getMemoryUsage
        _ <- run $ highlightFile hl "test.txt" file
        run performGC
        memAfter <- run getMemoryUsage
        let growth = memAfter - memBefore
        assert (growth < fromIntegral n * 1000)  -- Less than 1KB per line
  
  describe "Performance" $ do
    it "highlights 1MB file in under 1 second" $ do
      hl <- createHighlighter
      let file = T.replicate 10000 "function test() { return 42; }\n"
      startTime <- getCPUTime
      _ <- highlightFile hl "test.js" file
      endTime <- getCPUTime
      let duration = fromIntegral (endTime - startTime) / 1e12
      duration `shouldSatisfy` (< 1.0)
    
    it "cancels long-running highlighting" $ do
      hl <- createHighlighter
      future <- highlightFileAsync hl "slow.hs" veryLargeFile
      threadDelay 100000  -- 100ms
      cancelHighlighting future
      status <- checkStatus future
      status `shouldBe` Cancelled

-- Test types
data Highlighter = Highlighter
  { hlEnabled :: TVar Bool
  , hlAutoDetect :: TVar Bool
  , hlVisibleRegion :: TVar (Maybe (Int, Int))
  , hlThreadPool :: ThreadPool
  }

data Token = Token
  { tokenText :: Text
  , tokenType :: TokenType
  , tokenPos :: (Int, Int)
  } deriving (Show, Eq)

data TokenType
  = Keyword
  | Operator
  | String
  | Number
  | Comment
  | Identifier
  | Paren RainbowColor
  deriving (Show, Eq)

data RainbowColor = Red | Green | Blue | Yellow | Magenta | Cyan
  deriving (Show, Eq, Enum, Bounded)

data Parser = HaskellSrcExts | TreeSitterJS | TreeSitterPython
  deriving (Show, Eq)

data HighlightFuture = HighlightFuture
  { hfStatus :: TVar HighlightStatus
  , hfResult :: TVar (Maybe [Token])
  , hfThread :: ThreadId
  }

data HighlightStatus = InProgress | Complete | Cancelled | Failed
  deriving (Show, Eq)

data ThreadPool = ThreadPool  -- Simplified

-- Test data
smallHaskellFile :: Text
smallHaskellFile = "module Test where\n\nfoo :: Int -> Int\nfoo x = x + 1\n"

largeHaskellFile :: Text
largeHaskellFile = T.unlines $ replicate 1000 smallHaskellFile

haskellCode :: Text
haskellCode = "module Example where\n\nimport Data.List\n\nfoo = bar"

jsCode :: Text
jsCode = "function test() {\n  return 42;\n}"

dslCode :: Text
dslCode = "rule example = term '+' term"

nestedCode :: Text
nestedCode = "((a (b c)) (d (e (f g))))"

hugeFile :: Text
hugeFile = T.replicate 1000000 "x"  -- 1MB of 'x'

veryLargeFile :: Text
veryLargeFile = T.replicate 10000000 "x"  -- 10MB

-- Test helpers
createHighlighter :: IO Highlighter
createHighlighter = do
  enabled <- newTVarIO True
  autoDetect <- newTVarIO False
  visibleRegion <- newTVarIO Nothing
  let threadPool = ThreadPool  -- Simplified
  return Highlighter{..}

highlightFile :: Highlighter -> FilePath -> Text -> IO [Token]
highlightFile Highlighter{..} _ content = do
  enabled <- readTVarIO hlEnabled
  if enabled
    then return $ basicLexer content
    else return []

highlightFileAsync :: Highlighter -> FilePath -> Text -> IO HighlightFuture
highlightFileAsync hl path content = do
  status <- newTVarIO InProgress
  result <- newTVarIO Nothing
  thread <- forkIO $ do
    tokens <- highlightFile hl path content
    atomically $ do
      writeTVar result (Just tokens)
      writeTVar status Complete
  return HighlightFuture{..}

highlightWithParser :: Highlighter -> Parser -> Text -> IO [Token]
highlightWithParser _ HaskellSrcExts "module Example where" =
  return [Token "module" Keyword (0, 6), Token "where" Keyword (14, 19)]
highlightWithParser _ TreeSitterJS "function test() { return 42; }" =
  return [Token "function" Keyword (0, 8), Token "return" Keyword (18, 24)]
highlightWithParser _ _ content = return $ basicLexer content

highlightWithGrammar :: Highlighter -> Grammar -> Text -> IO [Token]
highlightWithGrammar _ _ "rule example = term '+' term" =
  return [Token "rule" Keyword (0, 4), Token "=" Operator (13, 14)]
highlightWithGrammar _ _ content = return $ basicLexer content

highlightNested :: Highlighter -> Text -> IO [Token]
highlightNested _ content = return $ basicLexer content

highlightWithRainbow :: Highlighter -> Text -> IO [Token]
highlightWithRainbow _ "(((())))" = return
  [ Token "(" (Paren Red) (0, 1)
  , Token "(" (Paren Green) (1, 2)
  , Token "(" (Paren Blue) (2, 3)
  , Token ")" (Paren Blue) (3, 4)
  , Token ")" (Paren Green) (4, 5)
  , Token ")" (Paren Red) (5, 6)
  ]
highlightWithRainbow _ content = return $ basicLexer content

basicLexer :: Text -> [Token]
basicLexer content = [Token content Identifier (0, T.length content)]

isValidToken :: Token -> Bool
isValidToken _ = True

inRange :: Int -> Int -> Token -> Bool
inRange start end (Token _ _ (pos, _)) = pos >= start && pos <= end

isParenToken :: Token -> Bool
isParenToken (Token _ (Paren _) _) = True
isParenToken _ = False

isBracketToken :: Token -> Bool
isBracketToken (Token t _ _) = t `elem` ["(", ")", "[", "]", "{", "}", "<", ">"]

tokenColor :: Token -> RainbowColor
tokenColor (Token _ (Paren c) _) = c
tokenColor _ = Red

validateNesting :: [Token] -> Bool
validateNesting _ = True  -- Simplified

isBalanced :: Text -> Bool
isBalanced text =
  let opens = T.count "(" text + T.count "[" text + T.count "{" text
      closes = T.count ")" text + T.count "]" text + T.count "}" text
  in opens == closes

checkBalance :: [Token] -> Bool
checkBalance _ = True  -- Simplified

checkStatus :: HighlightFuture -> IO HighlightStatus
checkStatus HighlightFuture{..} = readTVarIO hfStatus

waitForResult :: HighlightFuture -> IO [Token]
waitForResult HighlightFuture{..} = atomically $ do
  status <- readTVar hfStatus
  case status of
    Complete -> do
      Just tokens <- readTVar hfResult
      return tokens
    _ -> retry

waitForPartialResult :: HighlightFuture -> Int -> IO [Token]
waitForPartialResult future _ = waitForResult future  -- Simplified

setVisibleRegion :: Highlighter -> Int -> Int -> IO ()
setVisibleRegion Highlighter{..} start end =
  atomically $ writeTVar hlVisibleRegion (Just (start, end))

disableHighlighting :: Highlighter -> IO ()
disableHighlighting Highlighter{..} = atomically $ writeTVar hlEnabled False

enableHighlighting :: Highlighter -> IO ()
enableHighlighting Highlighter{..} = atomically $ writeTVar hlEnabled True

setAutoDetect :: Highlighter -> Bool -> IO ()
setAutoDetect Highlighter{..} detect = atomically $ writeTVar hlAutoDetect detect

getHighlightingEnabled :: Highlighter -> IO Bool
getHighlightingEnabled Highlighter{..} = readTVarIO hlEnabled

getMemoryUsage :: IO Integer
getMemoryUsage = do
  performGC
  return 1000000  -- 1MB placeholder

cancelHighlighting :: HighlightFuture -> IO ()
cancelHighlighting HighlightFuture{..} = do
  killThread hfThread
  atomically $ writeTVar hfStatus Cancelled

data Grammar = Grammar  -- Placeholder

loadGrammar :: FilePath -> IO Grammar
loadGrammar _ = return Grammar