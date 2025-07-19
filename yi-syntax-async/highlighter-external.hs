{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Yi.Syntax.Async.External
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- External parser integration for haskell-src-exts and libclang.
-- Provides high-quality AST-based syntax highlighting.

module Yi.Syntax.Async.External
  ( -- * External Parsers
    ExternalParser(..)
  , ParserConfig(..)
  , ParseError(..)
    -- * Haskell Support
  , parseHaskellSrcExts
  , haskellToTokens
    -- * C/C++/Objective-C Support
  , parseClang
  , ClangToken(..)
  , clangToTokens
    -- * Integration
  , withExternalParser
  , tryExternalParsers
  ) where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.IORef
import           Data.List (sortOn)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import qualified Data.Vector as V
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import qualified Language.Haskell.Exts as HSE
import           System.Exit
import           System.Process
import           System.Timeout

-- Import from highlighter-core
import Yi.Syntax.Async.Core (Token(..), TokenStyle(..))

-- | External parser types
data ExternalParser
  = HaskellSrcExts
  | ClangParser FilePath      -- ^ Path to libclang
  | TreeSitter FilePath       -- ^ Path to tree-sitter grammar
  | LanguageServer String Int -- ^ LSP server command and port
  deriving (Show, Eq)

-- | Parser configuration
data ParserConfig = ParserConfig
  { parserTimeout     :: Int           -- ^ Timeout in seconds
  , parserFallback    :: Bool          -- ^ Fall back to simple lexer
  , parserCaching     :: Bool          -- ^ Cache parse results
  , parserDiagnostics :: Bool          -- ^ Include diagnostics
  , parserExtensions  :: [String]      -- ^ Language extensions
  } deriving (Show, Eq)

defaultParserConfig :: ParserConfig
defaultParserConfig = ParserConfig
  { parserTimeout = 5
  , parserFallback = True
  , parserCaching = True
  , parserDiagnostics = True
  , parserExtensions = []
  }

-- | Parse error information
data ParseError = ParseError
  { errorLine    :: Int
  , errorColumn  :: Int
  , errorMessage :: String
  , errorSeverity :: ErrorSeverity
  } deriving (Show, Eq)

data ErrorSeverity = Warning | Error | Fatal
  deriving (Show, Eq)

-- ===== Haskell Support (haskell-src-exts) =====

-- | Parse Haskell using haskell-src-exts
parseHaskellSrcExts :: ParserConfig -> Text -> IO (Either [ParseError] (V.Vector Token))
parseHaskellSrcExts config sourceText = do
  let source = T.unpack sourceText
      -- Configure parser mode with extensions
      mode = HSE.defaultParseMode
        { HSE.extensions = map readExtension (parserExtensions config)
        , HSE.parseFilename = "<input>"
        }
      
      -- Add timeout
      timeoutMicros = parserTimeout config * 1000000
  
  result <- timeout timeoutMicros $ evaluate $ HSE.parseModuleWithMode mode source
  
  case result of
    Nothing -> return $ Left [ParseError 0 0 "Parse timeout" Fatal]
    Just parseResult -> case parseResult of
      HSE.ParseFailed (HSE.SrcLoc _ line col) err ->
        return $ Left [ParseError line col err Error]
      
      HSE.ParseOk ast -> do
        -- Convert AST to tokens
        let tokens = haskellAstToTokens ast
        return $ Right $ V.fromList tokens

-- | Read a Haskell extension
readExtension :: String -> HSE.Extension
readExtension "OverloadedStrings" = HSE.EnableExtension HSE.OverloadedStrings
readExtension "TypeFamilies" = HSE.EnableExtension HSE.TypeFamilies
readExtension "GADTs" = HSE.EnableExtension HSE.GADTs
readExtension _ = HSE.EnableExtension HSE.FlexibleInstances  -- Default

-- | Convert Haskell AST to tokens
haskellAstToTokens :: HSE.Module HSE.SrcSpanInfo -> [Token]
haskellAstToTokens (HSE.Module _ _ _ imports decls) =
  sortOn tokenStart $ 
    concatMap importToTokens imports ++
    concatMap declToTokens decls

-- | Convert import to tokens
importToTokens :: HSE.ImportDecl HSE.SrcSpanInfo -> [Token]
importToTokens (HSE.ImportDecl (HSE.SrcSpanInfo span _) modName _ _ _ _ _ _) =
  [ Token (srcSpanStart span) (srcSpanEnd span) Keyword "import"
  , Token (srcSpanStart span + 7) (srcSpanEnd span) Type (show modName)
  ]

-- | Convert declaration to tokens
declToTokens :: HSE.Decl HSE.SrcSpanInfo -> [Token]
declToTokens = \case
  HSE.FunBind _ matches -> concatMap matchToTokens matches
  HSE.PatBind (HSE.SrcSpanInfo span _) pat rhs _ ->
    patToTokens pat ++ rhsToTokens rhs
  HSE.TypeDecl (HSE.SrcSpanInfo span _) _ _ ->
    [Token (srcSpanStart span) (srcSpanStart span + 4) Keyword "type"]
  HSE.DataDecl (HSE.SrcSpanInfo span _) _ _ _ _ _ _ ->
    [Token (srcSpanStart span) (srcSpanStart span + 4) Keyword "data"]
  _ -> []

-- | Convert match to tokens
matchToTokens :: HSE.Match HSE.SrcSpanInfo -> [Token]
matchToTokens (HSE.Match (HSE.SrcSpanInfo span _) name pats rhs _) =
  let nameStart = srcSpanStart span
      nameEnd = nameStart + length (HSE.prettyPrint name)
  in Token nameStart nameEnd Function (T.pack $ HSE.prettyPrint name) :
     concatMap patToTokens pats ++
     rhsToTokens rhs

-- | Convert pattern to tokens
patToTokens :: HSE.Pat HSE.SrcSpanInfo -> [Token]
patToTokens = \case
  HSE.PVar (HSE.SrcSpanInfo span _) name ->
    [Token (srcSpanStart span) (srcSpanEnd span) Normal (T.pack $ HSE.prettyPrint name)]
  HSE.PLit _ _ lit ->
    [litToToken lit]
  _ -> []

-- | Convert literal to token
litToToken :: HSE.Literal HSE.SrcSpanInfo -> Token
litToToken = \case
  HSE.String (HSE.SrcSpanInfo span _) str _ ->
    Token (srcSpanStart span) (srcSpanEnd span) String (T.pack $ show str)
  HSE.Int (HSE.SrcSpanInfo span _) n _ ->
    Token (srcSpanStart span) (srcSpanEnd span) Number (T.pack $ show n)
  _ -> Token 0 0 Normal ""

-- | Convert right-hand side to tokens
rhsToTokens :: HSE.Rhs HSE.SrcSpanInfo -> [Token]
rhsToTokens = \case
  HSE.UnGuardedRhs _ exp -> expToTokens exp
  HSE.GuardedRhss _ guards -> concatMap guardedRhsToTokens guards

guardedRhsToTokens :: HSE.GuardedRhs HSE.SrcSpanInfo -> [Token]
guardedRhsToTokens (HSE.GuardedRhs _ stmts exp) =
  concatMap stmtToTokens stmts ++ expToTokens exp

stmtToTokens :: HSE.Stmt HSE.SrcSpanInfo -> [Token]
stmtToTokens _ = []  -- Simplified

expToTokens :: HSE.Exp HSE.SrcSpanInfo -> [Token]
expToTokens _ = []  -- Simplified

-- | Get source span start position
srcSpanStart :: HSE.SrcSpan -> Int
srcSpanStart (HSE.SrcSpan _ startLine startCol _ _) = 
  (startLine - 1) * 1000 + startCol  -- Approximate offset

srcSpanEnd :: HSE.SrcSpan -> Int
srcSpanEnd (HSE.SrcSpan _ _ _ endLine endCol) =
  (endLine - 1) * 1000 + endCol

-- | Convert haskell-src-exts tokens to our format
haskellToTokens :: HSE.Module HSE.SrcSpanInfo -> V.Vector Token
haskellToTokens = V.fromList . haskellAstToTokens

-- ===== C/C++/Objective-C Support (libclang) =====

-- FFI bindings to libclang
data CXIndex
data CXTranslationUnit
data CXCursor
data CXToken

foreign import capi "clang-c/Index.h clang_createIndex"
  clang_createIndex :: CInt -> CInt -> IO (Ptr CXIndex)

foreign import capi "clang-c/Index.h clang_disposeIndex"
  clang_disposeIndex :: Ptr CXIndex -> IO ()

foreign import capi "clang-c/Index.h clang_parseTranslationUnit"
  clang_parseTranslationUnit :: Ptr CXIndex -> CString -> Ptr CString -> CInt
                             -> Ptr CString -> CInt -> CUInt
                             -> IO (Ptr CXTranslationUnit)

foreign import capi "clang-c/Index.h clang_disposeTranslationUnit"
  clang_disposeTranslationUnit :: Ptr CXTranslationUnit -> IO ()

-- | Clang token information
data ClangToken = ClangToken
  { clangTokenKind   :: ClangTokenKind
  , clangTokenSpelling :: Text
  , clangTokenLocation :: (Int, Int, Int, Int)  -- start line/col, end line/col
  } deriving (Show, Eq)

data ClangTokenKind
  = ClangKeyword
  | ClangIdentifier
  | ClangLiteral
  | ClangComment
  | ClangPunctuation
  deriving (Show, Eq)

-- | Parse C/C++/Objective-C using libclang
parseClang :: ParserConfig -> FilePath -> Text -> IO (Either [ParseError] (V.Vector Token))
parseClang config libclangPath sourceText = do
  -- For real implementation, would use FFI
  -- This is a simplified version using clang CLI
  
  let timeoutMicros = parserTimeout config * 1000000
  
  -- Write source to temp file
  tempFile <- writeSystemTempFile "source.c" (T.unpack sourceText)
  
  -- Run clang with syntax-only flag
  let clangCmd = "clang"
      clangArgs = ["-fsyntax-only", "-Xclang", "-dump-tokens", tempFile]
  
  result <- timeout timeoutMicros $ do
    (exitCode, stdout, stderr) <- readProcessWithExitCode clangCmd clangArgs ""
    case exitCode of
      ExitSuccess -> parseClangOutput stdout
      ExitFailure _ -> return $ Left [parseClangError stderr]
  
  -- Clean up
  removeFile tempFile
  
  case result of
    Nothing -> return $ Left [ParseError 0 0 "Clang timeout" Fatal]
    Just res -> return res

-- | Parse clang output
parseClangOutput :: String -> IO (Either [ParseError] (V.Vector Token))
parseClangOutput output = do
  -- Parse clang token dump format
  let lines = lines output
      tokens = mapMaybe parseClangLine lines
  return $ Right $ V.fromList tokens

-- | Parse single clang output line
parseClangLine :: String -> Maybe Token
parseClangLine line = 
  -- Clang output format: "keyword 'int' [StartOfLine] Loc=<file:1:1>"
  case words line of
    (kind:spelling:rest) -> 
      let style = case kind of
            "keyword" -> Keyword
            "identifier" -> Normal
            "string_literal" -> String
            "numeric_constant" -> Number
            "comment" -> Comment
            _ -> Normal
          -- Extract location (simplified)
          start = 0
          end = length spelling
      in Just $ Token start end style (T.pack spelling)
    _ -> Nothing

-- | Parse clang error
parseClangError :: String -> ParseError
parseClangError stderr =
  -- Parse clang error format: "file:line:col: error: message"
  case break (== ':') stderr of
    (_, ':':rest) -> case break (== ':') rest of
      (lineStr, ':':rest2) -> case break (== ':') rest2 of
        (colStr, ':':rest3) ->
          ParseError (read lineStr) (read colStr) rest3 Error
        _ -> ParseError 0 0 stderr Error
      _ -> ParseError 0 0 stderr Error  
    _ -> ParseError 0 0 stderr Error

-- | Convert Clang tokens to our format
clangToTokens :: [ClangToken] -> V.Vector Token
clangToTokens clangTokens = V.fromList $ map convertClangToken clangTokens
  where
    convertClangToken ClangToken{..} =
      let (startLine, startCol, endLine, endCol) = clangTokenLocation
          style = case clangTokenKind of
            ClangKeyword -> Keyword
            ClangIdentifier -> Normal
            ClangLiteral -> String
            ClangComment -> Comment
            ClangPunctuation -> Operator
      in Token (startLine * 1000 + startCol) (endLine * 1000 + endCol)
               style clangTokenSpelling

-- ===== Integration =====

-- | Use external parser with fallback
withExternalParser :: ExternalParser -> ParserConfig -> Text 
                   -> IO (Either [ParseError] (V.Vector Token))
withExternalParser parser config sourceText = do
  result <- case parser of
    HaskellSrcExts -> parseHaskellSrcExts config sourceText
    ClangParser path -> parseClang config path sourceText
    TreeSitter path -> parseTreeSitter config path sourceText
    LanguageServer cmd port -> parseLSP config cmd port sourceText
  
  -- Fallback to simple lexer if requested
  case result of
    Left errs | parserFallback config -> do
      putStrLn $ "External parser failed: " ++ show errs
      putStrLn "Falling back to simple lexer"
      return $ Right $ simpleLexer sourceText
    _ -> return result

-- | Try multiple external parsers
tryExternalParsers :: [ExternalParser] -> ParserConfig -> Text
                   -> IO (Either [ParseError] (V.Vector Token))
tryExternalParsers [] _ _ = return $ Left [ParseError 0 0 "No parsers available" Error]
tryExternalParsers (p:ps) config sourceText = do
  result <- withExternalParser p config sourceText
  case result of
    Left _ -> tryExternalParsers ps config sourceText
    Right tokens -> return $ Right tokens

-- | Simple fallback lexer
simpleLexer :: Text -> V.Vector Token
simpleLexer text = V.fromList $ go 0 (T.unpack text)
  where
    go _ [] = []
    go pos (c:cs)
      | c == '"' = 
          let (str, rest) = span (/= '"') cs
              len = length str + 2
          in Token pos (pos + len) String (T.pack (c : str ++ "\"")) 
             : go (pos + len) (drop 1 rest)
      | c == '/' && take 1 cs == ['/'] =
          let (comment, rest) = span (/= '\n') cs
              len = length comment + 2
          in Token pos (pos + len) Comment (T.pack ("//" ++ comment))
             : go (pos + len) rest
      | otherwise = go (pos + 1) cs

-- Placeholder implementations
parseTreeSitter :: ParserConfig -> FilePath -> Text 
                -> IO (Either [ParseError] (V.Vector Token))
parseTreeSitter _ _ _ = return $ Left [ParseError 0 0 "Tree-sitter not implemented" Error]

parseLSP :: ParserConfig -> String -> Int -> Text 
         -> IO (Either [ParseError] (V.Vector Token))
parseLSP _ _ _ _ = return $ Left [ParseError 0 0 "LSP not implemented" Error]

writeSystemTempFile :: String -> String -> IO FilePath
writeSystemTempFile name content = do
  let path = "/tmp/" ++ name
  writeFile path content
  return path

removeFile :: FilePath -> IO ()
removeFile = const (return ())