{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      :  Yi.Syntax.Async.Grammar
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Grammar-based syntax highlighting using Megaparsec.
-- Allows users to define custom syntax highlighters via YAML/JSON configuration.

module Yi.Syntax.Async.Grammar
  ( -- * Grammar Definition
    Grammar(..)
  , Pattern(..)
  , PatternType(..)
  , Scope(..)
    -- * Grammar Loading
  , loadGrammarFromYAML
  , loadGrammarFromJSON
  , validateGrammar
    -- * Parser Generation
  , grammarToParser
  , compilePattern
    -- * Example Grammars
  , exampleConfigGrammar
  , exampleMarkdownGrammar
  , exampleINIGrammar
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString as BS
import           Data.Char (isAlphaNum)
import           Data.Either
import           Data.List (find, sortOn)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Data.Void
import           Data.Yaml (decodeFileEither, ParseException)
import           GHC.Generics
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Regex.PCRE

-- Import from highlighter-core
import Yi.Syntax.Async.Core (Token(..), TokenStyle(..))

-- | Grammar definition
data Grammar = Grammar
  { grammarName         :: Text
  , grammarFileTypes    :: [Text]       -- ^ File extensions
  , grammarPatterns     :: [Pattern]    -- ^ Ordered patterns
  , grammarRepository   :: M.Map Text Pattern  -- ^ Named patterns
  , grammarInjections   :: [Injection]  -- ^ Language injections
  , grammarVariables    :: M.Map Text Text    -- ^ Reusable regex parts
  } deriving (Show, Eq, Generic)

instance FromJSON Grammar
instance ToJSON Grammar

-- | Pattern definition
data Pattern = Pattern
  { patternName    :: Maybe Text
  , patternType    :: PatternType
  , patternScope   :: Scope
  , patternInclude :: Maybe Text      -- ^ Reference to repository
  } deriving (Show, Eq, Generic)

instance FromJSON Pattern
instance ToJSON Pattern

-- | Pattern matching type
data PatternType
  = Match Text                         -- ^ Single regex match
  | Begin Text Text                    -- ^ Begin/end pair
  | Patterns [Pattern]                 -- ^ Nested patterns
  | Include Text                       -- ^ Include from repository
  deriving (Show, Eq, Generic)

instance FromJSON PatternType where
  parseJSON = withObject "PatternType" $ \v -> do
    match <- v .:? "match"
    begin <- v .:? "begin"
    end <- v .:? "end"
    patterns <- v .:? "patterns"
    include <- v .:? "include"
    
    case (match, begin, end, patterns, include) of
      (Just m, _, _, _, _) -> return $ Match m
      (_, Just b, Just e, _, _) -> return $ Begin b e
      (_, _, _, Just ps, _) -> return $ Patterns ps
      (_, _, _, _, Just i) -> return $ Include i
      _ -> fail "Invalid pattern type"

instance ToJSON PatternType where
  toJSON = \case
    Match m -> object ["match" .= m]
    Begin b e -> object ["begin" .= b, "end" .= e]
    Patterns ps -> object ["patterns" .= ps]
    Include i -> object ["include" .= i]

-- | Token scope/style
data Scope
  = ScopeKeyword
  | ScopeString
  | ScopeComment
  | ScopeFunction
  | ScopeType
  | ScopeNumber
  | ScopeOperator
  | ScopeConstant
  | ScopeVariable
  | ScopeCustom Text
  deriving (Show, Eq, Generic)

instance FromJSON Scope where
  parseJSON = withText "Scope" $ \case
    "keyword" -> return ScopeKeyword
    "string" -> return ScopeString
    "comment" -> return ScopeComment
    "function" -> return ScopeFunction
    "type" -> return ScopeType
    "number" -> return ScopeNumber
    "operator" -> return ScopeOperator
    "constant" -> return ScopeConstant
    "variable" -> return ScopeVariable
    other -> return $ ScopeCustom other

instance ToJSON Scope where
  toJSON = \case
    ScopeKeyword -> "keyword"
    ScopeString -> "string"
    ScopeComment -> "comment"
    ScopeFunction -> "function"
    ScopeType -> "type"
    ScopeNumber -> "number"
    ScopeOperator -> "operator"
    ScopeConstant -> "constant"
    ScopeVariable -> "variable"
    ScopeCustom t -> String t

-- | Language injection
data Injection = Injection
  { injectionSelector :: Text          -- ^ Where to inject
  , injectionLanguage :: Text          -- ^ Language to inject
  } deriving (Show, Eq, Generic)

instance FromJSON Injection
instance ToJSON Injection

-- | Megaparsec parser type
type Parser = Parsec Void Text

-- | Load grammar from YAML file
loadGrammarFromYAML :: FilePath -> IO (Either String Grammar)
loadGrammarFromYAML path = do
  result <- decodeFileEither path
  case result of
    Left err -> return $ Left $ show err
    Right grammar -> return $ Right grammar

-- | Load grammar from JSON file
loadGrammarFromJSON :: FilePath -> IO (Either String Grammar)
loadGrammarFromJSON path = do
  content <- BS.readFile path
  case eitherDecodeStrict content of
    Left err -> return $ Left err
    Right grammar -> return $ Right grammar

-- | Validate grammar definition
validateGrammar :: Grammar -> Either [String] Grammar
validateGrammar grammar@Grammar{..} = do
  -- Check for circular includes
  let checkCircular :: Text -> Pattern -> Either String ()
      checkCircular name (Pattern _ (Include ref) _ _) =
        if ref == name
          then Left $ "Circular reference: " ++ T.unpack name
          else Right ()
      checkCircular _ _ = Right ()
  
  -- Validate all patterns
  let errors = lefts
        [ checkCircular name pat
        | (name, pat) <- M.toList grammarRepository
        ]
  
  if null errors
    then Right grammar
    else Left errors

-- | Convert grammar to Megaparsec parser
grammarToParser :: Grammar -> Parser [Token]
grammarToParser Grammar{..} = do
  -- Try each pattern in order
  tokens <- many $ choice $ map (try . patternToParser grammarRepository) grammarPatterns
  eof
  return $ concat tokens

-- | Convert pattern to parser
patternToParser :: M.Map Text Pattern -> Pattern -> Parser [Token]
patternToParser repo Pattern{..} = case patternType of
  Match regex -> do
    -- Use regex matching
    (start, matched) <- matchRegex regex
    let end = start + T.length matched
    return [Token start end (scopeToStyle patternScope) matched]
  
  Begin beginRegex endRegex -> do
    -- Match begin pattern
    (start, beginText) <- matchRegex beginRegex
    -- Match content until end pattern
    content <- manyTill anySingle (try $ void $ matchRegex endRegex)
    (endPos, endText) <- matchRegex endRegex
    let fullText = beginText <> T.pack content <> endText
    return [Token start (endPos + T.length endText) (scopeToStyle patternScope) fullText]
  
  Patterns patterns -> do
    -- Try nested patterns
    concat <$> many (choice $ map (try . patternToParser repo) patterns)
  
  Include ref -> case M.lookup ref repo of
    Just pattern -> patternToParser repo pattern
    Nothing -> empty

-- | Match regex and return position and matched text
matchRegex :: Text -> Parser (Int, Text)
matchRegex pattern = do
  pos <- getOffset
  -- Simplified regex matching
  text <- takeWhileP Nothing (const True)  -- Would use actual regex
  if T.pack (show pattern) `T.isInfixOf` text
    then return (pos, text)
    else empty

-- | Convert scope to token style
scopeToStyle :: Scope -> TokenStyle
scopeToStyle = \case
  ScopeKeyword -> Keyword
  ScopeString -> String
  ScopeComment -> Comment
  ScopeFunction -> Function
  ScopeType -> Type
  ScopeNumber -> Number
  ScopeOperator -> Operator
  ScopeConstant -> Normal  -- Map to closest style
  ScopeVariable -> Normal
  ScopeCustom _ -> Normal

-- | Compile pattern to efficient parser
compilePattern :: Pattern -> Parser [Token]
compilePattern pattern = patternToParser M.empty pattern

-- ===== Example Grammars =====

-- | Example: Custom configuration format
exampleConfigGrammar :: Grammar
exampleConfigGrammar = Grammar
  { grammarName = "MyConfig"
  , grammarFileTypes = [".myconf", ".config"]
  , grammarPatterns = 
      [ Pattern (Just "comment") (Match "#.*$") ScopeComment Nothing
      , Pattern (Just "section") (Match "^\\[.*\\]$") ScopeKeyword Nothing
      , Pattern (Just "key") (Match "^\\w+(?=\\s*=)") ScopeVariable Nothing
      , Pattern (Just "string") (Begin "\"" "\"") ScopeString Nothing
      , Pattern (Just "number") (Match "\\b\\d+(\\.\\d+)?\\b") ScopeNumber Nothing
      , Pattern (Just "boolean") (Match "\\b(true|false|yes|no)\\b") ScopeConstant Nothing
      , Pattern (Just "operator") (Match "[=:]") ScopeOperator Nothing
      ]
  , grammarRepository = M.fromList
      [ ("string_escape", Pattern Nothing (Match "\\\\[\"\\\\nrt]") ScopeConstant Nothing)
      ]
  , grammarInjections = []
  , grammarVariables = M.fromList
      [ ("identifier", "\\w+")
      , ("whitespace", "\\s+")
      ]
  }

-- | Example: Markdown grammar
exampleMarkdownGrammar :: Grammar
exampleMarkdownGrammar = Grammar
  { grammarName = "Markdown"
  , grammarFileTypes = [".md", ".markdown"]
  , grammarPatterns =
      [ Pattern (Just "heading") (Match "^#{1,6}\\s+.*$") ScopeKeyword Nothing
      , Pattern (Just "bold") (Begin "\\*\\*|__" "\\*\\*|__") ScopeType Nothing
      , Pattern (Just "italic") (Begin "\\*|_" "\\*|_") ScopeVariable Nothing
      , Pattern (Just "code_inline") (Begin "`" "`") ScopeString Nothing
      , Pattern (Just "code_block") (Begin "```" "```") ScopeString Nothing
      , Pattern (Just "link") (Match "\\[.*?\\]\\(.*?\\)") ScopeFunction Nothing
      , Pattern (Just "list_item") (Match "^\\s*[-*+]\\s+") ScopeOperator Nothing
      , Pattern (Just "blockquote") (Match "^>\\s+.*$") ScopeComment Nothing
      ]
  , grammarRepository = M.empty
  , grammarInjections = 
      [ Injection "code_block" "auto"  -- Auto-detect language in code blocks
      ]
  , grammarVariables = M.empty
  }

-- | Example: INI file grammar
exampleINIGrammar :: Grammar
exampleINIGrammar = Grammar
  { grammarName = "INI"
  , grammarFileTypes = [".ini", ".cfg"]
  , grammarPatterns =
      [ Pattern (Just "comment") (Match "[;#].*$") ScopeComment Nothing
      , Pattern (Just "section") (Match "^\\s*\\[([^\\]]+)\\]") ScopeKeyword Nothing
      , Pattern (Just "property") 
          (Patterns
            [ Pattern (Just "key") (Match "^([^=]+)(?==)") ScopeVariable Nothing
            , Pattern (Just "equals") (Match "=") ScopeOperator Nothing
            , Pattern (Just "value") (Match "[^\\n]+$") ScopeString Nothing
            ])
          ScopeCustom "property" Nothing
      ]
  , grammarRepository = M.empty
  , grammarInjections = []
  , grammarVariables = M.empty
  }

-- | Example YAML configuration for a grammar
exampleGrammarYAML :: Text
exampleGrammarYAML = T.unlines
  [ "name: MyLanguage"
  , "fileTypes: [.mylang]"
  , "patterns:"
  , "  - name: comment"
  , "    match: '//.*$'"
  , "    scope: comment"
  , "  - name: string"
  , "    begin: '\"'"
  , "    end: '\"'"
  , "    scope: string"
  , "    patterns:"
  , "      - include: '#string_escape'"
  , "  - name: keyword"
  , "    match: '\\b(if|else|while|for|return)\\b'"
  , "    scope: keyword"
  , "repository:"
  , "  string_escape:"
  , "    match: '\\\\[\"\\\\nrt]'"
  , "    scope: constant"
  , "variables:"
  , "  identifier: '[a-zA-Z_]\\w*'"
  ]

-- | Parse text using grammar
parseWithGrammar :: Grammar -> Text -> Either (ParseErrorBundle Text Void) [Token]
parseWithGrammar grammar = parse (grammarToParser grammar) "input"

-- | Example usage
exampleUsage :: IO ()
exampleUsage = do
  -- Parse config file
  let configText = T.unlines
        [ "# This is a comment"
        , "[section1]"
        , "key1 = \"value1\""
        , "key2 = 42"
        , "enabled = true"
        ]
  
  case parseWithGrammar exampleConfigGrammar configText of
    Left err -> putStrLn $ "Parse error: " ++ errorBundlePretty err
    Right tokens -> do
      putStrLn "Parsed tokens:"
      mapM_ print tokens