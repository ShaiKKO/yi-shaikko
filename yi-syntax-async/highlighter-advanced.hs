{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      :  Yi.Syntax.Async.Advanced
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Advanced syntax highlighting features including rainbow parentheses,
-- semantic highlighting, and visual indicators.

module Yi.Syntax.Async.Advanced
  ( -- * Rainbow Parentheses
    RainbowConfig(..)
  , ParenInfo(..)
  , computeRainbowParens
  , highlightMatchingParen
  , visualizeParenDepth
    -- * Semantic Highlighting
  , SemanticLayer(..)
  , SemanticInfo(..)
  , computeSemanticHighlights
  , addVariableWarnings
  , addTypeErrors
    -- * Visual Features
  , ComplexityMetric(..)
  , computeComplexity
  , addGitDiffIndicators
  , visualizeSyntaxTree
    -- * Integration
  , applyAdvancedHighlighting
  ) where

import           Control.Monad
import           Control.Monad.State
import           Data.Char (isSpace)
import           Data.Either
import           Data.Foldable (foldl')
import           Data.List (find, nub, partition, sortOn)
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Tree as Tree
import qualified Data.Vector as V
import           GHC.Generics

-- Import from highlighter-core
import Yi.Syntax.Async.Core (Token(..), TokenStyle(..))

-- | Rainbow parentheses configuration
data RainbowConfig = RainbowConfig
  { rainbowColors     :: [TokenStyle]   -- ^ Colors to cycle through
  , rainbowBrackets   :: [(Char, Char)] -- ^ Bracket pairs to colorize
  , rainbowHighlight  :: Bool           -- ^ Highlight matching bracket
  , rainbowMaxDepth   :: Int            -- ^ Maximum depth to track
  } deriving (Show, Eq, Generic)

defaultRainbowConfig :: RainbowConfig
defaultRainbowConfig = RainbowConfig
  { rainbowColors = 
      [ Normal     -- depth 0 (white)
      , Type       -- depth 1 (yellow) 
      , String     -- depth 2 (cyan)
      , Function   -- depth 3 (magenta)
      , Keyword    -- depth 4 (green)
      , Number     -- depth 5 (blue)
      , Comment    -- depth 6 (red)
      , Operator   -- depth 7 (orange)
      ]
  , rainbowBrackets = 
      [ ('(', ')')
      , ('[', ']')
      , ('{', '}')
      , ('⟨', '⟩')  -- Unicode brackets
      , ('<', '>')  -- Angle brackets (for templates)
      ]
  , rainbowHighlight = True
  , rainbowMaxDepth = 8
  }

-- | Information about a parenthesis
data ParenInfo = ParenInfo
  { parenChar     :: Char
  , parenPos      :: Int
  , parenDepth    :: Int
  , parenMatching :: Maybe Int  -- ^ Position of matching paren
  , parenType     :: ParenType
  } deriving (Show, Eq, Generic)

data ParenType = Opening | Closing
  deriving (Show, Eq, Generic)

-- | Compute rainbow parentheses coloring
computeRainbowParens :: RainbowConfig -> Text -> V.Vector Token -> V.Vector Token
computeRainbowParens config sourceText tokens = 
  let parenInfos = findAllParens config sourceText
      coloredParens = assignParenColors config parenInfos
      parenMap = M.fromList [(parenPos p, p) | p <- coloredParens]
  in V.map (colorizeToken parenMap) tokens

-- | Find all parentheses in text
findAllParens :: RainbowConfig -> Text -> [ParenInfo]
findAllParens RainbowConfig{..} text = 
  let bracketChars = S.fromList $ concatMap (\(o,c) -> [o,c]) rainbowBrackets
      isOpenBracket c = any ((== c) . fst) rainbowBrackets
      isCloseBracket c = any ((== c) . snd) rainbowBrackets
      matchingBracket c = 
        case find (\(o,_) -> o == c) rainbowBrackets of
          Just (_, close) -> Just close
          Nothing -> case find (\(_,cl) -> cl == c) rainbowBrackets of
            Just (open, _) -> Just open
            Nothing -> Nothing
  in evalState (findParens 0) (ParenState [] M.empty)
  where
    findParens :: Int -> State ParenState [ParenInfo]
    findParens pos
      | pos >= T.length text = do
          -- Return all unmatched parens
          ParenState stack _ <- get
          return $ map snd stack
      | otherwise = do
          let char = T.index text pos
          if isOpenBracket char
            then do
              -- Push opening bracket
              let paren = ParenInfo char pos 0 Nothing Opening
              modify $ \s -> s { psStack = (char, paren) : psStack s }
              rest <- findParens (pos + 1)
              return $ paren : rest
            else if isCloseBracket char
              then do
                -- Try to match closing bracket
                ParenState stack matches <- get
                case findMatchingOpen char stack of
                  Just ((openChar, openParen), newStack) -> do
                    -- Found matching bracket
                    let depth = length newStack
                        closeParen = ParenInfo char pos depth (Just (parenPos openParen)) Closing
                        updatedOpen = openParen { parenDepth = depth, parenMatching = Just pos }
                    put $ ParenState newStack (M.insert (parenPos openParen) updatedOpen matches)
                    rest <- findParens (pos + 1)
                    return $ closeParen : updatedOpen : rest
                  Nothing -> do
                    -- Unmatched closing bracket
                    let paren = ParenInfo char pos 0 Nothing Closing
                    rest <- findParens (pos + 1)
                    return $ paren : rest
              else findParens (pos + 1)
    
    isOpenBracket c = any ((== c) . fst) rainbowBrackets
    isCloseBracket c = any ((== c) . snd) rainbowBrackets
    
    findMatchingOpen :: Char -> [(Char, ParenInfo)] -> Maybe ((Char, ParenInfo), [(Char, ParenInfo)])
    findMatchingOpen closeChar = go []
      where
        go _ [] = Nothing
        go acc ((openChar, info):rest) =
          case find (\(o,c) -> o == openChar && c == closeChar) rainbowBrackets of
            Just _ -> Just ((openChar, info), reverse acc ++ rest)
            Nothing -> go ((openChar, info):acc) rest

-- | State for paren matching
data ParenState = ParenState
  { psStack   :: [(Char, ParenInfo)]     -- ^ Stack of unmatched opens
  , psMatches :: M.Map Int ParenInfo      -- ^ Completed matches
  } deriving (Show, Eq)

-- | Assign colors to parentheses
assignParenColors :: RainbowConfig -> [ParenInfo] -> [ParenInfo]
assignParenColors RainbowConfig{..} parens =
  map assignColor parens
  where
    assignColor p@ParenInfo{..} =
      let colorIndex = parenDepth `mod` length rainbowColors
          color = rainbowColors !! colorIndex
      in p

-- | Apply coloring to tokens
colorizeToken :: M.Map Int ParenInfo -> Token -> Token
colorizeToken parenMap token@Token{..} =
  case M.lookup tokenStart parenMap of
    Just ParenInfo{..} ->
      let colorIndex = parenDepth `mod` 8
          rainbowStyle = case colorIndex of
            0 -> Normal
            1 -> Type
            2 -> String
            3 -> Function
            4 -> Keyword
            5 -> Number
            6 -> Comment
            7 -> Operator
            _ -> Normal
      in token { tokenStyle = rainbowStyle }
    Nothing -> token

-- | Highlight matching parenthesis
highlightMatchingParen :: Int -> [ParenInfo] -> Maybe (Int, Int)
highlightMatchingParen cursorPos parens =
  case find (\p -> parenPos p == cursorPos || parenPos p == cursorPos - 1) parens of
    Just ParenInfo{..} -> case parenMatching of
      Just matchPos -> Just (parenPos, matchPos)
      Nothing -> Nothing
    Nothing -> Nothing

-- | Visualize parenthesis depth as text diagram
visualizeParenDepth :: Text -> [ParenInfo] -> Text
visualizeParenDepth sourceText parens = T.unlines
  [ visualizeLine lineNum line
  | (lineNum, line) <- zip [1..] (T.lines sourceText)
  ]
  where
    parensByLine = groupByLine parens
    
    visualizeLine :: Int -> Text -> Text
    visualizeLine lineNum line =
      let lineParens = M.findWithDefault [] lineNum parensByLine
          depthMarkers = [makeDepthMarker p | p <- lineParens]
          maxDepth = if null lineParens then 0 else maximum [parenDepth p | p <- lineParens]
      in T.pack $ show lineNum ++ ": " ++ T.unpack line ++ 
         "\n   " ++ replicate (T.length line) ' ' ++ 
         "\n   " ++ makeDepthLine lineParens (T.length line)
    
    makeDepthLine :: [ParenInfo] -> Int -> String
    makeDepthLine parens len = 
      [depthChar $ find (\p -> parenPos p == i) parens | i <- [0..len-1]]
    
    depthChar :: Maybe ParenInfo -> Char
    depthChar Nothing = ' '
    depthChar (Just p) = head $ show $ parenDepth p
    
    groupByLine :: [ParenInfo] -> M.Map Int [ParenInfo]
    groupByLine = M.fromListWith (++) . map (\p -> (getLine (parenPos p), [p]))
    
    getLine pos = pos `div` 80  -- Assume 80 chars per line
    
    makeDepthMarker :: ParenInfo -> String
    makeDepthMarker ParenInfo{..} = replicate parenDepth '│'

-- ===== Semantic Highlighting =====

-- | Semantic layer for additional highlighting
data SemanticLayer = SemanticLayer
  { semVariables    :: M.Map Text VariableInfo
  , semTypes        :: M.Map Text TypeInfo
  , semErrors       :: [ErrorInfo]
  , semWarnings     :: [WarningInfo]
  , semComplexity   :: M.Map Int ComplexityMetric
  } deriving (Show, Eq, Generic)

-- | Variable information
data VariableInfo = VariableInfo
  { varName        :: Text
  , varPos         :: Int
  , varType        :: Maybe Text
  , varUsages      :: [Int]
  , varShadows     :: Maybe Text
  , varUnused      :: Bool
  } deriving (Show, Eq, Generic)

-- | Type information
data TypeInfo = TypeInfo
  { typeName       :: Text
  , typePos        :: Int
  , typeKind       :: TypeKind
  , typeMembers    :: [Text]
  } deriving (Show, Eq, Generic)

data TypeKind = Class | Interface | Enum | Alias
  deriving (Show, Eq, Generic)

-- | Error information
data ErrorInfo = ErrorInfo
  { errPos         :: Int
  , errEndPos      :: Int
  , errMessage     :: Text
  , errSeverity    :: ErrorSeverity
  } deriving (Show, Eq, Generic)

data ErrorSeverity = SevError | SevWarning | SevInfo | SevHint
  deriving (Show, Eq, Generic)

-- | Warning information  
data WarningInfo = WarningInfo
  { warnPos        :: Int
  , warnEndPos     :: Int
  , warnMessage    :: Text
  , warnType       :: WarningType
  } deriving (Show, Eq, Generic)

data WarningType = UnusedVar | ShadowedVar | Deprecated | Complexity
  deriving (Show, Eq, Generic)

-- | Semantic information for a token
data SemanticInfo = SemanticInfo
  { semToken       :: Token
  , semSymbol      :: Maybe SymbolInfo
  , semReferences  :: [Int]
  , semDefinition  :: Maybe Int
  } deriving (Show, Eq, Generic)

data SymbolInfo = SymbolInfo
  { symName        :: Text
  , symKind        :: SymbolKind
  , symScope       :: Text
  } deriving (Show, Eq, Generic)

data SymbolKind = SymVar | SymFunc | SymClass | SymMethod
  deriving (Show, Eq, Generic)

-- | Compute semantic highlights
computeSemanticHighlights :: V.Vector Token -> Text -> SemanticLayer
computeSemanticHighlights tokens sourceText =
  let variables = findVariables tokens sourceText
      types = findTypes tokens sourceText
      errors = []  -- Would come from LSP or compiler
      warnings = computeWarnings variables
      complexity = computeComplexityMap tokens sourceText
  in SemanticLayer variables types errors warnings complexity

-- | Find variable declarations and usages
findVariables :: V.Vector Token -> Text -> M.Map Text VariableInfo
findVariables tokens sourceText = 
  let varTokens = V.filter isVarToken tokens
      varNames = nub [tokenText t | t <- V.toList varTokens]
      varInfos = [analyzeVariable name tokens | name <- varNames]
  in M.fromList [(varName v, v) | v <- varInfos]
  where
    isVarToken Token{..} = tokenStyle == Normal && isIdentifier tokenText
    isIdentifier t = T.all (\c -> isAlphaNum c || c == '_') t

-- | Analyze a variable
analyzeVariable :: Text -> V.Vector Token -> VariableInfo
analyzeVariable name tokens =
  let usages = [tokenStart t | t <- V.toList tokens, tokenText t == name]
      firstUse = if null usages then 0 else head usages
      isUnused = length usages <= 1
  in VariableInfo
      { varName = name
      , varPos = firstUse
      , varType = Nothing  -- Would infer from context
      , varUsages = usages
      , varShadows = Nothing  -- Would check scopes
      , varUnused = isUnused
      }

-- | Find type definitions
findTypes :: V.Vector Token -> Text -> M.Map Text TypeInfo
findTypes tokens sourceText =
  let typeTokens = V.filter isTypeToken tokens
      typeNames = nub [tokenText t | t <- V.toList typeTokens]
  in M.fromList [(typeName, makeTypeInfo typeName) | typeName <- typeNames]
  where
    isTypeToken Token{..} = tokenStyle == Type
    makeTypeInfo name = TypeInfo name 0 Class []

-- | Add variable warnings
addVariableWarnings :: SemanticLayer -> V.Vector Token -> V.Vector Token
addVariableWarnings SemanticLayer{..} tokens =
  V.map addWarning tokens
  where
    addWarning token@Token{..} =
      case M.lookup tokenText semVariables of
        Just VarInfo{..} | varUnused ->
          token { tokenStyle = Comment }  -- Dim unused variables
        _ -> token

-- | Add type errors from LSP
addTypeErrors :: [ErrorInfo] -> V.Vector Token -> V.Vector Token
addTypeErrors errors tokens =
  V.map (addError errorMap) tokens
  where
    errorMap = M.fromList [(errPos e, e) | e <- errors]
    
    addError errMap token@Token{..} =
      case M.lookup tokenStart errMap of
        Just _ -> token { tokenStyle = Error }
        Nothing -> token

-- | Compute warnings
computeWarnings :: M.Map Text VariableInfo -> [WarningInfo]
computeWarnings variables =
  concat
    [ unusedWarnings
    , shadowWarnings
    ]
  where
    unusedWarnings = 
      [ WarningInfo (varPos v) (varPos v + T.length (varName v)) 
          ("Unused variable: " <> varName v) UnusedVar
      | v <- M.elems variables, varUnused v
      ]
    
    shadowWarnings =
      [ WarningInfo (varPos v) (varPos v + T.length (varName v))
          ("Variable shadows: " <> fromMaybe "" (varShadows v)) ShadowedVar
      | v <- M.elems variables, isJust (varShadows v)
      ]

-- ===== Complexity Metrics =====

-- | Code complexity metric
data ComplexityMetric = ComplexityMetric
  { cmCyclomatic   :: Int     -- ^ Cyclomatic complexity
  , cmNesting      :: Int     -- ^ Maximum nesting depth
  , cmLineCount    :: Int     -- ^ Lines of code
  , cmTokenCount   :: Int     -- ^ Number of tokens
  } deriving (Show, Eq, Generic)

-- | Compute complexity for regions
computeComplexity :: V.Vector Token -> Text -> ComplexityMetric
computeComplexity tokens sourceText =
  ComplexityMetric
    { cmCyclomatic = computeCyclomatic tokens
    , cmNesting = computeMaxNesting tokens
    , cmLineCount = length $ T.lines sourceText
    , cmTokenCount = V.length tokens
    }

-- | Compute cyclomatic complexity
computeCyclomatic :: V.Vector Token -> Int
computeCyclomatic tokens =
  let decisionPoints = V.filter isDecisionPoint tokens
  in V.length decisionPoints + 1
  where
    isDecisionPoint Token{..} =
      tokenStyle == Keyword && 
      tokenText `elem` ["if", "else", "while", "for", "case", "catch"]

-- | Compute maximum nesting depth
computeMaxNesting :: V.Vector Token -> Int
computeMaxNesting tokens =
  let depths = scanl updateDepth 0 (V.toList tokens)
  in maximum depths
  where
    updateDepth depth Token{..} =
      case T.unpack tokenText of
        "{" -> depth + 1
        "}" -> max 0 (depth - 1)
        _ -> depth

-- | Compute complexity map by function
computeComplexityMap :: V.Vector Token -> Text -> M.Map Int ComplexityMetric
computeComplexityMap tokens sourceText =
  let functions = findFunctions tokens
  in M.fromList [(funcStart f, computeFunctionComplexity f tokens) | f <- functions]
  where
    findFunctions = const []  -- Would parse function boundaries
    computeFunctionComplexity _ toks = computeComplexity toks sourceText

-- ===== Git Integration =====

-- | Add git diff indicators
addGitDiffIndicators :: FilePath -> V.Vector Token -> IO (V.Vector Token)
addGitDiffIndicators filepath tokens = do
  -- Would run git diff and parse output
  let gitChanges = []  -- Placeholder
  return $ V.map (addGitInfo gitChanges) tokens
  where
    addGitInfo changes token = token  -- Would modify based on git info

-- ===== Visualization =====

-- | Visualize syntax tree as ASCII art
visualizeSyntaxTree :: V.Vector Token -> Text -> Text
visualizeSyntaxTree tokens sourceText =
  let tree = buildSyntaxTree tokens
  in T.pack $ Tree.drawTree $ fmap show tree

-- | Build syntax tree from tokens
buildSyntaxTree :: V.Vector Token -> Tree.Tree TokenInfo
buildSyntaxTree tokens = 
  Tree.Node (TokenInfo "root" 0 0) (buildChildren 0 (V.toList tokens))
  where
    buildChildren _ [] = []
    buildChildren depth (t:ts) =
      case tokenStyle t of
        Keyword -> Tree.Node (tokenToInfo t) (buildChildren (depth+1) ts) : []
        _ -> buildChildren depth ts
    
    tokenToInfo Token{..} = TokenInfo tokenText tokenStart tokenEnd

data TokenInfo = TokenInfo Text Int Int
  deriving (Show, Eq)

-- ===== Integration =====

-- | Apply all advanced highlighting features
applyAdvancedHighlighting :: RainbowConfig -> V.Vector Token -> Text 
                         -> IO (V.Vector Token, Text)
applyAdvancedHighlighting rainbowConfig tokens sourceText = do
  -- Apply rainbow parentheses
  let rainbowTokens = computeRainbowParens rainbowConfig sourceText tokens
  
  -- Compute semantic layer
  let semanticLayer = computeSemanticHighlights tokens sourceText
  
  -- Add warnings and errors
  let warningTokens = addVariableWarnings semanticLayer rainbowTokens
      errorTokens = addTypeErrors (semErrors semanticLayer) warningTokens
  
  -- Generate visualization
  let visualization = T.unlines
        [ "=== Syntax Visualization ==="
        , visualizeParenDepth sourceText (findAllParens rainbowConfig sourceText)
        , ""
        , "=== Complexity Metrics ==="
        , T.pack $ show $ computeComplexity tokens sourceText
        , ""
        , "=== Syntax Tree ==="
        , visualizeSyntaxTree tokens sourceText
        ]
  
  return (errorTokens, visualization)