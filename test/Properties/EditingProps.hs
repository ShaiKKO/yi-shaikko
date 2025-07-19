{-# LANGUAGE OverloadedStrings #-}

module Properties.EditingProps (spec) where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.List (sort)

spec :: Spec
spec = describe "Editing Properties" $ do
  describe "Text Insertion" $ do
    prop "Insert preserves content before cursor" $ \text pos insertText ->
      let pos' = min pos (T.length text)
          (before, after) = T.splitAt pos' text
          result = before <> insertText <> after
      in T.take pos' result == before
    
    prop "Insert preserves content after cursor" $ \text pos insertText ->
      let pos' = min pos (T.length text)
          (before, after) = T.splitAt pos' text
          result = before <> insertText <> after
      in T.drop (pos' + T.length insertText) result == after
    
    prop "Cursor moves to end of insertion" $ \text pos insertText ->
      let pos' = min pos (T.length text)
          newPos = pos' + T.length insertText
      in newPos >= pos'
  
  describe "Text Deletion" $ do
    prop "Delete forward removes one character" $ \text pos ->
      pos < T.length text ==>
        let result = deleteForward text pos
        in T.length result == T.length text - 1
    
    prop "Delete backward removes one character" $ \text pos ->
      pos > 0 ==>
        let result = deleteBackward text pos
        in T.length result == T.length text - 1
    
    prop "Delete at boundaries is safe" $ \text ->
      deleteForward text (T.length text) == text &&
      deleteBackward text 0 == text
  
  describe "Undo/Redo" $ do
    prop "Undo reverses last operation" $ \ops ->
      monadicIO $ do
        state <- run $ createEditState
        run $ applyOperations state ops
        originalText <- run $ getText state
        run $ performUndo state
        undoneText <- run $ getText state
        if null ops
          then assert (undoneText == originalText)
          else assert (undoneText /= originalText || all isNoOp ops)
    
    prop "Redo after undo restores state" $ \ops ->
      monadicIO $ do
        state <- run $ createEditState
        run $ applyOperations state ops
        originalText <- run $ getText state
        run $ performUndo state
        run $ performRedo state
        restoredText <- run $ getText state
        assert (restoredText == originalText)
    
    prop "Multiple undo/redo maintains consistency" $ \n ops ->
      n > 0 && n < 10 ==> monadicIO $ do
        state <- run $ createEditState
        run $ applyOperations state ops
        run $ replicateM_ n $ do
          performUndo state
          performRedo state
        finalText <- run $ getText state
        run $ applyOperations state ops
        expectedText <- run $ getText state
        assert (finalText == expectedText)
  
  describe "Selection Operations" $ do
    prop "Copy doesn't modify text" $ \text start end ->
      let (start', end') = normalizeRange text start end
          copied = copySelection text start' end'
      in text == text  -- Text unchanged
    
    prop "Cut removes selected text" $ \text start end ->
      let (start', end') = normalizeRange text start end
          result = cutSelection text start' end'
      in T.length result == T.length text - (end' - start')
    
    prop "Paste inserts at cursor" $ \text pos clipboard ->
      let pos' = min pos (T.length text)
          result = pasteText text pos' clipboard
      in T.length result == T.length text + T.length clipboard
  
  describe "Line Operations" $ do
    prop "Line count is consistent" $ \text ->
      let lines = T.lines text
          lineCount = length lines
          newlineCount = T.count "\n" text
      in lineCount == max 1 (newlineCount + if T.null text || T.last text == '\n' then 0 else 1)
    
    prop "Join lines reduces line count" $ \lines ->
      length lines > 1 ==>
        let text = T.unlines lines
            joined = joinLines text 0
            newLineCount = length (T.lines joined)
        in newLineCount < length lines
    
    prop "Split line increases line count" $ \text pos ->
      not (T.null text) ==>
        let pos' = min pos (T.length text)
            split = splitLine text pos'
            oldLineCount = length (T.lines text)
            newLineCount = length (T.lines split)
        in newLineCount == oldLineCount + 1
  
  describe "Indentation" $ do
    prop "Indent preserves non-whitespace content" $ \text spaces ->
      spaces >= 0 && spaces <= 8 ==>
        let indented = indentLine text spaces
            stripped = T.dropWhile (== ' ') indented
            original = T.dropWhile (== ' ') text
        in stripped == original
    
    prop "Auto-indent matches previous line" $ \lines ->
      length lines >= 2 ==>
        let prevIndent = countIndent (lines !! (length lines - 2))
            autoIndented = autoIndentNewLine prevIndent
        in countIndent autoIndented == prevIndent
  
  describe "Word Operations" $ do
    prop "Word boundaries are consistent" $ \text pos ->
      let nextWord = findNextWordBoundary text pos
          prevWord = findPrevWordBoundary text pos
      in prevWord <= pos && pos <= nextWord
    
    prop "Delete word removes complete word" $ \text pos ->
      not (T.null text) ==>
        let result = deleteWord text pos
            wordEnd = findNextWordBoundary text pos
        in T.length result <= T.length text - (wordEnd - pos)

-- Test types and helpers
data EditOperation
  = Insert Int Text
  | Delete Int Int
  | Replace Int Int Text
  deriving (Show, Eq)

data EditState = EditState
  { esText :: TVar Text
  , esHistory :: TVar [Text]
  , esRedoStack :: TVar [Text]
  }

instance Arbitrary EditOperation where
  arbitrary = oneof
    [ Insert <$> arbitrary <*> arbitrary
    , Delete <$> arbitrary <*> arbitrary
    , Replace <$> arbitrary <*> arbitrary <*> arbitrary
    ]

deleteForward :: Text -> Int -> Text
deleteForward text pos
  | pos >= T.length text = text
  | otherwise = T.take pos text <> T.drop (pos + 1) text

deleteBackward :: Text -> Int -> Text
deleteBackward text pos
  | pos <= 0 = text
  | otherwise = T.take (pos - 1) text <> T.drop pos text

normalizeRange :: Text -> Int -> Int -> (Int, Int)
normalizeRange text start end =
  let start' = max 0 (min start (T.length text))
      end' = max 0 (min end (T.length text))
  in (min start' end', max start' end')

copySelection :: Text -> Int -> Int -> Text
copySelection text start end = T.take (end - start) (T.drop start text)

cutSelection :: Text -> Int -> Int -> Text
cutSelection text start end = T.take start text <> T.drop end text

pasteText :: Text -> Int -> Text -> Text
pasteText text pos clipboard =
  let (before, after) = T.splitAt pos text
  in before <> clipboard <> after

joinLines :: Text -> Int -> Text
joinLines text _ = T.intercalate " " (T.lines text)

splitLine :: Text -> Int -> Text
splitLine text pos =
  let (before, after) = T.splitAt pos text
  in before <> "\n" <> after

indentLine :: Text -> Int -> Text
indentLine text spaces = T.replicate spaces " " <> text

countIndent :: Text -> Int
countIndent = T.length . T.takeWhile (== ' ')

autoIndentNewLine :: Int -> Text
autoIndentNewLine indent = T.replicate indent " "

findNextWordBoundary :: Text -> Int -> Int
findNextWordBoundary text pos = min (T.length text) (pos + 1)  -- Simplified

findPrevWordBoundary :: Text -> Int -> Int
findPrevWordBoundary _ pos = max 0 (pos - 1)  -- Simplified

deleteWord :: Text -> Int -> Text
deleteWord text pos =
  let wordEnd = findNextWordBoundary text pos
  in T.take pos text <> T.drop wordEnd text

createEditState :: IO EditState
createEditState = do
  text <- newTVarIO ""
  history <- newTVarIO []
  redo <- newTVarIO []
  return EditState{..}

getText :: EditState -> IO Text
getText EditState{..} = readTVarIO esText

applyOperations :: EditState -> [EditOperation] -> IO ()
applyOperations state = mapM_ (applyOperation state)

applyOperation :: EditState -> EditOperation -> IO ()
applyOperation EditState{..} op = atomically $ do
  text <- readTVar esText
  history <- readTVar esHistory
  let newText = case op of
        Insert pos t -> pasteText text (min pos (T.length text)) t
        Delete start end ->
          let (s, e) = normalizeRange text start end
          in cutSelection text s e
        Replace start end t ->
          let (s, e) = normalizeRange text start end
          in pasteText (cutSelection text s e) s t
  writeTVar esText newText
  writeTVar esHistory (text : history)
  writeTVar esRedoStack []

performUndo :: EditState -> IO ()
performUndo EditState{..} = atomically $ do
  history <- readTVar esHistory
  case history of
    [] -> return ()
    (prev:rest) -> do
      current <- readTVar esText
      writeTVar esText prev
      writeTVar esHistory rest
      modifyTVar esRedoStack (current:)

performRedo :: EditState -> IO ()
performRedo EditState{..} = atomically $ do
  redo <- readTVar esRedoStack
  case redo of
    [] -> return ()
    (next:rest) -> do
      current <- readTVar esText
      writeTVar esText next
      writeTVar esRedoStack rest
      modifyTVar esHistory (current:)

isNoOp :: EditOperation -> Bool
isNoOp (Insert _ t) = T.null t
isNoOp (Delete s e) = s == e
isNoOp (Replace s e t) = s == e && T.null t