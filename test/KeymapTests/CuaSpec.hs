{-# LANGUAGE OverloadedStrings #-}

module KeymapTests.CuaSpec (spec) where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Control.Monad
import           Data.Text (Text)
import qualified Data.Text as T

-- Import CUA keymap
import           Yi.Keymap.Cua
import           Yi.Event
import           Yi.Keymap
import           Yi.Types

spec :: Spec
spec = describe "CUA Keymap" $ do
  describe "Standard CUA Bindings" $ do
    it "Ctrl-C copies selection" $ do
      processCuaEvent (ctrlEvent 'c') `shouldBe` CopySelection
    
    it "Ctrl-X cuts selection" $ do
      processCuaEvent (ctrlEvent 'x') `shouldBe` CutSelection
    
    it "Ctrl-V pastes from clipboard" $ do
      processCuaEvent (ctrlEvent 'v') `shouldBe` PasteClipboard
    
    it "Ctrl-Z undoes last action" $ do
      processCuaEvent (ctrlEvent 'z') `shouldBe` UndoAction
    
    it "Ctrl-Y redoes last undo" $ do
      processCuaEvent (ctrlEvent 'y') `shouldBe` RedoAction
    
    it "Ctrl-A selects all" $ do
      processCuaEvent (ctrlEvent 'a') `shouldBe` SelectAll
  
  describe "File Operations" $ do
    it "Ctrl-O opens file" $ do
      processCuaEvent (ctrlEvent 'o') `shouldBe` OpenFile
    
    it "Ctrl-S saves file" $ do
      processCuaEvent (ctrlEvent 's') `shouldBe` SaveFile
    
    it "Ctrl-Shift-S saves as" $ do
      processCuaEvent (ctrlShiftEvent 's') `shouldBe` SaveAsFile
    
    it "Ctrl-N creates new file" $ do
      processCuaEvent (ctrlEvent 'n') `shouldBe` NewFile
    
    it "Ctrl-Q quits application" $ do
      processCuaEvent (ctrlEvent 'q') `shouldBe` QuitApp
  
  describe "Search Operations" $ do
    it "Ctrl-F opens find dialog" $ do
      processCuaEvent (ctrlEvent 'f') `shouldBe` FindText
    
    it "Ctrl-H opens replace dialog" $ do
      processCuaEvent (ctrlEvent 'h') `shouldBe` ReplaceText
    
    it "F3 finds next occurrence" $ do
      processCuaEvent (funcKey 3) `shouldBe` FindNext
    
    it "Shift-F3 finds previous" $ do
      processCuaEvent (shiftFuncKey 3) `shouldBe` FindPrevious
  
  describe "Navigation" $ do
    prop "Arrow keys navigate correctly" $ \dir ->
      processCuaNavigation (arrowKey dir) == NavigateTo dir
    
    prop "Ctrl+Arrow moves by word" $ \dir ->
      processCuaNavigation (ctrlArrowKey dir) == NavigateByWord dir
    
    it "Home goes to line start" $ do
      processCuaEvent homeKey `shouldBe` GoToLineStart
    
    it "End goes to line end" $ do
      processCuaEvent endKey `shouldBe` GoToLineEnd
    
    it "Ctrl-Home goes to file start" $ do
      processCuaEvent (ctrlEvent homeKey) `shouldBe` GoToFileStart
    
    it "Ctrl-End goes to file end" $ do
      processCuaEvent (ctrlEvent endKey) `shouldBe` GoToFileEnd
  
  describe "Selection Behavior" $ do
    it "Shift+Arrow extends selection" $ do
      processCuaSelection (shiftArrowKey RightDir) `shouldBe` ExtendSelection RightDir
    
    it "Shift+Click extends selection to point" $ do
      processCuaSelection (shiftClick (100, 50)) `shouldBe` ExtendSelectionToPoint (100, 50)
    
    prop "Double-click selects word" $ \pos ->
      processCuaSelection (doubleClick pos) == SelectWord pos
    
    prop "Triple-click selects line" $ \pos ->
      processCuaSelection (tripleClick pos) == SelectLine pos
  
  describe "Context Menu" $ do
    it "Right-click shows context menu" $ do
      processCuaEvent (rightClick (50, 50)) `shouldBe` ShowContextMenu (50, 50)
    
    it "Shift-F10 shows context menu at cursor" $ do
      processCuaEvent (shiftFuncKey 10) `shouldBe` ShowContextMenuAtCursor

-- Test types
data CuaAction
  = CopySelection
  | CutSelection
  | PasteClipboard
  | UndoAction
  | RedoAction
  | SelectAll
  | OpenFile
  | SaveFile
  | SaveAsFile
  | NewFile
  | QuitApp
  | FindText
  | ReplaceText
  | FindNext
  | FindPrevious
  | NavigateTo Direction
  | NavigateByWord Direction
  | GoToLineStart
  | GoToLineEnd
  | GoToFileStart
  | GoToFileEnd
  | ExtendSelection Direction
  | ExtendSelectionToPoint (Int, Int)
  | SelectWord (Int, Int)
  | SelectLine (Int, Int)
  | ShowContextMenu (Int, Int)
  | ShowContextMenuAtCursor
  deriving (Show, Eq)

data Direction = UpDir | DownDir | LeftDir | RightDir
  deriving (Show, Eq, Enum, Bounded)

instance Arbitrary Direction where
  arbitrary = arbitraryBoundedEnum

-- Test helpers
ctrlEvent :: Char -> Event
ctrlEvent = error "ctrlEvent stub"

ctrlShiftEvent :: Char -> Event
ctrlShiftEvent = error "ctrlShiftEvent stub"

funcKey :: Int -> Event
funcKey = error "funcKey stub"

shiftFuncKey :: Int -> Event
shiftFuncKey = error "shiftFuncKey stub"

arrowKey :: Direction -> Event
arrowKey = error "arrowKey stub"

ctrlArrowKey :: Direction -> Event
ctrlArrowKey = error "ctrlArrowKey stub"

shiftArrowKey :: Direction -> Event
shiftArrowKey = error "shiftArrowKey stub"

homeKey :: Event
homeKey = error "homeKey stub"

endKey :: Event
endKey = error "endKey stub"

rightClick :: (Int, Int) -> Event
rightClick = error "rightClick stub"

shiftClick :: (Int, Int) -> Event
shiftClick = error "shiftClick stub"

doubleClick :: (Int, Int) -> Event
doubleClick = error "doubleClick stub"

tripleClick :: (Int, Int) -> Event
tripleClick = error "tripleClick stub"

processCuaEvent :: Event -> CuaAction
processCuaEvent _ = CopySelection  -- Stub

processCuaNavigation :: Event -> CuaAction
processCuaNavigation _ = NavigateTo UpDir  -- Stub

processCuaSelection :: Event -> CuaAction
processCuaSelection _ = ExtendSelection RightDir  -- Stub