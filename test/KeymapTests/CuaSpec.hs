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
import           Yi.Keymap.Keys
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
  | NoAction
  deriving (Show, Eq)

data Direction = UpDir | DownDir | LeftDir | RightDir | HomePos | EndPos
  deriving (Show, Eq, Enum, Bounded)

instance Arbitrary Direction where
  arbitrary = arbitraryBoundedEnum

-- Test helpers
ctrlEvent :: Char -> Event
ctrlEvent c = Event (KASCII c) [MCtrl]

ctrlShiftEvent :: Char -> Event
ctrlShiftEvent c = Event (KASCII c) [MCtrl, MShift]

funcKey :: Int -> Event
funcKey n = Event (KFun n) []

shiftFuncKey :: Int -> Event
shiftFuncKey n = Event (KFun n) [MShift]

arrowKey :: Direction -> Event
arrowKey dir = case dir of
  UpDir -> Event KUp []
  DownDir -> Event KDown []
  LeftDir -> Event KLeft []
  RightDir -> Event KRight []

ctrlArrowKey :: Direction -> Event
ctrlArrowKey dir = case dir of
  UpDir -> Event KUp [MCtrl]
  DownDir -> Event KDown [MCtrl]
  LeftDir -> Event KLeft [MCtrl]
  RightDir -> Event KRight [MCtrl]

shiftArrowKey :: Direction -> Event
shiftArrowKey dir = case dir of
  UpDir -> Event KUp [MShift]
  DownDir -> Event KDown [MShift]
  LeftDir -> Event KLeft [MShift]
  RightDir -> Event KRight [MShift]

homeKey :: Event
homeKey = Event KHome []

endKey :: Event
endKey = Event KEnd []

rightClick :: (Int, Int) -> Event
rightClick (x, y) = Event (KMouse 2 x y) []  -- Button 2 is right button

shiftClick :: (Int, Int) -> Event
shiftClick (x, y) = Event (KMouse 1 x y) [MShift]  -- Button 1 with Shift

doubleClick :: (Int, Int) -> Event
doubleClick (x, y) = Event (KMouse 1 x y) []  -- Double click is handled differently

tripleClick :: (Int, Int) -> Event
tripleClick (x, y) = Event (KMouse 1 x y) []  -- Triple click is handled differently

processCuaEvent :: Event -> CuaAction
processCuaEvent (Event (KASCII 'x') [MCtrl]) = CutSelection
processCuaEvent (Event (KASCII 'c') [MCtrl]) = CopySelection
processCuaEvent (Event (KASCII 'v') [MCtrl]) = PasteClipboard
processCuaEvent (Event (KASCII 'z') [MCtrl]) = UndoAction
processCuaEvent (Event (KASCII 'y') [MCtrl]) = RedoAction
processCuaEvent (Event (KASCII 'a') [MCtrl]) = SelectAll
processCuaEvent (Event (KASCII 's') [MCtrl]) = SaveFile
processCuaEvent (Event (KASCII 'o') [MCtrl]) = OpenFile
processCuaEvent _ = NoAction

processCuaNavigation :: Event -> CuaAction
processCuaNavigation (Event KUp _) = NavigateTo UpDir
processCuaNavigation (Event KDown _) = NavigateTo DownDir
processCuaNavigation (Event KLeft _) = NavigateTo LeftDir
processCuaNavigation (Event KRight _) = NavigateTo RightDir
processCuaNavigation (Event KHome _) = NavigateTo HomePos
processCuaNavigation (Event KEnd _) = NavigateTo EndPos
processCuaNavigation _ = NoAction

processCuaSelection :: Event -> CuaAction
processCuaSelection (Event KUp [MShift]) = ExtendSelection UpDir
processCuaSelection (Event KDown [MShift]) = ExtendSelection DownDir
processCuaSelection (Event KLeft [MShift]) = ExtendSelection LeftDir
processCuaSelection (Event KRight [MShift]) = ExtendSelection RightDir
processCuaSelection _ = NoAction