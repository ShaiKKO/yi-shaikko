{-# LANGUAGE OverloadedStrings #-}

module KeymapTests.MgSpec (spec) where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Control.Monad
import           Data.Text (Text)
import qualified Data.Text as T

-- Import Mg keymap
import           Yi.Keymap.Mg
import           Yi.Event
import           Yi.Keymap
import           Yi.Types

spec :: Spec
spec = describe "Mg Keymap" $ do
  describe "Basic Movement" $ do
    it "Ctrl-F moves forward char" $ do
      processMgEvent (ctrlEvent 'f') `shouldBe` ForwardChar
    
    it "Ctrl-B moves backward char" $ do
      processMgEvent (ctrlEvent 'b') `shouldBe` BackwardChar
    
    it "Ctrl-N moves next line" $ do
      processMgEvent (ctrlEvent 'n') `shouldBe` NextLine
    
    it "Ctrl-P moves previous line" $ do
      processMgEvent (ctrlEvent 'p') `shouldBe` PreviousLine
    
    it "Ctrl-A moves to line beginning" $ do
      processMgEvent (ctrlEvent 'a') `shouldBe` BeginningOfLine
    
    it "Ctrl-E moves to line end" $ do
      processMgEvent (ctrlEvent 'e') `shouldBe` EndOfLine
  
  describe "Word Movement" $ do
    it "Alt-F moves forward word" $ do
      processMgEvent (altEvent 'f') `shouldBe` ForwardWord
    
    it "Alt-B moves backward word" $ do
      processMgEvent (altEvent 'b') `shouldBe` BackwardWord
  
  describe "Buffer Navigation" $ do
    it "Alt-< goes to buffer beginning" $ do
      processMgEvent (altEvent '<') `shouldBe` BeginningOfBuffer
    
    it "Alt-> goes to buffer end" $ do
      processMgEvent (altEvent '>') `shouldBe` EndOfBuffer
    
    it "Ctrl-V scrolls down" $ do
      processMgEvent (ctrlEvent 'v') `shouldBe` ScrollDown
    
    it "Alt-V scrolls up" $ do
      processMgEvent (altEvent 'v') `shouldBe` ScrollUp
  
  describe "Deletion" $ do
    it "Ctrl-D deletes forward char" $ do
      processMgEvent (ctrlEvent 'd') `shouldBe` DeleteChar
    
    it "Backspace deletes backward char" $ do
      processMgEvent backspaceKey `shouldBe` BackwardDeleteChar
    
    it "Alt-D deletes forward word" $ do
      processMgEvent (altEvent 'd') `shouldBe` KillWord
    
    it "Alt-Backspace deletes backward word" $ do
      processMgEvent (altEvent backspaceKey) `shouldBe` BackwardKillWord
    
    it "Ctrl-K kills to end of line" $ do
      processMgEvent (ctrlEvent 'k') `shouldBe` KillLine
  
  describe "Mark and Region" $ do
    it "Ctrl-Space sets mark" $ do
      processMgEvent (ctrlEvent ' ') `shouldBe` SetMark
    
    it "Ctrl-X Ctrl-X exchanges point and mark" $ do
      processMgSequence [ctrlX, ctrlEvent 'x'] `shouldBe` ExchangePointMark
    
    it "Ctrl-W kills region" $ do
      processMgEvent (ctrlEvent 'w') `shouldBe` KillRegion
    
    it "Alt-W copies region" $ do
      processMgEvent (altEvent 'w') `shouldBe` CopyRegion
  
  describe "File Operations" $ do
    it "Ctrl-X Ctrl-F finds file" $ do
      processMgSequence [ctrlX, ctrlEvent 'f'] `shouldBe` FindFile
    
    it "Ctrl-X Ctrl-S saves buffer" $ do
      processMgSequence [ctrlX, ctrlEvent 's'] `shouldBe` SaveBuffer
    
    it "Ctrl-X Ctrl-W writes file" $ do
      processMgSequence [ctrlX, ctrlEvent 'w'] `shouldBe` WriteFile
    
    it "Ctrl-X Ctrl-C exits Mg" $ do
      processMgSequence [ctrlX, ctrlEvent 'c'] `shouldBe` ExitMg
  
  describe "Buffer Management" $ do
    it "Ctrl-X b switches buffer" $ do
      processMgSequence [ctrlX, charEvent 'b'] `shouldBe` SwitchBuffer
    
    it "Ctrl-X k kills buffer" $ do
      processMgSequence [ctrlX, charEvent 'k'] `shouldBe` KillBuffer
    
    it "Ctrl-X Ctrl-B lists buffers" $ do
      processMgSequence [ctrlX, ctrlEvent 'b'] `shouldBe` ListBuffers
  
  describe "Search" $ do
    it "Ctrl-S searches forward" $ do
      processMgEvent (ctrlEvent 's') `shouldBe` IncrementalSearch
    
    it "Ctrl-R searches backward" $ do
      processMgEvent (ctrlEvent 'r') `shouldBe` ReverseIncrementalSearch
  
  describe "Undo" $ do
    it "Ctrl-_ undoes" $ do
      processMgEvent (ctrlEvent '_') `shouldBe` Undo
    
    it "Ctrl-X u also undoes" $ do
      processMgSequence [ctrlX, charEvent 'u'] `shouldBe` Undo
  
  describe "Miscellaneous" $ do
    it "Ctrl-G quits command" $ do
      processMgEvent (ctrlEvent 'g') `shouldBe` Quit
    
    it "Ctrl-L recenters" $ do
      processMgEvent (ctrlEvent 'l') `shouldBe` RecenterLine
    
    it "Alt-X executes extended command" $ do
      processMgEvent (altEvent 'x') `shouldBe` ExecuteExtendedCommand
    
    prop "Ctrl-Q quotes next character" $ \c ->
      processMgSequence [ctrlEvent 'q', charEvent c] == QuotedInsert c

-- Test types
data MgAction
  = ForwardChar
  | BackwardChar
  | NextLine
  | PreviousLine
  | BeginningOfLine
  | EndOfLine
  | ForwardWord
  | BackwardWord
  | BeginningOfBuffer
  | EndOfBuffer
  | ScrollDown
  | ScrollUp
  | DeleteChar
  | BackwardDeleteChar
  | KillWord
  | BackwardKillWord
  | KillLine
  | SetMark
  | ExchangePointMark
  | KillRegion
  | CopyRegion
  | FindFile
  | SaveBuffer
  | WriteFile
  | ExitMg
  | SwitchBuffer
  | KillBuffer
  | ListBuffers
  | IncrementalSearch
  | ReverseIncrementalSearch
  | Undo
  | Quit
  | RecenterLine
  | ExecuteExtendedCommand
  | QuotedInsert Char
  deriving (Show, Eq)

-- Test helpers
ctrlEvent :: Char -> Event
ctrlEvent = error "ctrlEvent stub"

altEvent :: Char -> Event
altEvent = error "altEvent stub"

charEvent :: Char -> Event
charEvent = error "charEvent stub"

backspaceKey :: Event
backspaceKey = error "backspaceKey stub"

ctrlX :: Event
ctrlX = ctrlEvent 'x'

processMgEvent :: Event -> MgAction
processMgEvent _ = ForwardChar  -- Stub

processMgSequence :: [Event] -> MgAction
processMgSequence _ = FindFile  -- Stub