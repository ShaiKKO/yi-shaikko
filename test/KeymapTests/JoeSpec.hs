{-# LANGUAGE OverloadedStrings #-}

module KeymapTests.JoeSpec (spec) where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Control.Monad
import           Data.Text (Text)
import qualified Data.Text as T

-- Import Joe keymap
import           Yi.Keymap.Joe
import           Yi.Event
import           Yi.Keymap
import           Yi.Types

spec :: Spec
spec = describe "Joe Keymap" $ do
  describe "Basic Editing" $ do
    it "Ctrl-K H shows help" $ do
      processJoeSequence [ctrlK, charEvent 'h'] `shouldBe` ShowHelp
    
    it "Ctrl-C aborts command" $ do
      processJoeEvent (ctrlEvent 'c') `shouldBe` AbortCommand
    
    it "Ctrl-K X saves and exits" $ do
      processJoeSequence [ctrlK, charEvent 'x'] `shouldBe` SaveAndExit
    
    it "Ctrl-K Q exits without saving" $ do
      processJoeSequence [ctrlK, charEvent 'q'] `shouldBe` QuitNoSave
  
  describe "File Operations" $ do
    it "Ctrl-K D saves file" $ do
      processJoeSequence [ctrlK, charEvent 'd'] `shouldBe` SaveFile
    
    it "Ctrl-K E opens file" $ do
      processJoeSequence [ctrlK, charEvent 'e'] `shouldBe` EditFile
    
    it "Ctrl-K R inserts file" $ do
      processJoeSequence [ctrlK, charEvent 'r'] `shouldBe` InsertFile
    
    it "Ctrl-K W writes block to file" $ do
      processJoeSequence [ctrlK, charEvent 'w'] `shouldBe` WriteBlock
  
  describe "Block Operations" $ do
    it "Ctrl-K B marks block begin" $ do
      processJoeSequence [ctrlK, charEvent 'b'] `shouldBe` MarkBlockBegin
    
    it "Ctrl-K K marks block end" $ do
      processJoeSequence [ctrlK, charEvent 'k'] `shouldBe` MarkBlockEnd
    
    it "Ctrl-K C copies block" $ do
      processJoeSequence [ctrlK, charEvent 'c'] `shouldBe` CopyBlock
    
    it "Ctrl-K M moves block" $ do
      processJoeSequence [ctrlK, charEvent 'm'] `shouldBe` MoveBlock
    
    it "Ctrl-K Y deletes block" $ do
      processJoeSequence [ctrlK, charEvent 'y'] `shouldBe` DeleteBlock
    
    it "Ctrl-K U unmarks block" $ do
      processJoeSequence [ctrlK, charEvent 'u'] `shouldBe` UnmarkBlock
  
  describe "Navigation" $ do
    it "Ctrl-A moves to line start" $ do
      processJoeEvent (ctrlEvent 'a') `shouldBe` GoToLineStart
    
    it "Ctrl-E moves to line end" $ do
      processJoeEvent (ctrlEvent 'e') `shouldBe` GoToLineEnd
    
    it "Ctrl-U scrolls up half screen" $ do
      processJoeEvent (ctrlEvent 'u') `shouldBe` ScrollUpHalf
    
    it "Ctrl-V scrolls down half screen" $ do
      processJoeEvent (ctrlEvent 'v') `shouldBe` ScrollDownHalf
    
    it "Ctrl-Y scrolls up one line" $ do
      processJoeEvent (ctrlEvent 'y') `shouldBe` ScrollUpLine
    
    it "Ctrl-K L goes to line number" $ do
      processJoeSequence [ctrlK, charEvent 'l'] `shouldBe` GoToLine
  
  describe "Search and Replace" $ do
    it "Ctrl-K F finds text" $ do
      processJoeSequence [ctrlK, charEvent 'f'] `shouldBe` FindText
    
    it "Ctrl-L finds next" $ do
      processJoeEvent (ctrlEvent 'l') `shouldBe` FindNext
    
    it "Ctrl-K S finds and replaces" $ do
      processJoeSequence [ctrlK, charEvent 's'] `shouldBe` FindReplace
  
  describe "Window Management" $ do
    it "Ctrl-K O opens window" $ do
      processJoeSequence [ctrlK, charEvent 'o'] `shouldBe` OpenWindow
    
    it "Ctrl-K G grows window" $ do
      processJoeSequence [ctrlK, charEvent 'g'] `shouldBe` GrowWindow
    
    it "Ctrl-K T shrinks window" $ do
      processJoeSequence [ctrlK, charEvent 't'] `shouldBe` ShrinkWindow
    
    it "Ctrl-K N next window" $ do
      processJoeSequence [ctrlK, charEvent 'n'] `shouldBe` NextWindow
    
    it "Ctrl-K P previous window" $ do
      processJoeSequence [ctrlK, charEvent 'p'] `shouldBe` PrevWindow
  
  describe "Undo/Redo" $ do
    it "Ctrl-_ undoes" $ do
      processJoeEvent (ctrlEvent '_') `shouldBe` Undo
    
    it "Ctrl-^ redoes" $ do
      processJoeEvent (ctrlEvent '^') `shouldBe` Redo
  
  describe "Special Characters" $ do
    prop "Quoted insert works" $ \c ->
      processJoeSequence [ctrlEvent 'q', charEvent c] == InsertLiteral c
    
    it "Tab inserts tab or spaces" $ do
      processJoeEvent tabKey `shouldBe` InsertTab
    
    it "Backspace deletes backward" $ do
      processJoeEvent backspaceKey `shouldBe` DeleteBackward
    
    it "Delete deletes forward" $ do
      processJoeEvent deleteKey `shouldBe` DeleteForward

-- Test types
data JoeAction
  = ShowHelp
  | AbortCommand
  | SaveAndExit
  | QuitNoSave
  | SaveFile
  | EditFile
  | InsertFile
  | WriteBlock
  | MarkBlockBegin
  | MarkBlockEnd
  | CopyBlock
  | MoveBlock
  | DeleteBlock
  | UnmarkBlock
  | GoToLineStart
  | GoToLineEnd
  | ScrollUpHalf
  | ScrollDownHalf
  | ScrollUpLine
  | GoToLine
  | FindText
  | FindNext
  | FindReplace
  | OpenWindow
  | GrowWindow
  | ShrinkWindow
  | NextWindow
  | PrevWindow
  | Undo
  | Redo
  | InsertLiteral Char
  | InsertTab
  | DeleteBackward
  | DeleteForward
  deriving (Show, Eq)

-- Test helpers
ctrlEvent :: Char -> Event
ctrlEvent = error "ctrlEvent stub"

charEvent :: Char -> Event
charEvent = error "charEvent stub"

tabKey :: Event
tabKey = error "tabKey stub"

backspaceKey :: Event
backspaceKey = error "backspaceKey stub"

deleteKey :: Event
deleteKey = error "deleteKey stub"

ctrlK :: Event
ctrlK = ctrlEvent 'k'

processJoeEvent :: Event -> JoeAction
processJoeEvent _ = AbortCommand  -- Stub

processJoeSequence :: [Event] -> JoeAction
processJoeSequence _ = ShowHelp  -- Stub