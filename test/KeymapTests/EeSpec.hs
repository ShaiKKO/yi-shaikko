{-# LANGUAGE OverloadedStrings #-}

module KeymapTests.EeSpec (spec) where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Control.Monad
import           Data.Text (Text)
import qualified Data.Text as T

-- Import Ee keymap
import           Yi.Keymap.Ee
import           Yi.Event
import           Yi.Keymap
import           Yi.Types

spec :: Spec
spec = describe "Ee Keymap" $ do
  describe "Escape Menus" $ do
    it "ESC shows main menu" $ do
      processEeEvent escapeKey `shouldBe` ShowMainMenu
    
    it "ESC-f shows file menu" $ do
      processEeSequence [escapeKey, charEvent 'f'] `shouldBe` ShowFileMenu
    
    it "ESC-e shows edit menu" $ do
      processEeSequence [escapeKey, charEvent 'e'] `shouldBe` ShowEditMenu
    
    it "ESC-s shows search menu" $ do
      processEeSequence [escapeKey, charEvent 's'] `shouldBe` ShowSearchMenu
    
    it "ESC-h shows help menu" $ do
      processEeSequence [escapeKey, charEvent 'h'] `shouldBe` ShowHelpMenu
  
  describe "File Operations (ESC-f)" $ do
    it "ESC-f o opens file" $ do
      processEeSequence [escapeKey, charEvent 'f', charEvent 'o'] `shouldBe` OpenFile
    
    it "ESC-f s saves file" $ do
      processEeSequence [escapeKey, charEvent 'f', charEvent 's'] `shouldBe` SaveFile
    
    it "ESC-f a saves as" $ do
      processEeSequence [escapeKey, charEvent 'f', charEvent 'a'] `shouldBe` SaveAs
    
    it "ESC-f n creates new file" $ do
      processEeSequence [escapeKey, charEvent 'f', charEvent 'n'] `shouldBe` NewFile
    
    it "ESC-f q quits editor" $ do
      processEeSequence [escapeKey, charEvent 'f', charEvent 'q'] `shouldBe` QuitEditor
  
  describe "Edit Operations (ESC-e)" $ do
    it "ESC-e c copies selection" $ do
      processEeSequence [escapeKey, charEvent 'e', charEvent 'c'] `shouldBe` Copy
    
    it "ESC-e x cuts selection" $ do
      processEeSequence [escapeKey, charEvent 'e', charEvent 'x'] `shouldBe` Cut
    
    it "ESC-e v pastes" $ do
      processEeSequence [escapeKey, charEvent 'e', charEvent 'v'] `shouldBe` Paste
    
    it "ESC-e u undoes" $ do
      processEeSequence [escapeKey, charEvent 'e', charEvent 'u'] `shouldBe` Undo
    
    it "ESC-e r redoes" $ do
      processEeSequence [escapeKey, charEvent 'e', charEvent 'r'] `shouldBe` Redo
    
    it "ESC-e a selects all" $ do
      processEeSequence [escapeKey, charEvent 'e', charEvent 'a'] `shouldBe` SelectAll
  
  describe "Search Operations (ESC-s)" $ do
    it "ESC-s f finds text" $ do
      processEeSequence [escapeKey, charEvent 's', charEvent 'f'] `shouldBe` FindText
    
    it "ESC-s n finds next" $ do
      processEeSequence [escapeKey, charEvent 's', charEvent 'n'] `shouldBe` FindNext
    
    it "ESC-s p finds previous" $ do
      processEeSequence [escapeKey, charEvent 's', charEvent 'p'] `shouldBe` FindPrevious
    
    it "ESC-s r replaces text" $ do
      processEeSequence [escapeKey, charEvent 's', charEvent 'r'] `shouldBe` Replace
    
    it "ESC-s g goes to line" $ do
      processEeSequence [escapeKey, charEvent 's', charEvent 'g'] `shouldBe` GotoLine
  
  describe "Navigation" $ do
    prop "Arrow keys work normally" $ \dir ->
      processEeNavigation (arrowKey dir) == Navigate dir
    
    it "Page Up scrolls up" $ do
      processEeEvent pageUpKey `shouldBe` PageUp
    
    it "Page Down scrolls down" $ do
      processEeEvent pageDownKey `shouldBe` PageDown
    
    it "Home goes to line start" $ do
      processEeEvent homeKey `shouldBe` LineStart
    
    it "End goes to line end" $ do
      processEeEvent endKey `shouldBe` LineEnd
  
  describe "Basic Editing" $ do
    prop "Regular characters insert" $ \c ->
      isPrintable c ==>
        processEeEvent (charEvent c) == InsertChar c
    
    it "Tab inserts tab/spaces" $ do
      processEeEvent tabKey `shouldBe` InsertTab
    
    it "Enter inserts newline" $ do
      processEeEvent enterKey `shouldBe` InsertNewline
    
    it "Backspace deletes backward" $ do
      processEeEvent backspaceKey `shouldBe` DeleteBackward
    
    it "Delete deletes forward" $ do
      processEeEvent deleteKey `shouldBe` DeleteForward
  
  describe "Help System" $ do
    it "ESC-h h shows general help" $ do
      processEeSequence [escapeKey, charEvent 'h', charEvent 'h'] `shouldBe` ShowGeneralHelp
    
    it "ESC-h k shows keyboard shortcuts" $ do
      processEeSequence [escapeKey, charEvent 'h', charEvent 'k'] `shouldBe` ShowKeyboardHelp
    
    it "ESC-h a shows about" $ do
      processEeSequence [escapeKey, charEvent 'h', charEvent 'a'] `shouldBe` ShowAbout
  
  describe "Menu Navigation" $ do
    it "ESC ESC cancels menu" $ do
      processEeSequence [escapeKey, escapeKey] `shouldBe` CancelMenu
    
    prop "Number keys select menu items" $ \n ->
      n >= 1 && n <= 9 ==>
        processEeMenuSelection n == SelectMenuItem n

-- Test types
data EeAction
  = ShowMainMenu
  | ShowFileMenu
  | ShowEditMenu
  | ShowSearchMenu
  | ShowHelpMenu
  | OpenFile
  | SaveFile
  | SaveAs
  | NewFile
  | QuitEditor
  | Copy
  | Cut
  | Paste
  | Undo
  | Redo
  | SelectAll
  | FindText
  | FindNext
  | FindPrevious
  | Replace
  | GotoLine
  | Navigate Direction
  | PageUp
  | PageDown
  | LineStart
  | LineEnd
  | InsertChar Char
  | InsertTab
  | InsertNewline
  | DeleteBackward
  | DeleteForward
  | ShowGeneralHelp
  | ShowKeyboardHelp
  | ShowAbout
  | CancelMenu
  | SelectMenuItem Int
  deriving (Show, Eq)

data Direction = Up | Down | Left | Right
  deriving (Show, Eq, Enum, Bounded)

instance Arbitrary Direction where
  arbitrary = arbitraryBoundedEnum

-- Test helpers
escapeKey :: Event
escapeKey = error "escapeKey stub"

charEvent :: Char -> Event
charEvent = error "charEvent stub"

arrowKey :: Direction -> Event
arrowKey = error "arrowKey stub"

pageUpKey :: Event
pageUpKey = error "pageUpKey stub"

pageDownKey :: Event
pageDownKey = error "pageDownKey stub"

homeKey :: Event
homeKey = error "homeKey stub"

endKey :: Event
endKey = error "endKey stub"

tabKey :: Event
tabKey = error "tabKey stub"

enterKey :: Event
enterKey = error "enterKey stub"

backspaceKey :: Event
backspaceKey = error "backspaceKey stub"

deleteKey :: Event
deleteKey = error "deleteKey stub"

processEeEvent :: Event -> EeAction
processEeEvent _ = ShowMainMenu  -- Stub

processEeSequence :: [Event] -> EeAction
processEeSequence _ = ShowMainMenu  -- Stub

processEeNavigation :: Event -> EeAction
processEeNavigation _ = Navigate Up  -- Stub

processEeMenuSelection :: Int -> EeAction
processEeMenuSelection n = SelectMenuItem n

isPrintable :: Char -> Bool
isPrintable c = c >= ' ' && c <= '~'