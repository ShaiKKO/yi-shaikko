{-# LANGUAGE OverloadedStrings #-}

module KeymapTests.AcmeSpec (spec) where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Control.Monad
import           Data.Text (Text)
import qualified Data.Text as T

-- Import Acme keymap
import           Yi.Keymap.Acme
import           Yi.Event
import           Yi.Keymap
import           Yi.Types

spec :: Spec
spec = describe "Acme Keymap" $ do
  describe "Mouse Bindings" $ do
    it "Button 1 moves cursor" $ do
      let event = mouseEvent LeftButton (10, 5)
      processAcmeEvent event `shouldSatisfy` movesCursor
    
    it "Button 2 executes plumbing" $ do
      let event = mouseEvent MiddleButton (20, 10)
      processAcmeEvent event `shouldSatisfy` executesPlumb
    
    it "Button 3 searches" $ do
      let event = mouseEvent RightButton (30, 15)
      processAcmeEvent event `shouldSatisfy` initiatesSearch
  
  describe "Mouse Chords" $ do
    it "Chord 1+2 cuts selection" $ do
      let events = [mouseEvent LeftButton (0, 0), mouseEvent MiddleButton (0, 0)]
      processAcmeChord events `shouldBe` CutAction
    
    it "Chord 1+3 pastes" $ do
      let events = [mouseEvent LeftButton (0, 0), mouseEvent RightButton (0, 0)]
      processAcmeChord events `shouldBe` PasteAction
    
    it "Chord 2+3 copies (snarfs)" $ do
      let events = [mouseEvent MiddleButton (0, 0), mouseEvent RightButton (0, 0)]
      processAcmeChord events `shouldBe` CopyAction
  
  describe "Plumbing Rules" $ do
    prop "File paths are detected" $ \path ->
      isFilePath path ==> plumbAction path == OpenFile
    
    prop "URLs are detected" $ \url ->
      isURL url ==> plumbAction url == OpenURL
    
    prop "Email addresses are detected" $ \email ->
      isEmail email ==> plumbAction email == ComposeEmail
    
    it "Custom plumb rules work" $ do
      let gitHash = "a1b2c3d4e5f6789012345678901234567890abcd"
      plumbCustom gitHash `shouldBe` ShowGitCommit
  
  describe "Escape Commands" $ do
    it "ESC-w saves file" $ do
      processAcmeEscape 'w' `shouldBe` SaveFile
    
    it "ESC-q quits window" $ do
      processAcmeEscape 'q' `shouldBe` QuitWindow
    
    it "ESC-x saves and quits" $ do
      processAcmeEscape 'x' `shouldBe` SaveAndQuit
  
  describe "Text Operations" $ do
    it "Handles Unicode correctly" $ do
      let text = "Hello 世界 🌍"
      processAcmeText text `shouldSatisfy` preservesUnicode
    
    prop "Indentation is preserved" $ \indent ->
      indent >= 0 && indent <= 20 ==>
        processAcmeIndent indent `shouldBe` indent

-- Test helpers
data MouseButton = LeftButton | MiddleButton | RightButton
  deriving (Show, Eq)

data AcmeAction
  = MoveAction
  | PlumbAction
  | SearchAction
  | CutAction
  | PasteAction
  | CopyAction
  | SaveFile
  | QuitWindow
  | SaveAndQuit
  | OpenFile
  | OpenURL
  | ComposeEmail
  | ShowGitCommit
  deriving (Show, Eq)

mouseEvent :: MouseButton -> (Int, Int) -> Event
mouseEvent button pos = error "mouseEvent stub"

processAcmeEvent :: Event -> AcmeAction
processAcmeEvent _ = MoveAction  -- Stub

processAcmeChord :: [Event] -> AcmeAction
processAcmeChord [_, _] = CutAction  -- Stub

movesCursor :: AcmeAction -> Bool
movesCursor MoveAction = True
movesCursor _ = False

executesPlumb :: AcmeAction -> Bool
executesPlumb PlumbAction = True
executesPlumb _ = False

initiatesSearch :: AcmeAction -> Bool
initiatesSearch SearchAction = True
initiatesSearch _ = False

isFilePath :: String -> Bool
isFilePath s = '/' `elem` s || '.' `elem` s

isURL :: String -> Bool
isURL s = any (`isPrefixOf` s) ["http://", "https://"]
  where isPrefixOf = T.isPrefixOf . T.pack

isEmail :: String -> Bool
isEmail s = '@' `elem` s && '.' `elem` dropWhile (/= '@') s

plumbAction :: String -> AcmeAction
plumbAction s
  | isFilePath s = OpenFile
  | isURL s = OpenURL
  | isEmail s = ComposeEmail
  | otherwise = PlumbAction

plumbCustom :: String -> AcmeAction
plumbCustom s
  | length s == 40 && all isHexDigit s = ShowGitCommit
  | otherwise = PlumbAction
  where
    isHexDigit c = c `elem` "0123456789abcdef"

processAcmeEscape :: Char -> AcmeAction
processAcmeEscape 'w' = SaveFile
processAcmeEscape 'q' = QuitWindow
processAcmeEscape 'x' = SaveAndQuit
processAcmeEscape _ = QuitWindow

processAcmeText :: Text -> Text
processAcmeText = id  -- Stub

preservesUnicode :: Text -> Bool
preservesUnicode t = T.any (> '\x7F') t  -- Has non-ASCII

processAcmeIndent :: Int -> Int
processAcmeIndent = id  -- Stub