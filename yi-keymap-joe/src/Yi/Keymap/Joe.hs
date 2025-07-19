{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      :  Yi.Keymap.Joe
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Joe's Own Editor (JOE) style keymap for Yi.
-- Implements WordStar-compatible bindings with extensive Ctrl+K sequences.

module Yi.Keymap.Joe
  ( keymapSet
  , mkKeymapSet
  , joeKeymap
  , JoeConfig(..)
  , defaultJoeConfig
  ) where

import           Control.Monad
import           Control.Monad.State
import           Data.Default
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Lens.Micro.Platform
import           Yi.Buffer
import           Yi.Editor
import           Yi.Event
import           Yi.File
import           Yi.Keymap
import           Yi.Keymap.Keys
import           Yi.MiniBuffer
import           Yi.Rectangle
import           Yi.Search
import           Yi.String
import           Yi.Window

-- | Joe configuration
data JoeConfig = JoeConfig
  { joeAutoIndent :: Bool
  , joeTabSize    :: Int
  , joeWordWrap   :: Bool
  , joeMarkRing   :: Int  -- ^ Number of marks to remember
  }

instance Default JoeConfig where
  def = defaultJoeConfig

defaultJoeConfig :: JoeConfig
defaultJoeConfig = JoeConfig
  { joeAutoIndent = True
  , joeTabSize = 8
  , joeWordWrap = False
  , joeMarkRing = 10
  }

-- | Joe keymap state
data JoeState = JoeState
  { joeMarks :: M.Map Int Point  -- ^ Numbered marks
  , joeBlock :: Maybe Region     -- ^ Current block selection
  }

-- | Create Joe keymap set
mkKeymapSet :: JoeConfig -> KeymapSet
mkKeymapSet config = modelessKeymapSet $ joeKeymap config

-- | Default keymap set
keymapSet :: KeymapSet
keymapSet = mkKeymapSet defaultJoeConfig

-- | Main Joe keymap (WordStar-style)
joeKeymap :: JoeConfig -> Keymap
joeKeymap config = choice
  [ -- Basic movement
    spec KUp         ?>>! moveUp
  , spec KDown       ?>>! moveDown
  , spec KLeft       ?>>! moveLeft
  , spec KRight      ?>>! moveRight
  , spec KHome       ?>>! moveToSol
  , spec KEnd        ?>>! moveToEol
  , spec KPageUp     ?>>! scrollPageUp
  , spec KPageDown   ?>>! scrollPageDown
  
    -- WordStar diamond (classic cursor movement)
  , ctrl (char 'e')  ?>>! moveUp
  , ctrl (char 'x')  ?>>! moveDown
  , ctrl (char 's')  ?>>! moveLeft
  , ctrl (char 'd')  ?>>! moveRight
  , ctrl (char 'a')  ?>>! moveWordLeft
  , ctrl (char 'f')  ?>>! moveWordRight
  
    -- Basic editing
  , spec KBS         ?>>! deleteCharB
  , spec KDel        ?>>! deleteCharB
  , spec KEnter      ?>>! newlineAndIndentJoe config
  , spec KTab        ?>>! insertTab config
  
    -- Control key sequences
  , ctrl (char 'g')  ?>>! deleteCharB         -- Delete char
  , ctrl (char 't')  ?>>! deleteWordForward   -- Delete word
  , ctrl (char 'y')  ?>>! killLineB           -- Delete line
  , ctrl (char 'w')  ?>>! scrollUpB           -- Scroll up
  , ctrl (char 'z')  ?>>! scrollDownB         -- Scroll down
  , ctrl (char 'l')  ?>>! gotoLinePrompt      -- Goto line
  , ctrl (char '_')  ?>>! undoB               -- Undo
  , ctrl (char '^')  ?>>! redoB               -- Redo
  
    -- Ctrl+K sequences (main command prefix)
  , ctrl (char 'k')  ?>> ctrlKKeymap config
  
    -- Ctrl+Q sequences (literal/quote)
  , ctrl (char 'q')  ?>> ctrlQKeymap
  
    -- Search
  , ctrl (char 'k') >> ctrl (char 'f') ?>>! searchPrompt Forward
  , ctrl (char 'k') >> ctrl (char 'r') ?>>! searchPrompt Backward
  
    -- Help
  , ctrl (char 'k') >> ctrl (char 'h') ?>>! showHelp
  , spec KF1                            ?>>! showHelp
  
    -- Self-inserting
  , anyEvent >>= insertSelfB
  ]

-- | Ctrl+K command sequences
ctrlKKeymap :: JoeConfig -> Keymap
ctrlKKeymap config = choice
  [ -- File operations
    char 'd'  ?>>! fwriteE                    -- Save
  , char 'x'  ?>>! fwriteE >> closeWindow    -- Save and exit
  , char 'q'  ?>>! askQuitEditor             -- Quit without save
  , char 'e'  ?>>! findFilePrompt            -- Edit file
  , char 'r'  ?>>! insertFilePrompt          -- Read file
  , char 'w'  ?>>! writeBlockPrompt          -- Write block
  
    -- Block operations
  , char 'b'  ?>>! markBlockBegin            -- Begin block
  , char 'k'  ?>>! markBlockEnd              -- End block
  , char 'c'  ?>>! copyBlock                 -- Copy block
  , char 'v'  ?>>! moveBlock                 -- Move block
  , char 'y'  ?>>! deleteBlock               -- Delete block
  , char 'h'  ?>>! toggleBlockHide           -- Hide/show block
  
    -- Window operations
  , char 'o'  ?>>! splitWindow               -- Split window
  , char 'i'  ?>>! vertSplitWindow           -- Vertical split
  , char 'g'  ?>>! enlargeWindow             -- Grow window
  , char 't'  ?>>! shrinkWindow              -- Shrink window
  , char 'n'  ?>>! nextWindow                -- Next window
  , char 'p'  ?>>! prevWindow                -- Previous window
  
    -- Marks (0-9)
  , digit    >>= \n -> setMarkN n            -- Set mark 0-9
  , char ' ' >> digit >>= \n -> gotoMarkN n  -- Goto mark 0-9
  
    -- Misc
  , char 'a'  ?>>! centerLine                -- Center line
  , char 'j'  ?>>! fillParagraph             -- Justify paragraph
  , char 'l'  ?>>! gotoLinePrompt            -- Line number
  , char 's'  ?>>! fwriteE                   -- Save (alias)
  , char 'u'  ?>>! topOfScreen               -- Top of screen
  , char 'v'  ?>>! bottomOfScreen            -- Bottom of screen
  ]

-- | Ctrl+Q sequences (quote/literal)
ctrlQKeymap :: Keymap
ctrlQKeymap = do
  -- Next key is inserted literally
  ev <- anyEvent
  case ev of
    Event _ (KASCII c) -> withCurrentBuffer $ insertB c
    _ -> return ()

-- | Block operations
markBlockBegin :: YiM ()
markBlockBegin = withCurrentBuffer $ do
  pos <- pointB
  -- Store block begin position
  setDynamic ("joe.blockBegin" :: String, pos)

markBlockEnd :: YiM ()
markBlockEnd = withCurrentBuffer $ do
  pos <- pointB
  getDynamic ("joe.blockBegin" :: String) >>= \case
    Just beginPos -> do
      let region = mkRegion beginPos pos
      setSelectRegionB region
      setVisibleSelection True
    Nothing -> return ()

copyBlock :: YiM ()
copyBlock = withCurrentBuffer $ do
  getSelectRegionB >>= readRegionB >>= setClipboard

moveBlock :: YiM ()
moveBlock = withCurrentBuffer $ do
  region <- getSelectRegionB
  text <- readRegionB region
  deleteRegionB region
  insertN text

deleteBlock :: YiM ()
deleteBlock = withCurrentBuffer $ do
  getSelectRegionB >>= deleteRegionB

toggleBlockHide :: YiM ()
toggleBlockHide = withCurrentBuffer $ do
  vis <- getVisibleSelection
  setVisibleSelection (not vis)

-- | Mark operations
setMarkN :: Int -> YiM ()
setMarkN n = withCurrentBuffer $ do
  pos <- pointB
  setDynamic (markKey n, pos)

gotoMarkN :: Int -> YiM ()
gotoMarkN n = withCurrentBuffer $ do
  getDynamic (markKey n) >>= \case
    Just pos -> moveTo pos
    Nothing -> return ()

markKey :: Int -> String
markKey n = "joe.mark." ++ show n

-- | Window operations  
splitWindow :: YiM ()
splitWindow = splitE

vertSplitWindow :: YiM ()
vertSplitWindow = vsplitE

enlargeWindow :: YiM ()
enlargeWindow = modifyWindowHeightE 1

shrinkWindow :: YiM ()
shrinkWindow = modifyWindowHeightE (-1)

nextWindow :: YiM ()
nextWindow = nextWinE

prevWindow :: YiM ()
prevWindow = prevWinE

-- | Line operations
centerLine :: YiM ()
centerLine = withCurrentBuffer $ do
  -- Center current line in window
  scrollToCursorB

topOfScreen :: YiM ()
topOfScreen = withCurrentBuffer $ do
  -- Move to top visible line
  getWindowStartB >>= moveTo

bottomOfScreen :: YiM ()
bottomOfScreen = withCurrentBuffer $ do
  -- Move to bottom visible line
  getWindowEndB >>= moveTo

-- | Prompts
gotoLinePrompt :: YiM ()
gotoLinePrompt = do
  line <- promptRead "Goto line: "
  case reads line of
    [(n, "")] -> withCurrentBuffer $ gotoLn n
    _ -> return ()

searchPrompt :: Direction -> YiM ()
searchPrompt dir = do
  pattern <- promptRead $ case dir of
    Forward -> "Search: "
    Backward -> "Reverse search: "
  setRegexE pattern
  searchE dir

findFilePrompt :: YiM ()
findFilePrompt = do
  file <- promptRead "Edit file: "
  fnewE file

insertFilePrompt :: YiM ()
insertFilePrompt = do
  file <- promptRead "Insert file: "
  content <- io $ readFile file
  withCurrentBuffer $ insertN content

writeBlockPrompt :: YiM ()
writeBlockPrompt = do
  file <- promptRead "Write block to: "
  withCurrentBuffer $ do
    getSelectRegionB >>= readRegionB >>= \text ->
      io $ writeFile file (T.unpack text)

-- | Help
showHelp :: YiM ()
showHelp = do
  -- Show Joe help in minibuffer or new buffer
  withEditor $ printMsg joeHelp

joeHelp :: String
joeHelp = unlines
  [ "JOE Editor Commands - Quick Reference"
  , "^K D  Save          ^K X  Save & Exit   ^K Q  Quit"
  , "^K B  Begin Block   ^K K  End Block     ^K C  Copy"
  , "^K V  Move Block    ^K Y  Delete Block  ^K W  Write Block"
  , "^K F  Find          ^K R  Replace       ^L    Goto Line"
  , "^K H  Help          ^K O  Split Win     ^K N  Next Win"
  ]

-- | Indentation
newlineAndIndentJoe :: JoeConfig -> YiM ()
newlineAndIndentJoe config = withCurrentBuffer $ do
  newlineB
  when (joeAutoIndent config) $ do
    indentAsTheMostIndentedNeighborLineB

insertTab :: JoeConfig -> YiM ()
insertTab config = withCurrentBuffer $ do
  insertN $ replicate (joeTabSize config) ' '

-- | Word movement
moveWordLeft :: YiM ()
moveWordLeft = withCurrentBuffer $ moveB unitWord Backward

moveWordRight :: YiM ()
moveWordRight = withCurrentBuffer $ moveB unitWord Forward

deleteWordForward :: YiM ()
deleteWordForward = withCurrentBuffer $ deleteB unitWord Forward

-- | Paragraph operations
fillParagraph :: YiM ()
fillParagraph = withCurrentBuffer $ do
  -- Simple paragraph filling
  fillParagraphB $ FillParagraph 72

-- Helper functions
promptRead :: String -> YiM String
promptRead prompt = withMinibuffer prompt (const $ return ()) id

digit :: Keymap Char
digit = oneOf $ map char ['0'..'9']

-- Some functions that would need implementation
setClipboard :: T.Text -> BufferM ()
setClipboard = error "setClipboard not implemented"

getWindowStartB :: BufferM Point
getWindowStartB = error "getWindowStartB not implemented"

getWindowEndB :: BufferM Point
getWindowEndB = error "getWindowEndB not implemented"

modifyWindowHeightE :: Int -> YiM ()
modifyWindowHeightE = error "modifyWindowHeightE not implemented"

setDynamic :: (String, Point) -> BufferM ()
setDynamic = error "setDynamic not implemented"

getDynamic :: String -> BufferM (Maybe Point)
getDynamic = error "getDynamic not implemented"

fillParagraphB :: FillParagraph -> BufferM ()
fillParagraphB = error "fillParagraphB not implemented"

data FillParagraph = FillParagraph Int