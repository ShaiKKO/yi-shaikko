{-# LANGUAGE OverloadedStrings #-}

import Yi
import Yi.Config.Default.Vty (defaultConfig)
import Yi.Config.Simple.Types
import Yi.Syntax.AsyncBuffer
import Yi.Mode.Haskell (haskellMode)
import Yi.Mode.JavaScript (javascriptMode)
import Yi.Keymap.Emacs (defKeymap)
import qualified Yi.Mode.Haskell as Haskell
import qualified Yi.Mode.JavaScript as JS

-- | Example configuration showing async syntax highlighting
main :: IO ()
main = yi $ defaultConfig
  { defaultKm = defKeymap
  , configUI = (configUI defaultConfig)
      { configTheme = defaultTheme
      }
  , startActions = [editorA (printMsg "Yi with async syntax highlighting")]
  , modeTable = [haskellModeWithAsync, javaScriptModeWithAsync]
  }

-- | Haskell mode with Tree-sitter async highlighting
haskellModeWithAsync :: AnyMode
haskellModeWithAsync = AnyMode $ haskellMode
  { modeAsyncHighlight = Just $ \bufRef -> do
      let highlighter = mkAsyncHighlighter
            "haskell"
            (\path -> ".hs" `isSuffixOf` path || ".lhs" `isSuffixOf` path)
            (TreeSitter "/usr/local/bin/tree-sitter-haskell")
      runAsyncHighlight highlighter bufRef
  }

-- | JavaScript mode with Tree-sitter async highlighting  
javaScriptModeWithAsync :: AnyMode
javaScriptModeWithAsync = AnyMode $ javascriptMode
  { modeAsyncHighlight = Just $ \bufRef -> do
      let highlighter = mkAsyncHighlighter
            "javascript"
            (\path -> ".js" `isSuffixOf` path || ".jsx" `isSuffixOf` path)
            (TreeSitter "/usr/local/bin/tree-sitter-javascript")
      runAsyncHighlight highlighter bufRef
  }

-- You can also use LSP for semantic highlighting:
-- (LanguageServer "haskell-language-server")

-- Or a custom parser that outputs START END TYPE format:
-- (CustomParser "/path/to/parser" ["--json"])

-- The async highlighter runs in the background and won't block
-- the UI, even on large files. It integrates with Yi's existing
-- incremental highlighting system.