{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Yi.Config.Default.Mg
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  stable
-- Portability :  portable
--
-- Default configuration for the Mg (Micro GNU Emacs) keymap.

module Yi.Config.Default.Mg
  ( configureMg
  , mgConfig
  ) where

import           Lens.Micro.Platform
import           Yi.Config
import           Yi.Config.Lens
import           Yi.Config.Misc
import qualified Yi.Keymap.Mg as Mg
import           Yi.Style

-- | Configure Yi to use the Mg keymap
configureMg :: ConfigM ()
configureMg = do
  defaultKmA .= Mg.keymapSet
  configFontSizeA .= Just 12
  configThemeA .= mgTheme
  configLineNumbersA .= True
  configAutoHideScrollBarA .= True
  configWindowFillA .= ' '
  
  -- Mg-specific settings
  modeTableA %= addMode fundamentalMode
  modeTableA %= addMode textMode

-- | Mg-specific configuration
mgConfig :: Config -> Config
mgConfig cfg = cfg
  { defaultKm = Mg.keymapSet
  , configTheme = mgTheme
  , startActions = [printMsg "Mg (Micro GNU Emacs) - C-h for help"]
  }

-- | Mg theme (similar to classic Emacs)
mgTheme :: Theme
mgTheme = defaultTheme
  { modelineAttributes = emptyAttributes { foreground = white, background = darkBlue }
  , selectedStyle      = withBg darkGray
  , eofStyle          = withFg darkGray
  , lineNumberStyle   = withFg darkGray
  }
  where
    darkBlue = RGB 0 0 139
    darkGray = RGB 105 105 105
    white = RGB 255 255 255

-- | Fundamental mode (default for Mg)
fundamentalMode :: AnyMode
fundamentalMode = error "fundamentalMode not implemented"

-- | Text mode
textMode :: AnyMode  
textMode = error "textMode not implemented"

addMode :: AnyMode -> a -> a
addMode = error "addMode not implemented"