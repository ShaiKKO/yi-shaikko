{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Yi.Config.Default.Acme
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Default configuration for the Acme keymap.

module Yi.Config.Default.Acme
  ( configureAcme
  , acmeConfig
  ) where

import           Lens.Micro.Platform
import           Yi.Config
import           Yi.Config.Lens
import           Yi.Config.Misc
import qualified Yi.Keymap.Acme as Acme
import           Yi.Keymap.Registry

-- | Configure Yi to use the Acme keymap
configureAcme :: ConfigM ()
configureAcme = do
  defaultKmA .= Acme.keymapSet
  configFontSizeA .= Just 10
  configWindowFillA .= '░'
  configThemeA .= acmeTheme
  
  -- Register with keymap registry if available
  -- This would need proper integration
  return ()

-- | Acme-specific configuration
acmeConfig :: Config -> Config
acmeConfig cfg = cfg
  { defaultKm = Acme.keymapSet
  , configWindowFill = '░'
  , configTheme = acmeTheme
  }

-- | Acme color theme (yellow background, etc.)
acmeTheme :: Theme
acmeTheme = defaultTheme
  { modelineAttributes = emptyAttributes { foreground = black, background = lightYellow }
  , tabBarAttributes   = emptyAttributes { foreground = black, background = lightYellow }
  , selectedStyle      = withBg lightBlue
  }
  where
    lightYellow = RGB 255 255 224
    lightBlue = RGB 230 230 255
    black = RGB 0 0 0