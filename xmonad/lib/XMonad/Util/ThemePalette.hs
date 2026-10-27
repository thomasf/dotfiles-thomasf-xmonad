-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.ThemePalette
-- Description :  Theme palette loading and styling utilities for XMonad
-- Maintainer  :  thomasf@jossystem.se
-- Stability   :  unstable
-- Portability :  unportable
--
-- Loads color palette from ~/.config/theme/xmonad-palette and provides
-- helpers for colors, fonts, and GridSelect configuration.
-----------------------------------------------------------------------------

module XMonad.Util.ThemePalette
    ( Palette
    , readPalette
    , readPaletteFromFile
    , col
    , myFocusedColor
    , myUrgentColor
    , myNormalBorderColor
    , myGsconfig
    , sizedXftFont
    , sizedFont
    , largeFont
    ) where

import           Control.Exception (try, IOException)
import           Data.Char (isSpace)
import           Data.IORef
import qualified Data.Map as M
import           System.Environment (getEnv)
import           System.IO.Unsafe (unsafePerformIO)
import           XMonad.Actions.GridSelect

-- | The palette has the colors of the base16-tomorrow theme, plus "bg_0"
-- with the mode as "mode". dotfiles writes them to
-- ~/.config/theme/xmonad-palette, as a Haskell list of pairs, when it
-- installs or switches the theme; switching restarts xmonad.
type Palette = M.Map String String

{-# NOINLINE paletteRef #-}
paletteRef :: IORef (Maybe Palette)
paletteRef = unsafePerformIO (newIORef Nothing)

readPalette :: IO Palette
readPalette = do
  cached <- readIORef paletteRef
  case cached of
    Just pal -> return pal
    Nothing  -> do
      pal <- readPaletteFromFile
      writeIORef paletteRef (Just pal)
      return pal

readPaletteFromFile :: IO Palette
readPaletteFromFile = do
  home <- getEnv "HOME"
  s <- try (readFile $ home ++ "/.config/theme/xmonad-palette") :: IO (Either IOException String)
  return $ case s of
    Right str | [(ps, rest)] <- reads str, all isSpace rest -> M.fromList ps
    _ -> M.empty

-- | col p name is the color name of p, or grey if p does not have it, as
-- before dotfiles has written the palette.
col :: Palette -> String -> String
col p name = M.findWithDefault "#808080" name p

myFocusedColor :: Palette -> String
myFocusedColor pal = col pal "magenta"

myUrgentColor :: Palette -> String
myUrgentColor pal = col pal "blue"

myNormalBorderColor :: Palette -> String
myNormalBorderColor pal = col pal "bg_1"

-- | GridSelect in the colors of the palette, like rofi and the prompts: the
-- cells are a raised surface, bg_1 with fg_1 text, the selected one magenta
-- with bg_0 text, and bg_0 separates them instead of white outlines.
myGsconfig :: Palette -> GSConfig a
myGsconfig pal = (buildDefaultGSConfig colorizer)
    { gs_cellheight = 70
    , gs_cellwidth = 150
    , gs_cellpadding = 6
    , gs_font = sizedXftFont "13"
    , gs_bordercolor = col pal "bg_0"
    }
  where
    colorizer _ active = return $ if active
      then (col pal "magenta", col pal "bg_0")
      else (col pal "bg_1", col pal "fg_1")

-- | Fonts
sizedXftFont :: String -> String
sizedXftFont px = "xft:PragmataPro:pixelsize=" ++ px

sizedFont :: String -> String
sizedFont px = "-xos4-terminus-*-r-*-*-" ++ px  ++ "-*-*-*-*-*-iso8859-*"

largeFont :: String
largeFont = sizedFont "32"
