-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.ThemePalette
-- Description :  Theme palette loading and styling utilities for XMonad
-- Maintainer  :  thomasf@jossystem.se
-- Stability   :  unstable
-- Portability :  unportable
--
-- Loads color palette from ~/.config/theme/xmonad-palette and provides
-- helpers for colors, fonts, DPI scaling, and GridSelect configuration.
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
    , myGsconfigWithFont
    , sizedXftFont
    , sizedXftFontScaled
    , sizedFont
    , dzenFont
    , dzenFontScaled
    , largeFont
    , largeFontSize
    , baseDpi
    , dpiFromPalette
    , dpiScale
    , scaleDpi
    , showMessage
    , showMessageWithFont
    ) where

import           Control.Exception (try, IOException, SomeException)
import           Control.Monad ((>=>))
import           Data.Char (isSpace)
import           Data.IORef
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, listToMaybe)
import           System.Environment (getEnv, lookupEnv)
import           System.IO.Unsafe (unsafePerformIO)
import           Text.Read (readMaybe)
import           XMonad
import           XMonad.Actions.GridSelect
import qualified XMonad.StackSet as W
import qualified XMonad.Util.Dzen as DZ

-- | The palette has the colors of the base16-tomorrow theme, plus "bg_0"
-- with the mode as "mode" and "dpi". dotfiles writes them to
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

-- | Base baseline DPI (standard 1.0 scale).
baseDpi :: Double
baseDpi = 96.0

-- | Query X11 root window resource string for Xft.dpi.
tryGetXftDpi :: IO (Maybe Double)
tryGetXftDpi = do
  res <- try (do
    d <- openDisplay ""
    let rm = resourceManagerString d
    closeDisplay d
    return (parseXftDpi rm)) :: IO (Either SomeException (Maybe Double))
  case res of
    Right mDpi -> return mDpi
    Left _     -> return Nothing

-- | Parse Xft.dpi from X resource manager string.
parseXftDpi :: String -> Maybe Double
parseXftDpi str = listToMaybe [ d | l <- lines str
                                  , let (k, rest) = break (\c -> c == ':' || isSpace c) l
                                  , k == "Xft.dpi"
                                  , let val = dropWhile (\c -> c == ':' || isSpace c) rest
                                  , Just d <- [readMaybe val] ]

-- | Detect DPI from Xft.dpi in X resources, environment variable, or default 96.0.
detectDpi :: IO Double
detectDpi = do
  mDpi <- tryGetXftDpi
  case mDpi of
    Just d  -> return d
    Nothing -> do
      mEnv <- lookupEnv "XFT_DPI"
      case mEnv >>= readMaybe of
        Just d  -> return d
        Nothing -> return baseDpi

readPaletteFromFile :: IO Palette
readPaletteFromFile = do
  home <- getEnv "HOME"
  s <- try (readFile $ home ++ "/.config/theme/xmonad-palette") :: IO (Either IOException String)
  let pal = case s of
        Right str | [(ps, rest)] <- reads str, all isSpace rest -> M.fromList ps
        _ -> M.empty
  case M.lookup "dpi" pal of
    Just _  -> return pal
    Nothing -> do
      dpiVal <- detectDpi
      return $ M.insert "dpi" (show (round dpiVal :: Int)) pal

-- | col p name is the color name of p, or grey if p does not have it, as
-- before dotfiles has written the palette.
col :: Palette -> String -> String
col p name = M.findWithDefault "#808080" name p

-- | Extract DPI from Palette, falling back to 96.0 if unspecified or invalid.
dpiFromPalette :: Palette -> Double
dpiFromPalette pal = case M.lookup "dpi" pal of
  Just str -> fromMaybe baseDpi (readMaybe str)
  Nothing  -> baseDpi

-- | DPI scale factor relative to 96 DPI (e.g. 1.0 at 96 DPI, 1.77 at 170 DPI).
dpiScale :: Palette -> Double
dpiScale pal = dpiFromPalette pal / baseDpi

-- | Scale an integer value proportionally by DPI factor.
scaleDpi :: Palette -> Double -> Int
scaleDpi pal base = round (base * dpiScale pal)

myFocusedColor :: Palette -> String
myFocusedColor pal = col pal "magenta"

myUrgentColor :: Palette -> String
myUrgentColor pal = col pal "blue"

myNormalBorderColor :: Palette -> String
myNormalBorderColor pal = col pal "bg_1"

-- | GridSelect in the colors of the palette, scaled with DPI using specified font:
-- the cells are a raised surface, bg_1 with fg_1 text, the selected one magenta
-- with bg_0 text, and bg_0 separates them instead of white outlines.
myGsconfigWithFont :: String -> Palette -> GSConfig a
myGsconfigWithFont font pal = (buildDefaultGSConfig colorizer)
    { gs_cellheight = toInteger (scaleDpi pal 70)
    , gs_cellwidth = toInteger (scaleDpi pal 220)
    , gs_cellpadding = toInteger (scaleDpi pal 6)
    , gs_font = sizedXftFontScaled pal font 13
    , gs_bordercolor = col pal "bg_0"
    }
  where
    colorizer _ active = return $ if active
      then (col pal "magenta", col pal "bg_0")
      else (col pal "bg_1", col pal "fg_1")

-- | Default GridSelect in the colors of the palette, scaled with DPI
myGsconfig :: Palette -> GSConfig a
myGsconfig = myGsconfigWithFont "PragmataPro"

-- | Format Xft font string for given font family
sizedXftFont :: String -> String -> String
sizedXftFont font px = "xft:" ++ font ++ ":pixelsize=" ++ px

-- | Scaled Xft font based on DPI from palette
sizedXftFontScaled :: Palette -> String -> Double -> String
sizedXftFontScaled pal font basePx = sizedXftFont font (show (scaleDpi pal basePx))

sizedFont :: String -> String
sizedFont px = "-xos4-terminus-*-r-*-*-" ++ px  ++ "-*-*-*-*-*-iso8859-*"

-- | Format font string for dzen (fontconfig pattern without XMonad's "xft:" prefix)
dzenFont :: String -> String -> String
dzenFont font px = font ++ ":pixelsize=" ++ px

-- | Scaled font string for dzen based on DPI from palette
dzenFontScaled :: Palette -> String -> Double -> String
dzenFontScaled pal font basePx = dzenFont font (show (scaleDpi pal basePx))

-- | Scaled large font size in pixels for dzen status popups
largeFontSize :: Palette -> Int
largeFontSize pal = scaleDpi pal 22

-- | Scaled large font for dzen status popups (scaled with DPI, 39px at 170 DPI)
largeFont :: Palette -> String -> String
largeFont pal font = dzenFontScaled pal font 22

-- | Get the rectangle of the currently focused screen.
getScreenRect :: X Rectangle
getScreenRect = screenRect . W.screenDetail . W.current <$> gets windowset

-- | Show msg in a dzen popup in the middle of the screen, in the colors
-- (fg, bg) of the palette with the specified font: bg_0 text on an accent reads in both modes.
showMessageWithFont :: (Real a) => String -> a -> (String, String) -> String -> X ()
showMessageWithFont font timeout (fgName, bgName) msg = do
  pal <- io readPalette
  let (fg, bg) = (col pal fgName, col pal bgName)
  sr <- getScreenRect
  let rx p = fromIntegral $ p sr
      sw = rx rect_width
      sh = rx rect_height
      sx = rx rect_x
      sy = rx rect_y
      fontSize = fromIntegral (largeFontSize pal) :: Double
      charW = fontSize * 0.52
      msgLines = lines msg
      maxLineLen = case msgLines of
        [] -> 0
        ls -> maximum (map length ls)
      hPadding = fromIntegral (scaleDpi pal 50)
      textW = fromIntegral maxLineLen * charW + hPadding
      minW = fromIntegral (scaleDpi pal 400)
      defaultW = max (sw/3) (min sw minW)
      maxW = max 0 (sw - hPadding)
      w = min maxW (max defaultW textW) :: Double
      x = sx + sw/2 - w/2
      baseH = fromIntegral (scaleDpi pal 65)
      h = min sh baseH :: Double
      y = sy + sh/2 - h/2
  DZ.dzenConfig
    (DZ.timeout (toRational timeout)
     >=> DZ.addArgs ["-x", show x, "-w", show w]
     >=> DZ.addArgs ["-y", show y, "-h", show h]
     >=> DZ.font (largeFont pal font)
     >=> DZ.addArgs ["-fg", fg]
     >=> DZ.addArgs ["-bg", bg]
    ) msg

-- | Default showMessage using "PragmataPro"
showMessage :: (Real a) => a -> (String, String) -> String -> X ()
showMessage = showMessageWithFont "PragmataPro"
