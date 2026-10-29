{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.ShowMessage
-- Description :  Transient screen overlay popups using Cairo and Pango
-- Maintainer  :  thomasf@jossystem.se
-- Stability   :  unstable
-- Portability :  unportable
--
-- Displays transient overlay message popups on the screen (such as
-- workspace names, layout switches, or status notifications) using
-- direct X11 windows with Cairo/Pango rendering.
-----------------------------------------------------------------------------

module XMonad.Util.ShowMessage
    ( showMessage
    , showMessageWithFont
    , showMessagePango
    , showMessageWithFontPango
    , popupRectangle
    , popupMaxTextWidth
    , measureText
    , measureTextWithWrap
    ) where

import           Control.Concurrent (threadDelay)
import           Control.Exception (catch, SomeException)
import           Control.Monad (forM_, void)
import           Data.IORef
import qualified Data.Map as M
import           Foreign.C.Types (CInt(..))
import           Foreign.Ptr (Ptr)
import           Graphics.X11.Xlib
import           Graphics.X11.Xlib.Types (Display(..), Visual(..))
import           Graphics.Rendering.Cairo
import           Graphics.Rendering.Cairo.Internal (mkSurface, manageSurface)
import qualified Graphics.Rendering.Cairo.Types as CairoTypes
import           Graphics.Rendering.Pango hiding (Rectangle)
import           Graphics.Rendering.Pango.Cairo
import           System.IO.Unsafe (unsafePerformIO)
import           XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ThemePalette as TP
import           XMonad.Util.XUtils (createNewWindow, showWindow)

-- | Active popups tracked per screen id so multiple monitors can display
-- their respective messages simultaneously.
{-# NOINLINE popupMapRef #-}
popupMapRef :: IORef (M.Map ScreenId Window)
popupMapRef = unsafePerformIO (newIORef M.empty)

-- | Direct C FFI binding to cairo_xlib_surface_create
foreign import ccall "cairo_xlib_surface_create"
  c_cairo_xlib_surface_create :: Display -> Drawable -> Visual -> CInt -> CInt -> IO (Ptr CairoTypes.Surface)

createXlibSurface :: Display -> Drawable -> Visual -> Int -> Int -> IO CairoTypes.Surface
createXlibSurface dpy drw vis w h = do
  sPtr <- c_cairo_xlib_surface_create dpy drw vis (fromIntegral w) (fromIntegral h)
  s <- mkSurface sPtr
  manageSurface s
  return s

-- | Parse hex string into Cairo RGB components in [0.0, 1.0]
hexToRGB :: String -> (Double, Double, Double)
hexToRGB ('#':r1:r2:g1:g2:b1:b2:_) =
  (fromIntegral (readHex2 r1 r2) / 255.0,
   fromIntegral (readHex2 g1 g2) / 255.0,
   fromIntegral (readHex2 b1 b2) / 255.0)
  where
    readHex2 a b = hexVal a * 16 + hexVal b
    hexVal c
      | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
      | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
      | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
      | otherwise = 0
hexToRGB _ = (1.0, 1.0, 1.0)

-- | Get the screen id and rectangle of the currently focused screen.
getScreenIdAndRect :: X (ScreenId, Rectangle)
getScreenIdAndRect = do
  cur <- gets (W.current . windowset)
  return (W.screen cur, screenRect (W.screenDetail cur))

-- | Configure a Pango layout with font, size, markup/text, and optional width wrapping.
setupLayout :: String -> Int -> Maybe Double -> String -> Render (PangoLayout, Double, Double, Bool)
setupLayout font fontSize maybeMaxW msg = do
  layout <- createLayout ""
  liftIO $ do
    fd <- fontDescriptionFromString (font ++ " " ++ show fontSize ++ "px")
    layoutSetFontDescription layout (Just fd)
    layoutSetAlignment layout AlignCenter
    catch (void (layoutSetMarkup layout msg :: IO String))
          (\(_ :: SomeException) -> layoutSetText layout msg)
    (PangoRectangle _ _ uw uh, _) <- layoutGetExtents layout
    case maybeMaxW of
      Just maxW | uw > maxW -> do
        layoutSetWrap layout WrapPartialWords
        layoutSetWidth layout (Just maxW)
        (PangoRectangle _ _ w h, _) <- layoutGetExtents layout
        return (layout, w, h, True)
      _ -> return (layout, uw, uh, False)

-- | Measure the exact pixel dimensions (width, height, wasWrapped) of text or Pango markup,
-- optionally wrapping if it overflows a maximum allowed width.
measureTextWithWrap :: String -> Int -> Maybe Double -> String -> IO (Double, Double, Bool)
measureTextWithWrap font fontSize maybeMaxW msg =
  withImageSurface FormatARGB32 1 1 $ \surf ->
    renderWith surf $ do
      (_, w, h, wrapped) <- setupLayout font fontSize maybeMaxW msg
      return (w, h, wrapped)

-- | Measure the exact pixel dimensions (width, height) of text or Pango markup unconstrained.
measureText :: String -> Int -> String -> IO (Double, Double)
measureText font fontSize msg = do
  (w, h, _) <- measureTextWithWrap font fontSize Nothing msg
  return (w, h)

-- | Maximum allowed text width before auto line breaking occurs.
popupMaxTextWidth :: TP.Palette -> Rectangle -> Double
popupMaxTextWidth pal sr = max 0 (maxW - hPadding)
  where
    sw       = fromIntegral (rect_width sr)
    hPadding = fromIntegral (TP.scaleDpi pal 50)
    maxW     = max 0 (sw - hPadding)

-- | Compute the centered popup rectangle from screen dimensions,
-- DPI-scaled font metrics, and exact text extents (textW, textH).
popupRectangle :: TP.Palette -> Rectangle -> (Double, Double) -> Rectangle
popupRectangle pal sr (textW, textH) = Rectangle ix iy (fromIntegral iw) (fromIntegral ih)
  where
    rx p       = fromIntegral $ p sr
    sw         = rx rect_width
    sh         = rx rect_height
    sx         = rx rect_x
    sy         = rx rect_y
    hPadding   = fromIntegral (TP.scaleDpi pal 50)
    vPadding   = fromIntegral (TP.scaleDpi pal 24)
    minW       = fromIntegral (TP.scaleDpi pal 400)
    defaultW   = max (sw / 3) (min sw minW)
    maxW       = max 0 (sw - hPadding)
    w          = min maxW (max defaultW (textW + hPadding)) :: Double
    x          = sx + sw / 2 - w / 2
    baseH      = fromIntegral (TP.scaleDpi pal 65)
    desiredH   = max baseH (textH + vPadding)
    h          = min sh desiredH :: Double
    y          = sy + sh / 2 - h / 2
    ix         = round x
    iy         = round y
    iw         = max 1 (round w)
    ih         = max 1 (round h)

-- | Show msg using a direct X11 window with Cairo/Pango rendering.
showMessageWithFont :: (Real a) => String -> a -> (String, String) -> String -> X ()
showMessageWithFont font timeout (fgName, bgName) msg = do
  pal <- io TP.readPalette
  (sid, sr) <- getScreenIdAndRect
  let (fgHex, bgHex) = (TP.col pal fgName, TP.col pal bgName)
      (fgR, fgG, fgB) = hexToRGB fgHex
      (bgR, bgG, bgB) = hexToRGB bgHex
      fontSize = TP.largeFontSize pal
      maxTextW = popupMaxTextWidth pal sr
  (textW, textH, isWrapped) <- io $ measureTextWithWrap font fontSize (Just maxTextW) msg
  let rect@(Rectangle _ _ iw ih) = popupRectangle pal sr (textW, textH)
  dpy <- asks display
  let vis = defaultVisual dpy (defaultScreen dpy)
  win <- createNewWindow rect Nothing bgHex True
  io $ do
    pix <- createPixmap dpy win iw ih (defaultDepth dpy (defaultScreen dpy))
    surf <- createXlibSurface dpy pix vis (fromIntegral iw) (fromIntegral ih)
    renderWith surf $ do
      setSourceRGB bgR bgG bgB
      paint
      (layout, _, _, _) <- setupLayout font fontSize (Just maxTextW) msg
      setSourceRGB fgR fgG fgB
      let layoutW = if isWrapped then maxTextW else textW
      moveTo ((fromIntegral iw - layoutW) / 2) ((fromIntegral ih - textH) / 2)
      showLayout layout
    surfaceFinish surf
    setWindowBackgroundPixmap dpy win pix
    clearWindow dpy win
    freePixmap dpy pix
  showWindow win
  io $ sync dpy False
  oldWin <- io $ atomicModifyIORef popupMapRef $ \m ->
    (M.insert sid win m, M.lookup sid m)
  io $ forM_ oldWin $ \w -> do
    catch (destroyWindow dpy w) (\(_ :: SomeException) -> return ())
    sync dpy False
  void . xfork $ do
    threadDelay (round (toRational timeout * 1000000))
    catch (do
      d <- openDisplay ""
      unmapWindow d win
      destroyWindow d win
      sync d False
      closeDisplay d)
      (\(_ :: SomeException) -> return ())

-- | Aliases for compatibility
showMessageWithFontPango :: (Real a) => String -> a -> (String, String) -> String -> X ()
showMessageWithFontPango = showMessageWithFont

showMessagePango :: (Real a) => a -> (String, String) -> String -> X ()
showMessagePango = showMessage

-- | Default showMessage using "PragmataPro"
showMessage :: (Real a) => a -> (String, String) -> String -> X ()
showMessage = showMessageWithFont "PragmataPro"
