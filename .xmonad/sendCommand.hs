 import Graphics.X11.Xlib
 import Graphics.X11.Xlib.Extras
 import System.Environment
 import Data.Char

 usage :: String -> String
 usage n = "Usage: " ++ n ++ " command number\nSend a command number to a running instance of XMonad"

 main :: IO ()
 main = do
   args <- getArgs
   pn <- getProgName
   let com = case args of
               [] -> error $ usage pn
               w -> (w !! 0)
   sendCommand com

 sendCommand :: String -> IO ()
 sendCommand s = do
   d   <- openDisplay ""
   rw  <- rootWindow d $ defaultScreen d
   a <- internAtom d "XMONAD_COMMAND" False
   allocaXEvent $ \e -> do
                   setEventType e clientMessage
                   setClientMessageEvent e rw a 32 (fromIntegral (read s)) currentTime
                   sendEvent d rw False structureNotifyMask e
                   sync d False