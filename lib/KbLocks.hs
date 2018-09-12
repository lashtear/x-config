{-# LANGUAGE ForeignFunctionInterface #-}

module KbLocks (resetMods) where

import           Control.Monad             (void)
import           Foreign.C                 (CInt, CString)
import           Foreign.Marshal.Error     (throwIfNull)
import           Foreign.Ptr               (Ptr, nullPtr)
import           Graphics.X11.Xlib.Display (closeDisplay)
import           Graphics.X11.Xlib.Types   (Display (..))

xkbOpenDisplay :: IO Display
xkbOpenDisplay = do
  d <- throwIfNull "XkbOpenDisplay" $
    c_kbOpenDisplay nullPtr nullPtr nullPtr nullPtr nullPtr nullPtr
  return $ Display d
foreign import ccall "XkbOpenDisplay"
  c_kbOpenDisplay :: CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO (Ptr Display)

-- /usr/include/X11/extensions/XKB.h
-- #define XkbUseCoreKbd           0x0100
-- #define XkbNoModifierMask       0
-- #define XkbAllModifiersMask     0xff

xkbUseCoreKbd :: Int
xkbUseCoreKbd = 0x0100

noModifiers :: Int
noModifiers = 0

allModifiers :: Int
allModifiers = 0xff

resetMods :: IO ()
resetMods = do
  disp <- xkbOpenDisplay
  void $ c_xkbLockModifiers disp xkbUseCoreKbd allModifiers noModifiers
  closeDisplay disp
foreign import ccall "XkbLockModifiers"
  c_xkbLockModifiers :: Display -> Int -> Int -> Int -> IO Bool


