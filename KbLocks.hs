module KbLocks (resetMods) where

-- not used currently!

import           Foreign.C
import           Foreign.Marshal.Error     (throwIf_)
import           Foreign.Ptr
import           Graphics.X11.Xlib.Display
import           Graphics.X11.Xlib.Types
import           System.IO.Unsafe


-- /usr/include/X11/extensions/XKB.h
-- #define XkbUseCoreKbd           0x0100
-- #define XkbNoModifierMask       0
-- #define XkbAllModifiersMask     0xff

xkbUseCoreKbd :: Int
xkbUseCoreKbd = 0x0

noModifiers :: Int
noModifiers = 0

allModifiers :: Int
allModifiers = 0xff

foreign import ccall unsafe "X11/XKBlib.h XkbLockModifiers"
    kbLockModifiers :: Display -> Int -> Int -> Int -> IO Bool

resetMods :: Display -> IO ()
resetMods disp =
  throwIf_ id (const "XkbLockModifiers") $ do
  xkbInit disp
  kbLockModifiers disp xkbUseCoreKbd allModifiers noModifiers

foreign import ccall unsafe "X11/XKBlib.h XkbQueryExtension"
    kbQueryExtension :: Display -> Int -> Int -> Int -> Int -> Int -> IO Bool

xkbInit :: Display -> IO ()
xkbInit disp =
  throwIf_ id (const "XkbQueryExtension") $
  kbQueryExtension disp 0 0 0 0 0
