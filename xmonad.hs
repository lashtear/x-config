{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeOperators #-}

module Main where

--import           KbLocks

import qualified Data.List                    as L
import           Data.Map                     (Map)
import qualified Data.Map                     as M
import           Data.Ratio                   ((%))
import           Data.Set                     (Set)
import qualified Data.Set                     as S
import           System.IO
import           XMonad
import           XMonad.Config.Desktop
import           XMonad.Config.Xfce
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FadeInactive
import qualified XMonad.Hooks.InsertPosition  as H
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.Circle
import           XMonad.Layout.Grid
import           XMonad.Layout.IM
import           XMonad.Layout.LayoutModifier (ModifiedLayout)
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Renamed
import qualified XMonad.StackSet              as W
import           XMonad.Util.Run              (spawnPipe)
import           XMonad.Util.SpawnOnce        (spawnOnce)

--import           XMonad.Layout.Reflect
--import           XMonad.Layout.Square

myKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_Return), spawn "exec urxvt")
    , ((modm,               xK_p     ),
       spawn "exec /home/lucca/bin/my_dmenu_run")
    , ((modm .|. shiftMask, xK_p     ),
       spawn "exec /home/lucca/bin/my_dmenu_run -s")
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_z     ), spawn "exec dm-tool lock")
    , ((modm,               xK_n     ), refresh)
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm,               xK_m     ), windows W.focusMaster  )
    , ((modm,               xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    , ((modm,               xK_comma ), sendMessage (IncMasterN 1))
    , ((modm,               xK_period), sendMessage (IncMasterN (-1)))
    , ((modm,               xK_b     ), sendMessage ToggleStruts)
    , ((modm .|. shiftMask, xK_F4    ), --spawn "exec xfce4-session-logout")
        spawn "killall xmonad-x86_64-linux")
    , ((modm,               xK_F4    ),
       spawn "xmonad --recompile && killall xmobar && xmonad --restart")
    , ((modm,               xK_F12   ),
       spawn "exec xfce4-screenshooter -f -s ~/Downloads")
    , ((modm,               xK_F1    ),
       spawn "exec xmodmap ~/.Xmodmap")
    , ((modm,               xK_F2    ),
       spawn "exec xdotool key Caps_Lock")

    ]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) mobaKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  where
    mobaKeys :: [KeySym]
    mobaKeys =
      [xK_1 .. xK_4]
      ++ [xK_q, xK_w, xK_e, xK_r]
      ++ [xK_a, xK_s, xK_d, xK_f]

-- | Move the window out of the floating layer
doSink :: ManageHook
--doSink = ask >>= doF . W.sink
doSink = ask >>= \w -> liftX (reveal w) >> doF (W.sink w)

doFocusFullFloat :: ManageHook
doFocusFullFloat = composeAll [doFullFloat, H.insertPosition H.Above H.Newer]

--doFocus :: ManageHook
--doFocus = (ask >>= (\w -> doF $ W.focusWindow w >> W.greedyView w))

-- discover these using xprop
myManageHook :: ManageHook
myManageHook =
  manageHook xfceConfig
  <+> composeOne
  [ checkDock
    -?> manageDocks

  , className =? "Pidgin"
    <||> (className =? "Steam" <&&> (title =? "Friends"))
    <||> className =? "crx_nckgahadagoaajjgafhacjanaoiihapd" -- hangouts
    <||> className =? "Mumble"
    -?> doShift "Comm"

  , (className =? "Steam" <&&> (title `endsWith` " - Chat"))
    -?> doShift "Comm"

  , className =? "Steam"
    <&&> title =? "Steam"
    -?> doShift "Steam"

  , role =? "browser"
    -?> doShift "Web"

  , className =? "Emacs"
    -?> doShift "Edit" <+> (doF $ W.greedyView "Edit")

  , className =? "Sylpheed"
    -?> doShift "Mail"

  , role =? "gimp-toolbox"
    <||> role =? "gimp-dock"
    <||> role =? "gimp-image-window"
    -?> composeAll [doShift "Graph", doSink, doF $ W.greedyView "Graph"]

  , className =? "Bitcoin-Qt"
    -?> doShift "Graph"

  , title =? "xfce4-notifyd"
    -?> doIgnore

  , className =? "xbmc.bin"
    <||> className =? "Clementine"
    -?> doShift "Media"

  , role =? "vlc-main"
    -?> doShift "Media" <+> doFullFloat

  , className =? "MPlayer"
    -?> doFloat

  , className =? "Geeqie"
    <&&> role =? "fullscreen"
    -?> doFocusFullFloat

  , className =? "Hexchat"
    -?> doShift "IRC"
  ]
  where
    role = stringProperty "WM_WINDOW_ROLE"
    --windowName = stringProperty "WM_NAME"
    endsWith :: Eq a => Query [a] -> [a] -> Query Bool
    endsWith q x = fmap (x `L.isSuffixOf`) q

layout :: ModifiedLayout
                  AvoidStruts
                  (ModifiedLayout
                     SmartBorder
                     (PerWorkspace
                        (ModifiedLayout
                           Rename
                           (ModifiedLayout
                              AddRoster
                              (ModifiedLayout
                                 AddRoster (ModifiedLayout AddRoster (Choose Grid Circle)))))
                        (Choose
                           (ModifiedLayout Rename Tall)
                           (ModifiedLayout Rename (Mirror (ModifiedLayout Rename Tall))))))
                  Window

layout = desktopLayoutModifiers
         $ smartBorders
         $ onWorkspace "Comm" commLayout
--         $ onWorkspace "Graph" (vTile ||| gimpLayout ||| fontForgeLayout)
         $ vTile ||| hTile --  ||| Circle ||| Grid ||| Full ||| Square
  where
    vTile = renamed [Replace "VTile"]
            $ Tall 1 (3/100) (2/3)
    hTile = renamed [Replace "HTile"]
            $ Mirror vTile
    commLayout = renamed [Replace "Comm"]
                 $ withIM (1%8) (Role "buddy_list")
                 -- this one doesn't work since steam re-did their app
                 $ withIM (1%7) (ClassName "Steam" `And` Title "Friends")
                 $ withIM (1%6) (Title "Hangouts")
                 $ Grid ||| Circle
                 --these work fine, but I use those tools less often
{-    gimpLayout = renamed [Replace "GIMP"]
                 $ withIM (1%8) (Role "gimp-toolbox")
                 $ reflectHoriz $ withIM (1%7) (Role "gimp-dock")
                 Grid
    fontForgeLayout = renamed [Replace "FontF"]
                      $ withIM (1%32) (ClassName "fontforge"
                                       `And` Title "Tools")
                      $ Mirror
                      $ withIM (1%8) (ClassName "fontforge"
                                       `And` Title "Layers")
                      hTile
-}

abbreviateWidth :: Int -> String -> String
abbreviateWidth n s | length s <= n = s
abbreviateWidth n s =
  let noVowels = dropVowels s in
  if length s <= n then s
  else take n noVowels
  where
    dropVowels :: String -> String
    dropVowels = filter notVowel
    vowelSet :: Set Char
    vowelSet = S.fromList "aeiou"
    notVowel :: Char -> Bool
    notVowel = flip S.notMember vowelSet

fnn :: Int -> String -> String
fnn n = wrap ("<fn=" ++ show n ++ ">") "</fn>"

abbreviateNameWidth :: Int -> String -> String
abbreviateNameWidth n s =
  case s of
    "Steam"  -> fnn 2 "\xF1B6"
    "Mail"   -> fnn 2 "\xF003"
    "Web"    -> fnn 2 "\xF268"
    "Edit"   -> fnn 2 "\xF044"
    "Media"  -> fnn 2 "\xF001"
    "Graph"  -> fnn 2 "\xF15a" --"\xF03E"
    "Comm"   -> fnn 2 "\xF086"
    "IRC"    -> fnn 2 "\xF292"
    "1"      -> fnn 2 "\xF013"
    "2"      -> fnn 2 "\xF09B"
    "5"      -> fnn 2 "\xF120"
    "6"      -> fnn 2 "\xF233"
    "VTile"  -> fnn 3 "◧"
    "Full"   -> fnn 3 "□"
    "Circle" -> fnn 3 "○"
    "HTile"  -> fnn 3 "⬒"
    "Grid"   -> fnn 3 "┼"
    "Gimp"   -> fnn 2 "\xF03E"
    "FontF"  -> fnn 3 "Ff"
    _        -> abbreviateWidth n s

logXMobar :: Handle -> X ()
logXMobar mobar = dynamicLogWithPP xmobarPP
    { ppOutput = hPutStrLn mobar
    , ppTitle = (fnn 1) . (mobf "#95e454")
    , ppHiddenNoWindows = mobf "#483d8b"
                          . abbreviateNameWidth 2
    , ppCurrent = mobf "#95e454"
                  . abbreviateNameWidth 32
    , ppHidden = abbreviateNameWidth 4
    , ppUrgent = mobfb "black" "red"
    , ppSep = wrap " " " " $ mobf "#000000" "│"
    , ppLayout = (abbreviateNameWidth 8)
    }
  where
    mobf = flip xmobarColor ""
    mobfb = xmobarColor

spawns :: [String]
spawns = ["google-chrome", "emacs", "sylpheed", "pidgin", "hexchat", "clementine", "bitcoin-qt", "steam"]

spawnOnceExec :: String -> X ()
spawnOnceExec = spawnOnce . (++) "exec "

runSpawns :: X ()
runSpawns = mapM_ spawnOnceExec spawns

main :: IO ()
main = do
  topbar <- spawnPipe "exec $HOME/.local/bin/xmobar $HOME/.xmobarrc"
  spawn "exec $HOME/.local/bin/xmobar $HOME/.xmobarrc2"
  xmonad $ ewmh xfceConfig
           { modMask = mod4Mask
           , keys = myKeys
           , manageHook = myManageHook
           , logHook = logXMobar topbar
                       <+> (fadeInactiveLogHook (3%5))
                       <+> logHook xfceConfig
           , layoutHook = layout
           , handleEventHook = handleEventHook xfceConfig
                               <+> docksEventHook
                               <+> fullscreenEventHook
           , normalBorderColor = "#483d8b"
           , focusedBorderColor = "#95e454"
           , workspaces = ["1", "2", "Web", "Edit",
                           "5", "6", "Mail", "Comm",
                           "Steam", "Media", "Graph", "IRC"]
           , startupHook = startupHook xfceConfig <+>
                           runSpawns <+>
                           setWMName "LG3D" -- needed because java is crap
           }
