--https://gist.github.com/maxbane/9521612#file-xmonad-hs-L43

import XMonad
import XMonad.Util.EZConfig
 
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
 
import XMonad.Layout.NoBorders
import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
-- import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps 
import Data.Monoid
import System.Exit
import XMonad.Util.Run  
import System.IO  
 
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- See https://github.com/davidbrewer/xmonad-ubuntu-conf
 
-- Width of the window border in pixels.
--
myBorderWidth   = 0

--
myWorkspaces = ["web", "code"] ++ map show [3..9]


------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
 
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
 
    -- launch dmenu
    , ((modm,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
 
    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")
 
    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)
 
     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
 
    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)
 
    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )
 
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )
 
    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)
 
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
 
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
 
    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
 
    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)
 
    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
 
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
 
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)
 
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
 
    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++
 
    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
 
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
 


-- The main function.
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig
 
-- Command to launch the bar.
myBar = "xmobar"
 
{-
Xmobar configuration variables. These settings control the appearance
of text which xmonad is sending to xmobar via the DynamicLog hook.
-}
 
myTitleColor = "#eeeeee" -- color of window title
myTitleLength = 80 -- truncate window title to this length
myCurrentWSColor = "#e6744c" -- color of active workspace
myVisibleWSColor = "#c185a7" -- color of inactive workspace
myUrgentWSColor = "#cc0000" -- color of workspace with 'urgent' window
myCurrentWSLeft = "[" -- wrap active workspace with these
myCurrentWSRight = "]"
myVisibleWSLeft = "(" -- wrap inactive workspace with these
myVisibleWSRight = ")"
myUrgentWSLeft = "{" -- wrap urgent workspace with these
myUrgentWSRight = "}"
 
 
-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP {
-- ppCurrent = xmobarColor "#429942" "" . wrap "<" ">"
ppTitle = xmobarColor myTitleColor "" . shorten myTitleLength
, ppCurrent = xmobarColor myCurrentWSColor ""
. wrap myCurrentWSLeft myCurrentWSRight
, ppVisible = xmobarColor myVisibleWSColor ""
. wrap myVisibleWSLeft myVisibleWSRight
, ppUrgent = xmobarColor myUrgentWSColor ""
. wrap myUrgentWSLeft myUrgentWSRight
}
 
-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)
 
 
-- Main configuration, override the defaults to your liking.
myConfig = defaultConfig {
    modMask = mod1Mask,
    --terminal = "xterm -bg black -fg gray -fa \"Ubuntu Mono\" -fs 12 -e '/usr/bin/env -u TMUX tmux -2'",
    terminal = "rxvt",
    borderWidth = myBorderWidth,
    workspaces = myWorkspaces,
    keys = myKeys,
    handleEventHook = fullscreenEventHook,
    -- border colors from solarized dark palette
    --focusedBorderColor = "#268bd2",
    --normalBorderColor = "#586e75",
    layoutHook = smartBorders(avoidStruts(
    ResizableTall 1 (3/100) (1/2) []
    ||| Mirror (ResizableTall 1 (3/100) (1/2) [])
    ||| noBorders Full
    ||| Grid
    ||| ThreeColMid 1 (3/100) (3/4)
    ||| Circle
    ))
     
    -- manageHook = composeAll [
    -- className =? "Gimp" --> doFloat,
    -- className =? "MPlayer" --> doFloat,
    -- isFullscreen --> doFullFloat
    -- ]
}

--`additionalKeys`
--[
--((mod4Mask .|. shiftMask, xK_l), spawn "slock")
--, ((mod4Mask, xK_minus),
--spawn "pactl set-sink-volume bluez_sink.FC_58_FA_01_4F_BD -- -5%")
--, ((mod4Mask, xK_equal),
--spawn "pactl set-sink-volume bluez_sink.FC_58_FA_01_4F_BD -- +5%")
--, ((mod4Mask, xK_0),
--spawn "pactl set-sink-volume bluez_sink.FC_58_FA_01_4F_BD -- 0%")
--]