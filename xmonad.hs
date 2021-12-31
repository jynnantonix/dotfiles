import XMonad
import XMonad.Actions.CycleWS
import XMonad.Config.Desktop
import XMonad.Config.Mate
import XMonad.Layout.ThreeColumns

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

 ------------------------------------------------------------------------
-- Modify and add some keybindings
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    [
    -- Increment the number of windows in the master area
    -- ((modm                 , xK_w ), sendMessage (IncMasterN 1))

    -- Decrement the number of windows in the master area
    -- , ((modm                 , xK_e), sendMessage (IncMasterN (-1)))

    -- Change the keyboard layout to remap the programmer dvorak layout
    --, ((modm                 , xK_F1), setKeyRemap dvorakProgrammerKeyRemap)

    -- Change the keyboard layout to remap the programmer dvorak layout
    --, ((modm                 , xK_F2), setKeyRemap emptyKeyRemap)

    -- Switch to next workspace.
    ((modm                 , xK_u), nextWS)

    -- Switch to previous workspace.
    , ((modm                 , xK_e), prevWS)

    -- Move to next workspace.
    , ((modm .|. shiftMask   , xK_u), shiftToNext)

    -- Switch to previous workspace.
    , ((modm .|. shiftMask   , xK_e), shiftToPrev)

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    -- , ((modMask .|. shiftMask, xK_r ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]

    ++

    --
    -- mod-{comma,period,slash}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{comma,period,slash}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_comma, xK_period, xK_slash] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

------------------------------------------------------------------------
-- Float some additional applications
--
myManageHook = composeAll
    [ appName =? "klavaro"  --> doFloat
    , appName =? "meld"     --> doFloat
    , appName =? "nautilus" --> doFloat
    , appName =? "vmplayer" --> doFloat
    , appName =? "mate-panel" --> doFloat
    , title =? "alacritty-scrollback" --> doFloat
      -- Derp, hangouts
    , appName   =? "crx_ljclpkphhpbpinifbeabbhlfddcpfdde" --> doFloat]

------------------------------------------------------------------------
-- Put the master window in the center of the screen.
--
myLayoutHook = desktopLayoutModifiers $ ThreeColMid nmaster delta ratio
  where
    -- One master pane in the middle of the screen.
    nmaster = 1

    -- Proportion of screen occupied by master pane.
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes.
    delta   = 3/100

------------------------------------------------------------------------
main = xmonad $ mateConfig
        {
          modMask    = mod4Mask -- use super key as the mod key
        , keys       = myKeys <+> keys mateConfig
        , manageHook = myManageHook <+> manageHook mateConfig
        , layoutHook = myLayoutHook ||| layoutHook mateConfig
        }
