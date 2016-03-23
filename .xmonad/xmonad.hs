import XMonad

import qualified Data.Map as M
import Data.Bits ((.|.))

import XMonad.Layout.Grid
import XMonad.Config.Gnome
import XMonad.Layout.TwoPane
import XMonad.Layout.LayoutScreens
import XMonad.Config.Desktop (desktopLayoutModifiers)
import XMonad.Hooks.ManageHelpers (isFullscreen,doFullFloat)
import XMonad.Hooks.ICCCMFocus
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.ManageDocks


-- Import statements
-- import XMonad
-- import Control.Monad
-- import qualified XMonad.StackSet as W
 
-- Define the names of all workspaces
-- myWorkspaces = ["main","web","chat","media","browse","dev","mail"]
--  
-- -- Define the workspace an application has to go to
-- myManageHook = composeAll . concat $
--             [ -- Applications that go to web
--               [ className =? b --> viewShift "web" | b <- myClassWebShifts ]
--               -- Applications that go to chat
--             , [ resource =? c --> doF (W.shift "chat") | c <- myClassChatShifts ]
--             ]
--             where
--               viewShift = doF . liftM2 (.) W.greedyView W.shift
--               myClassWebShifts  = ["google-chrome"]
--               myClassChatShifts = ["empathy"]
--  
-- -- Run XMonad
-- main = do
--     xmonad $ defaultConfig {
--         workspaces = myWorkspaces
--           , manageHook = myManageHook
--     }
-- 

main = do
xmonad $ gnomeConfig
        { modMask = mod4Mask
        , focusedBorderColor = "grey"
        , normalBorderColor  = "black"
        , borderWidth = 1
        , logHook = takeTopFocus
        , layoutHook = desktopLayoutModifiers ( TwoPane (3/100) (1/2)  ||| Full ||| Grid )
        , focusFollowsMouse = True
        , manageHook = composeAll
             [ manageDocks
             , manageHook gnomeConfig
             , className =? "Unity-2d-panel"    --> doIgnore
             , className =? "Unity-2d-launcher" --> doIgnore
             , className =? "emulator-arm" --> doFloat
             , className =? "emulator64-arm" --> doFloat
             , appName =? "Hangouts" --> doFloat
             , className =? "Cssh" --> doFloat
             , isFullscreen --> doFullFloat ]
        , terminal = "gnome-terminal"
        , keys     = \c -> philKeys `M.union` keys gnomeConfig c
        }
  where
    philKeys = M.fromList $
              [ ((mod4Mask , xK_g) , spawn "browser") 
              , ((mod4Mask , xK_p) , spawn "gmrun")]
