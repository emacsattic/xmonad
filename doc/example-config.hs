--
-- An example configuration.
--



import XMonad
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.Monitor

-- Not required.
-- The layouts used in this example; yours likely are very different.
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

-- Not required.
-- Allows defining keybindings Emacs-like among other things.
import XMonad.Util.EZConfig

-- Not required.
-- Used to define myNumberBindings in this example.
import qualified XMonad.StackSet as W



-- Reserve space for the dedicated minibuffer frame.
emacsMinibuffer = monitor
    { name       = "emacsMinibuffer"
    , prop       = Title "*Minibuf-0*"
    , persistent = True
    }

-- Reserve space for the dedicated completion frame.
emacsCompletion = monitor
    { name       = "emacsCompletion"
    , prop       = Title "*Completions*"
    , persistent = True
    }

-- The window named emacsFlushState is created and deleted right away by
-- xmonad.el to cause Xmonad to make use of space previously reserved by a
-- strut window.  This hopefully won't be required in futur releases.
-- This kludge causes least problems when the tempory window is floating.
manageFloats = composeAll
    [ title =? "emacsFlushState" --> doFloat ]

-- avoidStruts from ManageDocks is also required.
myLayoutHook = avoidStruts $ myLayouts

myManageHook = composeAll
    [ manageFloats
    , manageMonitor emacsMinibuffer
    , manageMonitor emacsCompletion
    ]

main = xmonad $ myConfig

myConfig = defaultConfig
    { layoutHook = myLayoutHook
    , manageHook = myManageHook

--  .  ------------------------------------------------------------------
-- /|\
--  |  Not much you have to / can change in your configuration that would
--  |  make it differ from this example (Except for the optional imports).
--
-------------------------------------------------------------------------
--
--  |  Your will likely have to adjust this heavily to suit your needs.
--  |  These are just examples not suitable for production.
-- \|/
--  '  ------------------------------------------------------------------


    , modMask    = myModMask
    , keys       = \c -> mkKeymap c myKeyBindings -- or use traditional way
    }
    `additionalKeys`  myNumberBindings -- if defined below
    `additionalKeysP` myInputEvents    -- if defined below

-- The default modMask is mod1Mask, that is Meta/Alt.  Since that modifier
-- is heavily used in Emacs we need to use something else.  I use mod4Mask
-- which on my system is Hyper.
myModMask = mod4Mask

-- Instead of creating your key bindings from scratch like in this
-- example you could also just extend the defaults.
--
-- Here I use the Emacs-like way to define key bindings provided by
-- XMonad.Util.EZConfig.
--
-- Note that "M-" does not necessarly stand for "Meta-" but what ever
-- modifier you have set (myModMask =).
myKeyBindings =
  [ -- At least this should be added to your bindings.  It allows you
    -- to focus the dedicated minibuffer frame, if it ever loses focus.
    -- This does happen even when you do not use the emonad script.
    -- Using the mouse to focus the minibuffer frame does not necessarly
    -- work.
    ("M-x", spawn "emonad")
    -- Some more emonad examples, see the emonad script itself for
    -- better description.  These are totally optional.  The key
    -- bindings in this example are fairly random.
  , ("M-e", spawn "emonad -e") -- run execute-extended-command interactively
  , ("M-f", spawn "emonad -f") -- run find-file-other-frame interactively
  , ("M-m", spawn "emonad (message \"hello\")") -- run ... you guessed it
    -- Go back to my regular configuration after testing this one and
    -- putting my real xmonad.hs in place again.  If you just extend
    -- the default configuration then you don't need to define this.
  , ("M-q", spawn "xmonad --recompile; xmonad --restart")
  ]

-- Not required.
-- However if you define your keybindings from scratch like in this example
-- you likely want it.
myNumberBindings =
  [((m .|. mod4Mask, k), windows $ f i)
       | (i, k) <- zip (XMonad.workspaces myConfig) [xK_1 .. xK_9]
       , (f, m) <- [(W.greedyView, 0), (W.shift, controlMask)]]

-- Not required.
-- Allows sending events from external applications to Xmonad using fake
-- key presses.  While xmonad.el currently does not make use of this it
-- likely will in the futur.
--
-- Use it like: xdotool key F1
--
-- In practice you probably want to use keys not actually available on your
-- keyboard.
myInputEvents =
  [ ("<F1>", spawn "xmessage \"I can here you.\"") ]

-- Your myLayouts probably differs a lot, this one helps me testing.
myLayouts = columns
  where
     columns = ThreeCol nmaster delta ratio
     nmaster = 1
     delta   = 3/100
     ratio   = 1/3
