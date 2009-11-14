-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Emacs
-- Copyright   :  Jonas Bernoulli <jonas@bernoul.li>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Jonas Bernoulli <jonas@bernoul.li>
-- Stability   :  unstable
-- Portability :  unportable
--
-- TODO
--
-----------------------------------------------------------------------------

module XMonad.Util.Emacs (
    -- * Usage
    -- $usage
    emacsManageHook
  ) where

import XMonad
import XMonad.Layout.Monitor

import qualified XMonad.StackSet as W

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Util.Emacs
-- > import XMonad.Hooks.EwmhDesktop
-- >
-- > myConfig = ewmh defaultConfig
-- >     { layoutHook = avoidStruts $ myLayout
-- >     , manageHook = myManageHook <+> emacsManageHook
-- >     }
--
-- After you have done this you also have to configure Emacs as described
-- in xmonad.el.

-- | Adding this to manageHook causes the Completion and Minibuffer frames
--   to behave as configured in Emacs.
--
emacsManageHook :: ManageHook
emacsManageHook = composeAll
    [ manageMonitor emacsMinibufferMonitor
    , manageMonitor emacsCompletionMonitor
    , title =? "emacsFlushState"       --> doFloat
    , title =? "emacsCompletionsFloat" --> doFloat
    , title =? "emacsCompletionsFloat" --> doF W.focusDown
    ]

emacsMinibufferMonitor :: Monitor a
emacsMinibufferMonitor = monitor
    { name       = "emacsMinibuffer"
    , prop       = Title "emacsMinibuffer"
    , persistent = True
    }

emacsCompletionMonitor :: Monitor a
emacsCompletionMonitor = monitor
    { name       = "emacsCompletionStrut"
    , prop       = Title "emacsCompletionsStrut"
    , persistent = True
    }
