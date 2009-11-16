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
-- The xmonad part of xmonad-emacs.  This file only describes how to
-- configure Xmonad.  Do that first then configure Emacs as described at
-- <http://github.com/tarsius/xmonad-emacs/blob/master/xmonad.el>.
--
-- For an introduction to xmonad-emacs see
-- <http://tarsius.github.com/xmonad-emacs/>.
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

emacsManageHook :: ManageHook
emacsManageHook = composeAll
    [ manageMonitor emacsMinibufferMonitor
    , manageMonitor emacsCompletionMonitor
    , title =? "emacsFlushState"       --> doFloat
    , title =? "emacsCompletionsFloat" --> doFloat
    , title =? "emacsCompletionsFloat" --> doF W.focusDown
    , title =? "Ediff"                 --> doFloat
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
