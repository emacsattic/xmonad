;;
;; An example configuration.
;;

;; This example assumes a monitor resolution of 2560x1600.  The minibuffer
;; frame is places at the bottom and the and the completion frame at the
;; left.  At the top some space is reserved for dzen.  I also still use
;; dmenu which covers parts of the minibuffer frame when active.

;; While it might be possible to write a function to calculate these values
;; you would likely still have to adjust it to accommodate for other pagers
;; you have started and/or specific Xmonad layout modifiers you are using.
;; Nevertheless if you do write such a function let me know so I can include
;; it in `xmonad.el'.

(add-to-list 'load-path "/path/to/emacs/xmonad/")
(require 'xmonad)
(setq xmo-minibuffer-frame-alist
      '((left               .    0)   ; pixels
	(top                . 1570)   ; pixels
	(width              .  282)   ; characters
	(height             .    2)   ; lines
	(strut-bottom       .   30)   ; pixels
	(strut-bottom-end-x . 1599))) ; pixels, do not forget this
(setq xmo-completions-frame-alist
      '((left               .    0)
	(top                .   19)
	(width              .   50)
	(height             .  103)
	(strut-left         .  468)
	(strut-left-end-y   . 1566))) ; do not forget this
(xmonad-mode 1)

;; You can also use Customize to set these variables.  If you do make sure
;; your customizations are loaded before you start Xmonad mode.
