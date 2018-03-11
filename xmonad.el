;;; xmonad.el -- integrate XMonad and Emacs

;; Copyright (C) 2009  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; MAINTAINER WANTED.  I have stopped using xmonad and switched back to
;; wmii.  Xmonad is a nice window manager but I really like the way
;; windows are arranged in columns in wmii.  Making xmonad behave the
;; same way would have required learning haskell - and I just don't have
;; the time for this at the moment.  If you are interested in taking over
;; this library please contact me - but be aware that this library is not
;; quite usable yet in it's current form.

;; xmonad-emacs tries to create an even more enjoyable environment than
;; what you already get from just using both xmonad and emacs by making
;; the two aware of one another.

;; xmonad-emacs consists of xmonad.el which make emacs aware of xmonad
;; and XMonad.Util.Emacs to make xmonad aware of emacs.  Additionally a
;; shell script wrapper around emacsclient named emonad is provided
;; which allows using emacs as a menu, just like xmobar or dzen.

;; The two most important features of xmonad-emacs are:
;;
;; * Instead of using one minibuffer per frame a dedicated minibuffer
;;   frame is used which is placed at the bottom (or top) of the screen
;;   and is visible on all workspaces.
;;
;; * When completing input in the minibuffer the *Completions* buffer is
;;   shown in a dedicated frame.  This frame can eigher float over other
;;   X windows or is placed at one of the edges of the screen while other
;;   X windows are resized to make room for it.  Once input is confirmed
;;   or aborted the frame is removed.

;; This has at least the following benefits:
;;
;; * Both the minibuffer and completion buffers appear in consistant
;;   places on the screen.
;;
;; * The active buffer doesn't shrink when completing some input.
;;
;; * Last but not least emacs can be used as a menu - from xmonad itself
;;   for example to spawn application but also by other applications like
;;   Uzbl that could benefit from using something a bit more advanced than
;;   Dmenu.

;; `xmonad-mode' can be turned off but this currently fails to restore
;; the previous configuration completely.  Most notably the minibuffer
;; frame is not removed because existing frames can not easily be
;; modified to use their own minibuffer window instead of the default
;; minibuffer frame.

;; For more information see http://tarsius.github.com/xmonad-emacs/.

;;; Configuration:

;; TODO cleanup and extend

;; Emacs has to be configures as follows.  But before you do that you
;; should first configure Xmonad as described at
;; http://http://tarsius.github.com/xmonad-emacs/XMonad-Util-Emacs.html.

;; This example assumes a monitor resolution of 2560x1600.  The minibuffer
;; frame is places at the bottom and the completion frame at the left.

;; (add-to-list 'load-path "/path/to/emacs/xmonad/")
;; (require 'xmonad)
;; (setq xmo-minibuffer-frame-alist
;;       '((left               .    0)   ; pixels
;;         (top                . 1570)   ; pixels
;;         (width              .  282)   ; characters
;;         (height             .    2)   ; lines
;;         (strut-bottom       .   30)   ; pixels
;;         (strut-bottom-end-x . 1599))) ; pixels, do not forget this
;; (setq xmo-completions-frame-alist
;;       '((left               .    0)
;;         (top                .   19)
;;         (width              .   50)
;;         (height             .  103)
;;         (strut-left         .  468)
;;         (strut-left-end-y   . 1566))) ; do not forget this
;; (xmonad-mode 1)

;; ****
;; **** This library REDEFINES `completion--insert-strings'
;; ****             DEFINED IN `minibuffer.el'.
;; ****

;;; Code:

(with-no-warnings (require 'cl)) ; for member*

(defgroup xmobar nil
  "Integrate Xmonad and Emacs."
  :group 'environment
  :group 'X)

(defcustom xmo-dedicated-minibuffer-frame 1
  "In Xmonad mode; Whether to create a default minibuffer frame.
nil Do not create default minibuffer frame.
t   Create default minibuffer frame; other frames use their own minibuffer.
1   Create default minibuffer frame; all frames use the default minibuffer."
  :type '(choice (const nil :tag "Do not create default minibuffer frame")
		 (const t   :tag "Create default minibuffer frame")
		 (const 1   :tag "Use default minibuffer frame for all frames"))
  :group 'xmobar)

(defcustom xmo-dedicated-completions-frame 1
  "In Xmonad mode; Whether to show the *Completions* buffer in a seperate frame."
  :type '(choice (const nil :tag "Do not use a dedicated completion frame")
		 (const t   :tag "Use a dedicated completion frame; as strut")
		 (const 1   :tag "Use a dedicated completion frame; floating"))
  :group 'xmobar)

(defcustom xmo-minibuffer-frame-alist nil
  "In Xmonad mode; Alist of parameters for the default minibuffer frame.
The default minibuffer frame is used by frames that do not have a
minibuffer of their own.  It is created when such a frame is created.

The parameters specified here supersede those given in
`minibuffer-frame-alist' which in turn supersede the values given in
`default-frame-alist'.  When `xmonad-mode' is turned on the parameters
specified here are actually appended to `minibuffer-frame-alist' and
removed when it is turned off.  (A fake parameter `xmo-end' is added
as seperator).

The properties `left', `top', `width' and `height' are used to specify the
geometry of the frame.  Additionally at least two of the strut properties
have to be specified.  See the example configuration distributed with this
library and variable `xmo-strut-properties' for more information.

It is not necessary to include (minibuffer . only); that is added when
the minibuffer frame is created.  But you may include (minibuffer . t)
if you want the dedicated minibuffer frame to have a mode line."
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Parameter")
		       (sexp :tag "Value")))
  :group 'xmobar)

(defcustom xmo-completions-frame-alist nil
  "In Xmonad mode; Alist of parameters for the completion frame."
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Parameter")
		       (sexp :tag "Value")))
  :group 'xmobar)

(defcustom xmo-completions-gravity nil
  "In Xmonad mode; Direction."
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Parameter")
		       (sexp :tag "Value")))
  :group 'xmobar)

(defcustom xmo-xmonad-frame-alist
  '((minibuffer . nil))
  "In Xmonad mode; Alist of Xmonad dependent default frame parameters.
Parameters specified here supersede the values given in the associated
value of the key `x' of the alist `window-system-default-frame-alist'
which in turn supersede the values given in `default-frame-alist'.

Normally this should contain (minibuffer . nil) causing all frames to
have no minibuffer except for the default minibuffer frame whose
minibuffer is used for all other frames instead.  If you want this
behaviour regardless of whether `xmonad-mode' is turned on or not you
should set this in one of the superseded variables mentioned above.

On the other hand if you want each frame to have it's own minibuffer
set this in your init file,

 (setq xmo-window-system-default-frame-alist nil)

If you do this but still want a default minibuffer frame (e.g. for the
benefit of the `emonad' script) you have to create it by calling
`xmo-make-minibuffer-frame' after turning `xmonad-mode' on."
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Parameter")
		       (sexp :tag "Value")))
  :group 'xmobar)

(defvar xmo-completions-frame nil
  "The dedicated completions frame; if it exists.
DO NOT EDIT this variable.")

(defconst xmo-strut-properties
  '(strut-left strut-right strut-top strut-bottom
    strut-left-start-y   strut-left-end-y
    strut-right-start-y  strut-right-end-y
    strut-top-start-x    strut-top-end-x
    strut-bottom-start-x strut-bottom-end-x)
  "Ordered list of values in the _NET_WM_STRUT_PARTIAL property.
See http://standards.freedesktop.org/wm-spec/1.3/ar01s05.html#id2523368
DO NOT EDIT this variable.")

(define-minor-mode xmonad-mode
  "Integrate Xmonad and Emacs.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When Xmonad mode is enabled, Xmonad and Emacs are better integrated.
The following features are available:

* Instead of one minibuffer per frame a dedicated minibuffer frame
  is used which is placed at the bottom (or top) of the screen and
  is visible on all workspaces.

* When completing input in the minibuffer the *Completions* buffer is
  shown in a dedicated frame placed at one of the edges of the screen.
  Other X windows are resized to make room for that X window.  Once
  input is confirmed or aborted the frame is removed.

* `emonad', a bash script wrapper around emacsclient, allows using
  Emacs for purposes utilities like dzen, xmonad and dmenu are
  normally used for."
  :global t
  (cond (xmonad-mode
	 ;; Minibuffer Frame.
	 (when xmo-dedicated-minibuffer-frame
	   (add-hook 'minibuffer-exit-hook 'xmo-delete-completions)
	   (put  'make-initial-minibuffer-frame 'nonxmo-function
		 (symbol-function 'make-initial-minibuffer-frame))
	   (fset 'make-initial-minibuffer-frame
		 (symbol-function 'xmo-make-minibuffer-frame))
	   (unless (assq 'xmo-end minibuffer-frame-alist)
	     (setq minibuffer-frame-alist
		   (nconc (cons (cons 'title "emacsMinibuffer")
				(copy-alist xmo-minibuffer-frame-alist))
			  (list (cons 'minibuffer 'only)
				(list 'xmo-end))
			  minibuffer-frame-alist)))
	   (put 'minibuffer-auto-raise 'noxmo-value
		 minibuffer-auto-raise)
	   (setq minibuffer-auto-raise t))
	 ;; Completions Frame.
	 (when xmo-dedicated-completions-frame
	   (add-to-list
	    'special-display-buffer-names
	    (list "*Completions*"
		  'xmo-display-completions
		  (cons (cons 'title
			      (if (eq xmo-dedicated-completions-frame t)
				  "emacsCompletionsStrut"
				"emacsCompletionsFloat"))
			xmo-completions-frame-alist))))
	 ;; Other Frames.
	 (let* ((ass (assq 'x window-system-default-frame-alist))
	 	(old (cdr ass))
	 	(new (nconc (copy-alist xmo-xmonad-frame-alist)
	 		    (cons (list 'xmo-end)
				  old))))
	   (if ass (setcdr ass new)
	     (push (cons 'x new) window-system-default-frame-alist))))
	(t
	 ;; Minibuffer Frame.
	 (remove-hook 'minibuffer-exit-hook 'xmo-delete-completions)
	 (fset 'make-initial-minibuffer-frame
	       (get 'make-initial-minibuffer-frame 'nonxmo-function))
	 (setq minibuffer-frame-alist
	       (with-no-warnings
		 (cdr (member* 'xmo-end minibuffer-frame-alist
			       :key 'car))))
	 (setq minibuffer-auto-raise
	       (get 'minibuffer-auto-raise 'noxmo-value))
	 ;; Completions Frame.
	 (setq special-display-buffer-names
	       (delete "*Completions*" special-display-buffer-names))
	 ;; Other Frames.
	 (setq window-system-default-frame-alist
	       (with-no-warnings
		 (cdr (member* 'xmo-end window-system-default-frame-alist
			       :key 'car))))
	 ;; * FIXME This only has an effect on new frames.
	 (modify-all-frames-parameters '((minibuffer . t)))
	 ;; * FIXME Since there are still minibuffer-less frames the
	 ;; dedicated minibuffer frame can't be deleted.
	 ;; (delete-frame default-minibuffer-frame)
	 )))

(defun xmo-make-minibuffer-frame (&optional display)
  "Create the default minibuffer frame and set the strut property.

When `xmonad-mode' is turned on `make-initial-minibuffer-frame' is
redefined to use the functions stored in `xmo-make-minibuffer-frame's
function cell.  Turning of `xmonad-mode' restores the original
definition as found in `frames.el'.

Normally you do not have to call this function directly it is called
in function `frame-notice-user-settings' during startup.  However if
you have customized option `xmo-default-frame-alist' so that it does
not contain (minibuffer . nil), no minibuffer frame is created at
startup and you can later use this function to create it.

This probably is only useful when you want each frame to have it's
own minibuffer but want to use a default minibuffer when using Emacs
as a menu/pager."
  (unless xmonad-mode
    (error "Xmonad-mode has to be turned on"))
  (let* ((parms (nconc minibuffer-frame-alist
		       '((minibuffer . only))))
	 (frame (if display
		    (make-frame-on-display display parms)
		  (make-frame parms))))
    (xmo-frame-set-strut-partial frame xmo-minibuffer-frame-alist)
    frame))

(defun xmo-display-completions (buffer &optional args)
  (unless (frame-live-p xmo-completions-frame)
    (setq xmo-completions-frame
	  (with-current-buffer buffer
	    (make-frame (append args special-display-frame-alist))))
    (let ((window (frame-selected-window xmo-completions-frame)))
      (xmo-frame-set-strut-partial xmo-completions-frame args)
      (set-window-buffer window buffer)
      (set-window-dedicated-p window t)
      (xmo-refresh)
      (make-frame-visible xmo-completions-frame)
      (raise-frame xmo-completions-frame)))
  (fit-frame-to-buffer xmo-completions-frame)
  (frame-selected-window xmo-completions-frame))

(defun xmo-delete-completions ()
  (when (get-buffer "*Completions*")
    (kill-buffer "*Completions*"))
  (when xmo-completions-frame
    (delete-frame xmo-completions-frame)
    (xmo-refresh)))

(defun xmo-frame-set-strut-partial (frame properties)
  (let (geometry)
    (dolist (prop (reverse xmo-strut-properties))
      (push (or (cdr (assoc prop properties)) 0) geometry))
    (x-change-window-property "_NET_WM_STRUT_PARTIAL" geometry
			      frame "CARDINAL" 32 t)))

;; FIXME Help is very welcome.
(defun xmo-refresh (&optional forcep)
  "Refresh Xmonad's state.

Xmonad does not instantly detect when a strut disappears.  This function
can be used to force Xmonad to detect that such a change happended.

Unfortunately the only way I found to do this involves creating an
X-window and deleting right away.  If you have a better idea let me know.

Unless FORCEP or `xmo-dedicated-completions-frame' is t this function
does nothing."
  ;; * I was hoping that something like this would work:
  ;;   In Xmonad bind 'refresh' to F1 and here
  ;;   (call-process-shell-command "xdotool" nil nil nil "key" "F1")
  ;; * Since that doesn't work create an X window and delete it right
  ;;   away.  This gives Xmonad a change to adapt to a new situation.
  ;; * Unfortunatly some Emacs windows end up not using all the space
  ;;   available in the containing frame, even after creating the
  ;;   tempory X-window.  Using (redraw-display) or a delay does not
  ;;   fix it eigher.
  (when (or forcep (eq xmo-dedicated-completions-frame t))
    (with-temp-buffer
      (delete-frame (make-frame '((title  . "emacsFlushState")
				  (width  . 1)
				  (height . 1)))))))

;; REDEFINE `completion--insert-strings' DEFINED IN `minibuffer.el'.
;;
;; When in Xmonad mode (otherwise behaviour is unchanged):
;;
;; * Instead of using the default of 79 extract width of *Completions*
;;   buffer from `xmo-completions-frame-alist'.
;;
(defun completion--insert-strings (strings)
  "Insert a list of STRINGS into the current buffer.
Uses columns to keep the listing readable but compact.
It also eliminates runs of equal strings."
  (when (consp strings)
    (let* ((length (apply 'max
			  (mapcar (lambda (s)
				    (if (consp s)
					(+ (string-width (car s))
                                           (string-width (cadr s)))
				      (string-width s)))
				  strings)))
	   (window (get-buffer-window (current-buffer) 0))
	   (wwidth (if window
		       (1- (window-width window))
		     (if xmonad-mode
			 (or (cdr (assq 'width xmo-completions-frame-alist)) 79)
		       79)))
	   (columns (min
		     1 ; FIXME
		     ;; At least 2 columns; at least 2 spaces between columns.
		     (max 2 (/ wwidth (+ 2 length)))
		     ;; Don't allocate more columns than we can fill.
		     ;; Windows can't show less than 3 lines anyway.
		     (max 1 (/ (length strings) 2))))
	   (colwidth (/ wwidth columns))
           (column 0)
	   (laststring nil))
      ;; The insertion should be "sensible" no matter what choices were made
      ;; for the parameters above.
      (dolist (str strings)
	(unless (equal laststring str) ; Remove (consecutive) duplicates.
	  (setq laststring str)
          (let ((length (if (consp str)
                            (+ (string-width (car str))
                               (string-width (cadr str)))
                          (string-width str))))
            (unless (bolp)
              (if (< wwidth (+ (max colwidth length) column))
                  ;; No space for `str' at point, move to next line.
                  (progn (insert "\n") (setq column 0))
                (insert " \t")
                ;; Leave the space unpropertized so that in the case we're
                ;; already past the goal column, there is still
                ;; a space displayed.
                (set-text-properties (- (point) 1) (point)
                                     ;; We can't just set tab-width, because
                                     ;; completion-setup-function will kill all
                                     ;; local variables :-(
                                     `(display (space :align-to ,column)))
                nil))
            (if (not (consp str))
                (put-text-property (point) (progn (insert str) (point))
                                   'mouse-face 'highlight)
              (put-text-property (point) (progn (insert (car str)) (point))
                                 'mouse-face 'highlight)
              (add-text-properties (point) (progn (insert (cadr str)) (point))
                                   '(mouse-face nil
                                     face completions-annotations)))
            ;; Next column to align to.
            (setq column (+ column
                            ;; Round up to a whole number of columns.
                            (* colwidth (ceiling length colwidth))))))))))

(provide 'xmonad)
;;; xmonad.el ends here
