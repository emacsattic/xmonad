;;; xmonad.el -- integrate Xmonad and Emacs

;; Copyright (C) 2009  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20090621
;; Updated: 20090621
;; Version: 0.1
;; Homepage: https://github.com/tarsius/xmonad.el
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

;; Turning `xmonad-mode' on integrates Xmonad and Emacs, creating an
;; even more enjoyable environment.

;; When `xmonad-mode' is turned on and Xmonad and Emacs have been
;; configure as described below the following features are available:

;; * Instead of one minibuffer per frame a dedicated minibuffer frame
;;   is used which is placed at the bottom (or top) of the screen and
;;   is visible on all workspaces.

;; * When completing input in the minibuffer the *Completions* buffer is
;;   shown in a dedicated frame placed at one of the edges of the screen.
;;   Other X windows are resized to make room for that X window.  Once
;;   input is confirmed or aborted the frame is removed.

;; * `emonad', a bash script wrapper around emacsclient, allows using
;;   Emacs for purposes utilities like dzen, xmonad and dmenu are
;;   normally used for.

;; `xmonad-mode' can be turned off but this currently fails to restore
;; the previous configuration completely.  Most notably the minibuffer
;; frame is not removed because existing frames can not easily be
;; modified to use their own minibuffer window instead of the default
;; minibuffer frame.

;; Example configurations for Xmonad and Emacs can be found in the files
;; `xmonad.hs' and `emacs.el', which are distributed with this library.
;; You have to read those files and adjust your configuration accordingly,
;; before turning `xmonad-mode' on or strange things will happen.

;; Dependencies:
;;
;; * >=Emacs-23     (from cvs,   not tested with older versions)
;; * XMonad         (from darcs, not tested with older versions)
;; * XMonad-Contrib (from darcs)
;; * xdotool        (http://www.semicomplete.com/projects/xdotool/)

;; If you would like to share your configuration or hack on `xmonad.el'
;; please clone the git repository at git://github.com/tarsius/xmonad.el.

;; **** This library REDEFINES `completion--insert-strings'
;; ****             DEFINED IN `completions.el'.

;; Bugs:
;;
;; * After the completions frame is deleted frames sometimes fail to use
;;   all the available space.
;; * Minibuffer frame sometimes loses focus when it shouldn't.
;; * Turning of Xmonad mode fails to add minibuffer windows to existing
;;   frames and remove default minibuffer frame.
;; * `emonad' fails to print anything to stout making it useless as
;;   a replacment for e.g. dmenu.  This is due to a limitation in
;;   `emacsclient' which has been reported upstream.

;;; Code:

(require 'cl) ; for member*

(defgroup xmobar nil
  "Integrate Xmonad and Emacs."
  :group 'environment
  :group 'X)

(defcustom xmo-decicated-minibuffer-frame 1
  "In Xmonad mode; Whether to create a default minibuffer frame.
nil Do not create default minibuffer frame.
t   Create default minibuffer frame; other frames use their own minibuffer.
1   Create default minibuffer frame; all frames use the default minibuffer."
  :type '(choice (const nil :tag "Do not create default minibuffer frame")
		 (const t   :tag "Create default minibuffer frame")
		 (const 1   :tag "Use default minibuffer frame for all frames"))
  :group 'xmobar)

(defcustom xmo-decicated-completions-frame t
  "In Xmonad mode; Whether to show the *Completions* buffer in a seperate frame."
  :type 'boolean
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

(defconst xmo-emonad-minibuffer-focus-delay 0.4)

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
	 (when xmo-decicated-minibuffer-frame
	   (add-hook 'minibuffer-exit-hook 'xmo-delete-completions)
	   (fset 'make-initial-minibuffer-frame
		 (symbol-function 'xmo-make-minibuffer-frame))
	   (unless (assq 'xmo-end minibuffer-frame-alist)
	     (setq minibuffer-frame-alist
		   (nconc (cons (cons 'title "*Minibuf-0*")
				(copy-alist xmo-minibuffer-frame-alist))
			  (list (cons 'minibuffer 'only)
				(list 'xmo-end))
			  minibuffer-frame-alist))))
	 ;; Completions Frame.
	 (when xmo-decicated-completions-frame
	   (add-to-list 'special-display-buffer-names
			(list "*Completions*"
			      'xmo-display-completions
			      (cons (cons 'title "*Completions*")
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
	 (setq minibuffer-frame-alist
	       (cdr (member* 'xmo-end minibuffer-frame-alist
			     :key 'car)))
	 ;; Completions Frame.
	 (setq special-display-buffer-names
	       (delete "*Completions*" special-display-buffer-names))
	 ;; Other Frames.
	 (setq window-system-default-frame-alist
	       (cdr (member* 'xmo-end window-system-default-frame-alist
			     :key 'car)))
	 ;; FIXME This only has an effect on new frames.
	 (modify-all-frames-parameters '((minibuffer . t)))
	 ;; FIXME Since there are still minibuffer-less frames the
	 ;; deficated minibuffer frame can't be deleted.
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
      ;; Kludge.  Xmonad does not notice that the strut disappeared.
      (xmo-refresh)
      (make-frame-visible xmo-completions-frame)
      (raise-frame xmo-completions-frame)))
  (prog1 (frame-selected-window xmo-completions-frame)
    ;; Kludge.  When using the `emonad' script we have to ensure the
    ;; minibuffer frame get's the focus.  A little delay is necessary.
    ;; If the delay is to long then it is noticable.  If it is to short
    ;; then the minibuffer might not get focus sometimes.  Is there a
    ;; better way to ensure focus?
    (when (bound-and-true-p emonad-completions)
      (sleep-for xmo-emonad-minibuffer-focus-delay)
      (select-frame-set-input-focus default-minibuffer-frame))))

(defun xmo-delete-completions ()
  (when (get-buffer "*Completions*")
    (kill-buffer "*Completions*"))
  (when xmo-completions-frame
    (delete-frame xmo-completions-frame)
    ;; Kludge.  Xmonad does not notice that the strut disappeared.
    (xmo-refresh)))

(defun xmo-frame-set-strut-partial (frame properties)
  (let (geometry)
    (dolist (prop (reverse xmo-strut-properties))
      (push (or (cdr (assoc prop properties)) 0) geometry))
    (x-change-window-property "_NET_WM_STRUT_PARTIAL" geometry
			      frame "CARDINAL" 32 t)))

(defun xmo-refresh ()
  "Refresh Xmonad's state." ; FIXME Help is very welcome.

  ;; I was hoping that something like this would work:
  ;; In Xmonad bind 'refresh' to F1 and here
  ;; (call-process-shell-command "xdotool" nil nil nil "key" "F1")

  ;; Since that doesn't work create an X window and delete it right
  ;; away.  This gives Xmonad a change to adapt to a new situation.
  (with-temp-buffer
    (delete-frame (make-frame '((title  . "emacsFlushState")
				(width  . 1)
				(height . 1)))))

  ;; Unfortunatly some Emacs windows end up not using all the space
  ;; available in the containing frame.  And this does not fix it.
  ;; Using a delay also does not help.
  ;; (redraw-display)
  )

;; REDEFINE `completion--insert-strings' DEFINED IN `completions.el'.
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
	   (wwidth (if window (1- (window-width window))
		     (if xmonad-mode
			 (cdr (assq 'width xmo-completions-frame-alist))
		       79)))
	   (columns (min
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
              (put-text-property (point) (progn (insert (cadr str)) (point))
                                 'mouse-face nil))
            ;; Next column to align to.
            (setq column (+ column
                            ;; Round up to a whole number of columns.
                            (* colwidth (ceiling length colwidth))))))))))

(provide 'xmonad)
;;; xmonad.el ends here

