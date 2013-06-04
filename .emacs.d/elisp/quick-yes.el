;;; quick-yes.el --- M-y to answer "yes" to `yes-or-no-p'.

;; Copyright 2004, 2005, 2007, 2009, 2010 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 8
;; Keywords: convenience
;; URL: http://user42.tuxfamily.org/quick-yes/index.html
;; EmacsWiki: QuickYes

;; quick-yes.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; quick-yes.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This spot of code for Emacs 21 (and up) binds M-y and M-n to answer "yes"
;; or "no" to a `yes-or-no-p' question.
;;
;; Some `yes-or-no-p' questions can be disabled, but if you leave them
;; enabled as a reminder or just in case it's important then M-y is a good
;; shortcut for accepting.
;;
;; If you start typing "y" or "ye", etc, you can still use M-y to finish it.
;; Typing "y" is easy to do if you don't immediately notice it's
;; `yes-or-no-p' instead of `y-or-n-p'.
;;
;; quick-yes only affects a `yes-or-no-p' question in the minibuffer, it
;; doesn't change a window system dialog box as used by `yes-or-no-p' when
;; the invoking action was a mouse button press instead of something from
;; the keyboard.

;;; Install:

;; Put quick-yes.el in one of your `load-path' directories, and in your
;; .emacs add
;;
;;     (require 'quick-yes)

;;; History:

;; Version 1 - the first version
;; Version 2 - add M-n for no
;; Version 3 - cope with xemacs prompt not part of the minibuffer as such
;; Version 4 - GPL v.3
;; Version 5 - track other hacks to minibuffer-local-map value too
;; Version 6 - complete "y" etc when point at start of minibuffer too
;; Version 7 - undo defadvice on unload-feature
;; Version 8 - express dependency on 'advice

;;; Code:

;; for `ad-find-advice' macro when running uncompiled
;; (don't unload 'advice before our -unload-function)
(require 'advice)

;; This is implemented as an advice around `yes-or-no-p' and a temporarily
;; applied `quick-yes-map' keymap, so as to restrict the effect to just
;; `yes-or-no-p'.  All other minibuffer reading is unchanged, and M-y can be
;; bound to other things in other minibuffer contexts, through additions to
;; `minibuffer-local-map' or whatever.

(defun quick-yes-answer-yes ()
  "Say \"yes\" in the minibuffer.
It either enters \"yes\" or finishes a \"y\" or \"ye\" you've
started to type, then presses return (ie. `exit-minibuffer').

Read more in the comments in quick-yes.el.  The quick-yes.el home
page is URL `http://user42.tuxfamily.org/quick-yes/index.html'"
  (interactive)
  ;; in xemacs the prompt isn't part of the buffer, so must watch out for
  ;; going below (point-min) when subtracting from (point)
  (goto-char (point-max))
  (cond ((string= "y"   (buffer-substring (max (point-min) (1- (point)))
                                          (point)))
         (insert "es"))
        ((string= "ye"  (buffer-substring (max (point-min) (- (point) 2))
                                          (point)))
         (insert "s"))
        ((string= "yes" (buffer-substring (max (point-min) (- (point) 3))
                                          (point))))
        (t
         (insert "yes")))
  (exit-minibuffer))

(defun quick-yes-answer-no ()
  "Say \"no\" in the minibuffer.
It either enters \"no\" or finishes an \"n\" you've started to
type, then presses return (ie. `exit-minibuffer')."
  (interactive)
  ;; in xemacs the prompt isn't part of the buffer, so must watch out for
  ;; going below (point-min) when subtracting from (point)
  (goto-char (point-max))
  (cond ((string= "n"  (buffer-substring (max (point-min) (1- (point)))
                                         (point)))
         (insert "o"))
        ((string= "no" (buffer-substring (max (point-min) (- (point) 2))
                                         (point))))
        (t
         (insert "no")))
  (exit-minibuffer))

(defvar quick-yes-map
  (let ((m (make-sparse-keymap)))
    (define-key m [?\M-n] 'quick-yes-answer-no)
    (define-key m [?\M-y] 'quick-yes-answer-yes)
    m)
  "Extra keymap advised around `yes-or-no-p' by quick-yes.")

(defadvice yes-or-no-p (around quick-yes activate)
  "\\<quick-yes-map>\\[quick-yes-answer-yes] to answer yes (`quick-yes-answer-yes'), and \\<quick-yes-map>\\[quick-yes-answer-no] for no."
  ;; set-keymap-parent each time in case someone else is mucking about with
  ;; the minibuffer-local-map variable the same as done here!
  (set-keymap-parent quick-yes-map minibuffer-local-map)
  (let ((minibuffer-local-map quick-yes-map))
    ad-do-it))

(defun quick-yes-unload-function ()
  (when (ad-find-advice 'yes-or-no-p 'around 'quick-yes)
    (ad-remove-advice   'yes-or-no-p 'around 'quick-yes)
    (ad-activate        'yes-or-no-p))
  nil) ;; and do normal unload-feature actions too

(provide 'quick-yes)

;;; quick-yes.el ends here
