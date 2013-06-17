;; Add system site-lisp to load-path
(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(let ((default-directory "/usr/share/emacs/site-lisp"))
  (normal-top-level-add-subdirs-to-load-path))

;; Add the local elisp subdirectory for my files
(add-to-list 'load-path "~/.emacs.d/elisp/")

;; set my name and email
(setq user-full-name "Chirantan Ekbote")
(setq user-mail-address "ekbotec@google.com")

;; Don't allow the startup screen to show
(setq inhibit-startup-screen t)

;; Don't want scroll bar, menu bar, or tool bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; turn on column numbers
(column-number-mode t)

;; use M-y and M-n to answer yes-or-no-p questions
(require 'quick-yes)

;; always show matching parens
(require 'paren)
(show-paren-mode t)

;; set the default tab width to 2 spaces
;(setq default-tab-width 8)

;; interactively set a buffer-local tab width
(defun set-tab-width (num)
  (interactive "NTab Width: ")
  (setq tab-width num))

;; maybe delete trailing whitespace before savin
;; From ~adonovan/.emacs
(defvar skip-whitespace-check nil
  "If non-nil, inhibits behaviour of
  `maybe-delete-trailing-whitespace', which is typically a
  write-file-hook.  This variable may be buffer-local, to permit
  extraneous whitespace on a per-file basis.")
(make-variable-buffer-local 'skip-whitespace-check)

(defun buffer-whitespace-normalized-p ()
  "Returns non-nil if the current buffer contains no tab characters
nor trailing whitespace.  This predicate is useful for determining
whether to enable automatic whitespace normalization.  Simply applying
it blindly to other people's files can cause enormously messy diffs!"
  (save-excursion
    (not  (or (progn (beginning-of-buffer)
                     (search-forward "\t" nil t))
              (progn (beginning-of-buffer)
                     (re-search-forward " +$" nil t))))))

(defun whitespace-check-find-file-hook ()
  (unless (buffer-whitespace-normalized-p)
    (message "Disabling whitespace normalization for this buffer...")
    (setq skip-whitespace-check t)))

(defun toggle-whitespace-removal ()
  "Toggle the value of `skip-whitespace-check' in this buffer."
  (interactive)
  (setq skip-whitespace-check (not skip-whitespace-check))
  (message "Whitespace trimming %s"
           (if skip-whitespace-check "disabled" "enabled")))

(defun maybe-delete-trailing-whitespace ()
  "Calls `delete-trailing-whitespace' iff buffer-local variable
 skip-whitespace-check is nil.  Returns nil."
  (or skip-whitespace-check
      (delete-trailing-whitespace))
  nil)

;; Install hooks so we don't accidentally normalise non-normal files.
(add-hook 'find-file-hook 'whitespace-check-find-file-hook)
(add-hook 'before-save-hook 'maybe-delete-trailing-whitespace)

;; easy boilerplate headers
(require 'header2)

;; use erc for IRC
(require 'my-erc)

;; use chrome for browsing urls
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; Load the color configuration file
(require 'zenburn-theme)
(load-theme 'zenburn t)

;; tetris!!
(require 'tetris)
(define-key tetris-mode-map (kbd "<up>") 'tetris-move-bottom)

;; use ido for fast buffer switching
(require 'ido)
(ido-mode t)

;; always open files and buffers in the current window
(setq ido-default-file-method 'selected-window
      ido-default-buffer-method 'selected-window)

;; use unique filenames
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; set the agenda to look in the org folder
(require 'org-install)
(setq org-agenda-files '("~/.emacs.d/org/"))
(setq org-agenda-start-on-weekday nil          ;; start the agenda on the current day
      org-agenda-start-with-log-mode t         ;; show the daily log
      org-agenda-start-with-follow-mode t      ;; follow the relevant entries
      org-agenda-start-with-clockreport-mode t);; show the clock for the day

;; use native syntax highlighting for src blocks
(setq org-src-fontify-natively t)

;; use ibuffer for more organized buffer management
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; show whitespace with special characters
(require 'whitespace)
(setq whitespace-style '(face
                         tabs
                         spaces
                         trailing
                         space-before-tab
                         newline
                         indentation
                         empty
                         space-after-tab
                         space-mark
                         tab-mark
                         newline-mark))


;; use auto-complete for completion
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             "/usr/local/share/emacs/site-lisp/auto-complete/ac-dict")
(ac-config-default)

;; use completion mode for anything auto-complete cannot handle
(require 'completion)
(dynamic-completion-mode)

;; use google's code style for C/C++
(require 'cc-mode)
;(load-file "/google/src/head/depot/eng/elisp/google.el")

;; linux kernel coding style
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

;; Add kernel style
(c-add-style
 "linux-tabs-only"
 '("linux" (c-offsets-alist
            (arglist-cont-nonempty
             c-lineup-gcc-asm-reg
             c-lineup-arglist-tabs-only))))

(add-hook 'c-mode-hook
          ; set kernel coding style
          (lambda ()
            (setq indent-tabs-mode t)
            (c-set-style "linux-tabs-only")))

(require 'whitespace)
(add-hook 'c-mode-common-hook 'whitespace-mode)

;; enable cc-mode for CUDA source file
(setq auto-mode-alist
      (cons '("\\.cu$" . c-mode) auto-mode-alist))

;; load xcscope for indexing files
(require 'xcscope)

;; use dired-x with dired
(require 'dired)
(load "dired-x")

;; create tags in directory
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command
   (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))

;; Turn on auto-fill when we are in text-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'flyspell-mode)

;; Use shell-pop for running the shell
(require 'shell-pop)
(shell-pop-set-internal-mode "eshell")
(global-set-key (kbd "<f6>") 'shell-pop)

;; toggle window dedication
(defun toggle-window-dedicated()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p
          window
          (not (window-dedicated-p window))))
       "Window %s is dedicated"
     "Window %s is not dedicated")
   (current-buffer)))
(global-set-key (kbd "<f9>") 'toggle-window-dedicated)

;; Pretty-print format xml file
(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))

;; functions to scroll one line at a time
(defun scroll-up-one-line ()
  (interactive)
  (let ((current-prefix-arg '(1)))
    (call-interactively 'scroll-up)))

(defun scroll-down-one-line ()
  (interactive)
  (let ((current-prefix-arg '(1)))
    (call-interactively 'scroll-down)))

;; Keybindings for λ and ƒ
(global-set-key (kbd "H-l") (lambda () (interactive) (insert "\u03bb")))
(global-set-key (kbd "H-f") (lambda () (interactive) (insert "\u0192")))

;; scroll lines globally
(global-set-key (kbd "M-n") 'scroll-up-one-line)
(global-set-key (kbd "M-p") 'scroll-down-one-line)
(global-set-key (kbd "H-n") 'scroll-up-one-line)
(global-set-key (kbd "H-p") 'scroll-down-one-line)

;; call M-x without the alt key
(global-set-key (kbd "C-c C-m") 'execute-extended-command)

;; swap backward-kill-word and kill-region
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "<C-backspace>") 'kill-region)

;; more accessible buffer switching
(global-set-key (kbd "C-x ,") 'previous-buffer)
(global-set-key (kbd "C-x .") 'next-buffer)
(global-set-key (kbd "C-x /") 'set-fill-prefix)           ; use to be on C-x .

;; use buffer-move to switch buffers across windows
(require 'buffer-move)
(global-set-key (kbd "<H-S-left>") 'buf-move-left)
(global-set-key (kbd "<H-S-right>") 'buf-move-right)
(global-set-key (kbd "<H-S-up>") 'buf-move-up)
(global-set-key (kbd "<H-S-down>") 'buf-move-down)
(global-set-key (kbd "C-c m h") 'buf-move-left)
(global-set-key (kbd "C-c m l") 'buf-move-right)
(global-set-key (kbd "C-c m k>") 'buf-move-up)
(global-set-key (kbd "C-c m j") 'buf-move-down)

;; easy commands for window switching
(global-set-key (kbd "C-c <left>") 'windmove-left)       ; move to left window
(global-set-key (kbd "C-c <right>") 'windmove-right)     ; move to right window
(global-set-key (kbd "C-c <up>") 'windmove-up)           ; move to upper window
(global-set-key (kbd "C-c <down>")  'windmove-down)      ; move to lower window
(global-set-key (kbd "C-c h") 'windmove-left)            ; move to left window
(global-set-key (kbd "C-c l") 'windmove-right)           ; move to right window
(global-set-key (kbd "C-c k") 'windmove-up)              ; move to upper window
(global-set-key (kbd "C-c j")  'windmove-down)           ; move to lower window

(global-set-key (kbd "<H-left>") 'windmove-left)         ; move to left window
(global-set-key (kbd "<H-right>") 'windmove-right)       ; move to right window
(global-set-key (kbd "<H-up>") 'windmove-up)             ; move to upper window
(global-set-key (kbd "<H-down>")  'windmove-down)        ; move to lower window

;; switch keybindings for regex and non-regex search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; switch keybindings for regex and non-regex query replace
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace)

;; set functions keys to some useful commands
(global-set-key (kbd "<f7>") 'compile)
(global-set-key (kbd "<f8>") 'gdb)

;; use C-. to repeat commands, only works with X windows
(global-set-key (kbd "C-.") 'repeat)

;; Use M-. when we are not in an X window, swap with find-tag
(global-set-key (kbd "M-.") 'repeat)
(global-set-key (kbd "C-x z") 'find-tag)

;; allow dired to open directories in the same buffer
(put 'dired-find-alternate-file 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
