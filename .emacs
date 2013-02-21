;; Add the local elisp subdirectory for my files
(add-to-list 'load-path "~/.emacs.d/elisp/")

;; Don't allow the startup screen to show
(setq inhibit-startup-screen t)

;; Don't want scroll bar, menu bar, or tool bar
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Use continuous mode when using DocView and mimic emacs scrolling
(require 'doc-view)
(setq doc-view-continuous t)
(define-key doc-view-mode-map (kbd "C-v") 'doc-view-scroll-up-or-next-page)
(define-key doc-view-mode-map (kbd "M-v") 'doc-view-scroll-down-or-previous-page)


;; Have w3m ready incase we need to do some browsing
(require 'w3m-load)

;; Load the color configuration file
(require 'my-colors)

;; use ido-mode for improved buffer switching
(require 'ido)
(ido-mode t)

;; use AucTex for LaTex files
;(load "auctex.el" nil t t)
;(load "preview-latex.el" nil t t)

;; turn on column numbers
(column-number-mode t)

;; use google's code style for C/C++
(require 'cc-mode)
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; enable cc-mode for CUDA source file
(setq auto-mode-alist
      (cons '("\\.cu$" . c-mode) auto-mode-alist))

;; load xcscope for indexing files
;; and ascope for fast searching
(require 'xcscope)
(require 'ascope)
(define-key c-mode-base-map (kbd "C-c a I") 'ascope-init)
(define-key c-mode-base-map (kbd "C-c a d") 'ascope-find-global-definition)
(define-key c-mode-base-map (kbd "C-c a s") 'ascope-find-this-symbol)
(define-key c-mode-base-map (kbd "C-c a t") 'ascope-find-this-text-string)
(define-key c-mode-base-map (kbd "C-c a c") 'ascope-find-functions-calling-this-function)
(define-key c-mode-base-map (kbd "C-c a C") 'ascope-find-called-functions)
(define-key c-mode-base-map (kbd "C-c a e") 'ascope-find-this-egrep-pattern)
(define-key c-mode-base-map (kbd "C-c a f") 'ascope-find-this-file)
(define-key c-mode-base-map (kbd "C-c a i") 'ascope-find-files-including-file)
(define-key c-mode-base-map (kbd "C-c a o") 'ascope-clear-overlay-arrow)
(define-key c-mode-base-map (kbd "C-c a u") 'ascope-pop-mark)

;; Turn on auto-fill when we are in text-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Use shell-pop for running the shell
(require 'shell-pop)
(shell-pop-set-internal-mode "eshell")
(global-set-key (kbd "<f6>") 'shell-pop)

;; use auto-fill for latex
(add-hook 'latex-mode-hook (lambda ()
			     (setq auto-fill-mode t)))

;; change python indents to 4 spaces and set the fill-column
;; to 79
(add-hook 'python-mode-hook (lambda ()
			      (setq python-guess-indent nil
				    python-indent 4
				    fill-column 79
				    auto-fill-mode t)))

;; enable cmake-mode for cmake files
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

;; enable shell-script-mode for AUR pkgbuilds
(setq auto-mode-alist
      (cons '("PKGBUILD\\'" . shell-script-mode) auto-mode-alist))

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

(global-set-key (kbd "C->") 'scroll-up-one-line)
(global-set-key (kbd "C-<") 'scroll-down-one-line)

;; easy commands for window switching
(global-set-key (kbd "C-c <left>") 'windmove-left)          ; move to left window
(global-set-key (kbd "C-c <right>") 'windmove-right)        ; move to right window
(global-set-key (kbd "C-c <up>") 'windmove-up)              ; move to upper window
(global-set-key (kbd "C-c <down>")  'windmove-down)         ; move to lower window
(global-set-key (kbd "H-h") 'windmove-left)                 ; move to left window
(global-set-key (kbd "H-l") 'windmove-right)                ; move to right window
(global-set-key (kbd "H-k") 'windmove-up)                   ; move to upper window
(global-set-key (kbd "H-j")  'windmove-down)                ; move to lower window

;; set functions keys to some useful commands
(global-set-key (kbd "<f7>") 'compile)
(global-set-key (kbd "<f8>") 'gdb)

;; use C-. to repeat commands
(global-set-key (kbd "C-.") 'repeat)

;; allow dired to open directories in the same buffer
(put 'dired-find-alternate-file 'disabled nil)
