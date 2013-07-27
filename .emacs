;; Add the local elisp subdirectory for my files
(add-to-list 'load-path "~/.emacs.d/elisp/")

;; set my name and email
(setq user-full-name "Chirantan Ekbote")
(setq user-mail-address "chirantan.ekbote@gmail.com")

;; Don't allow the startup screen to show
(setq inhibit-startup-screen t)

;; Don't want scroll bar, menu bar, or tool bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; turn on column numbers
(column-number-mode t)

;; use popup windows for temporary buffers
(require 'popwin)
(popwin-mode 1)

;; use M-y ad M-n for easy yes-or-no-p
(require 'quick-yes)

;; always show matching parens
(require 'paren)
(show-paren-mode t)

;; always delete trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; use an incognito chromium window to browse urls
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium"
      browse-url-generic-args '("--new-window" "--incognito"))

;; use git and magit to handle repos
(require 'git)
(require 'magit)

;; use erc for IRC
(require 'my-erc)

;; Use continuous mode when using DocView and mimic emacs scrolling
(require 'doc-view)
(setq doc-view-continuous t)
(define-key doc-view-mode-map (kbd "C-v") 'doc-view-scroll-up-or-next-page)
(define-key doc-view-mode-map (kbd "M-v") 'doc-view-scroll-down-or-previous-page)

;; don't ask when reloading pdfs
(add-to-list 'revert-without-query ".*\.pdf")

;; Have w3m ready incase we need to do some browsing
(require 'w3m-load)

;; play video stream using mplayer and youtube-dl
(defun play-stream (url)
  (interactive "sURL: ")
  (let ((buffer (get-buffer-create "*stream-output*")))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      ;; Setting buffer-read-only to nil doesn't suffice
      ;; if some text has a non-nil read-only property,
      ;; which comint sometimes adds for prompts.
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq proc (start-process-shell-command "streaming" buffer
                   (concat "mplayer $(youtube-dl -g " (shell-quote-argument url) ")" )))
      (setq mode-line-process '(":%s"))
      (require 'shell) (shell-mode)
      (set-process-sentinel proc 'shell-command-sentinel)
      ;; Use the comint filter for proper handling of carriage motion
      ;; (see `comint-inhibit-carriage-motion'),.
      (set-process-filter proc 'comint-output-filter))))

;; play a streaming url
(defun stream-region ()
  (interactive)
  (if (use-region-p)
      (play-stream (buffer-substring (region-beginning) (region-end)))
    (message "No region selected...")))

;; play a stream through w3m
(defun w3m-stream ()
  (interactive)
  (let ((url (w3m-anchor)))
    (play-stream url)))

(eval-after-load "w3m"
  '(progn
     ;; Overwrite copy-buffer because C-c C-t does the same thing
     (define-key w3m-mode-map (kbd "M-n") 'scroll-up-one-line)
     ;; Use p to play streaming urls
     (define-key w3m-mode-map (kbd "p") 'w3m-stream)))

;; Load the color configuration file
(require 'zenburn-theme)
(load-theme 'zenburn t)

;; tetris!!
(require 'tetris)
(define-key tetris-mode-map (kbd "<up>") 'tetris-move-bottom)

;; use ido for fast buffer switching
(require 'ido)
(ido-mode t)

;; use ibuffer for more organized buffer management
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; use flymake for on-th-fly syntax checking
(require 'flymake)
;(add-hook 'find-file-hook 'flymake-find-file-hook)

;; show whitespace with special characters
(require 'whitespace)

;; use auto-complete for completion
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             "/usr/share/emacs/site-lisp/auto-complete/ac-dict")
(ac-config-default)

;; workaround for flyspell mode
(ac-flyspell-workaround)

;; use completion mode for anything auto-complete cannot handle
(require 'completion)
(dynamic-completion-mode)

;; use AucTex for LaTeX files
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;; make AucTex ask for the master file
(setq-default TeX-master nil)

;; default to creating pdf not dvi
(setq-default TeX-PDF-mode t)

;; use reftex with auctex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'reftex-mode-hook 'imenu-add-menubar-index)

;; use auto-fill for latex
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)

;; flyspell and math modes
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

;; use the latest org-mode
(require 'org-install)

;; use gnus for mailing lists
(require 'gnus)

;; set the appropriate mail and new directories
(setq-default message-directory "~/mail/gnus/mail")
(setq-default gnus-directory "~/mail/gnus/news")

(setq gnus-select-method '(nntp "news.gmane.org"))
(add-to-list 'gnus-secondary-select-methods '(nntp "news.gnus.org"))

;; render html with gnus w3m
(setq mm-text-html-renderer 'gnus-w3m)

;; use mu4e for emails
(require 'mu4e)
(setq mu4e-maildir "~/mail")               ;; top-level mail directory
(setq mu4e-get-mail-command "mbsync -a"    ;; use mbsync to sync mail ...
      mu4e-update-interval 600)            ;; ... every 10 minutes
(setq mu4e-change-filenames-when-moving t) ;; work with mbsync maildir format

(add-hook 'mu4e-index-updated-hook
          (lambda () (shell-command "notify-send 'Mail index updated'")))

;; use w3m to render html emails
(setq mu4e-html2text-command "w3m -T text/html -dump")

;; use gnus smtp for sending mails
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; delete messages sent through gmail
(setq mu4e-sent-messages-behavior 'delete)
(setq message-kill-buffer-on-exit t)

;; other mu4e useful features
(setq mu4e-view-show-images t
      mu4e-view-image-max-width 800)

;; load gnuplot
(require 'gnuplot)
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))

;; use org-babel-gnuplot to create graphs from org
(require 'ob-gnuplot)

;; set the agenda to look in the org folder
(setq org-agenda-files '("~/.emacs.d/org/"))

;; emms for multimedia files
(require 'emms-setup)
(emms-standard)
(emms-default-players)

;; volume control and playback through emms
(require 'emms-volume)
(global-set-key (kbd "<XF86AudioNext>") 'emms-next)
(global-set-key (kbd "<XF86AudioPrev>") 'emms-previous)
(global-set-key (kbd "<XF86AudioPlay>") 'emms-pause)

;; use google's code style for C/C++
(require 'cc-mode)
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(add-hook 'c-mode-common-hook 'whitespace-mode)

;; enable cc-mode for CUDA source file
(setq auto-mode-alist
      (cons '("\\.cu$" . c-mode) auto-mode-alist))

;; use go mode for Go files
(require 'go-mode-load)

;; always run gofmt before saving
(add-hook 'before-save-hook #'gofmt-before-save)

;; load xcscope for indexing files
(require 'xcscope)

;; use dired-x with dired
(require 'dired)
(load "dired-x")

;; use header2 to generate and update file headers
(require 'header2)
(add-hook 'c-mode-common-hook 'auto-make-header)
(add-hook 'emacs-lisp-mode-hook 'auto-make-header)
(add-hook 'write-file-hooks 'auto-update-file-header)

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

;; easy commands for window switching
(global-set-key (kbd "C-c <left>") 'windmove-left)          ; move to left window
(global-set-key (kbd "C-c <right>") 'windmove-right)        ; move to right window
(global-set-key (kbd "C-c <up>") 'windmove-up)              ; move to upper window
(global-set-key (kbd "C-c <down>")  'windmove-down)         ; move to lower window
(global-set-key (kbd "C-c <C-left>") 'windmove-left)        ; move to left window
(global-set-key (kbd "C-c <C-right>") 'windmove-right)      ; move to right window
(global-set-key (kbd "C-c <C-up>") 'windmove-up)            ; move to upper window
(global-set-key (kbd "C-c <C-down>")  'windmove-down)       ; move to lower window

(global-set-key (kbd "<H-left>") 'windmove-left)            ; move to left window
(global-set-key (kbd "<H-right>") 'windmove-right)          ; move to right window
(global-set-key (kbd "<H-up>") 'windmove-up)                ; move to upper window
(global-set-key (kbd "<H-down>")  'windmove-down)           ; move to lower window

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

;; use C-. to repeat commands
(global-set-key (kbd "C-.") 'repeat)

;; allow dired to open directories in the same buffer
(put 'dired-find-alternate-file 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'set-goal-column 'disabled nil)
