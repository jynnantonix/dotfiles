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

;; smooth scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-up-aggressively 0
      scroll-down-aggressively 0
      scroll-preserve-screen-position t)

;; limit width of fringes
(set-fringe-mode '(1 . 1))

;; delete selection with keypress or paste
(delete-selection-mode 1)

;; copy-paste interoperability
(setq x-select-enable-clipboard t
      interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; highlight search matches
(setq search-highlight t
      query-replace-highlight t)

;; use popup windows for temporary buffers
(require 'popwin)
(popwin-mode 1)

;; use M-y ad M-n for easy yes-or-no-p
(require 'quick-yes)

;; always show matching parens
(require 'paren)
(show-paren-mode t)

;; turn off tabs
(defun set-no-tabs ()
  (setq indent-tabs-mode nil))

;; ignore case for completions
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t)

;; maybe delete trailing whitespace before savin
(defvar skip-whitespace-check nil
  "If non-nil, inhibits behaviour of
  `maybe-delete-trailing-whitespace', which is typically a
  write-file-hook.  This variable may be buffer-local, to permit
  extraneous whitespace on a per-file basis.")
(make-variable-buffer-local 'skip-whitespace-check)

(defun buffer-whitespace-normalized-p ()
  "Returns non-nil if the current buffer contains no trailing whitespace.
This predicate is useful for determining whether to enable automatic
whitespace normalization.  Simply applying it blindly to other
people's files can cause enormously messy diffs!"
  (save-excursion
    (not (progn (beginning-of-buffer)
                (re-search-forward " +$" nil t)))))

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

;; use an incognito chromium window to browse urls
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium"
      browse-url-generic-args '("--new-window" "--incognito"))

;; use git and magit to handle repos
(require 'git)
(require 'magit)

;; add signoff message with commits
(setq magit-commit-signoff t)

;; set a default key for magit-status
(global-set-key (kbd "C-c g") 'magit-status)

;; use text mode for commit message
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . text-mode))

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

;; always open files and buffers in the current window
(setq ido-default-file-method 'selected-window
      ido-default-buffer-method 'selected-window)

;; use unique filenames
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; use ibuffer for more organized buffer management
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; use helm for completion awesomeness
(require 'helm-config)
(helm-mode 1)

;; use flymake for on-th-fly syntax checking
;(require 'flymake)
;(add-hook 'find-file-hook 'flymake-find-file-hook)

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

;; set the agenda to look in the org folder
(setq org-agenda-files '("~/.emacs.d/org/"))
(setq org-agenda-start-on-weekday 1            ;; start the agenda on Monday
      org-agenda-start-with-log-mode t         ;; show the daily log
      org-agenda-start-with-follow-mode nil    ;; don't follow entries
      org-agenda-start-with-clockreport-mode t);; show the clock for the day

;; use 5 priority levels
(setq org-highest-priority ?A
      org-lowest-priority ?E
      org-default-priority ?C)

;; use chromium as default pdf viewer
(setq org-file-apps '((auto-mode . emacs)
		      ("\\.mm\\'" . default)
		      ("\\.x?html?\\'" . default)
		      ("\\.pdf\\'" . "/usr/bin/chromium %s")))

;; use a logbook for clock entries
(setq org-clock-into-drawer t)

;; mark when a task is marked as done
(setq org-log-done t)

;; put logs into LOGBOOK
(setq org-log-into-drawer t)

;; use year/month/day for calendar entries
(setq calendar-date-style 'iso)

;; use symbolic links for attachments
(setq org-attach-method 'lns)

;; use native fontification for source code
;(setq org-src-fontify-natively nil)

;; set a keybinding for org-agenda
(global-set-key (kbd "C-c a") 'org-agenda)

;; use gnus for mailing lists
(require 'gnus)

;; set the appropriate mail and new directories
(setq-default message-directory "~/mail/gnus/mail")
(setq-default gnus-directory "~/mail/gnus/news")

(setq gnus-select-method '(nntp "news.gmane.org"))
(add-to-list 'gnus-secondary-select-methods '(nntp "news.gnus.org"))
(add-to-list 'gnus-secondary-select-methods '(nntp "news.gwene.org"))

;; render html with gnus w3m
(setq mm-text-html-renderer 'gnus-w3m)

;; create keybinding for gnus
(global-set-key (kbd "C-c n") 'gnus)

;; use mu4e for emails
(require 'mu4e)
(setq mu4e-maildir "~/mail")               ;; top-level mail directory
(setq mu4e-get-mail-command "mbsync -a"    ;; use mbsync to sync mail ...
      mu4e-update-interval 600)            ;; ... every 10 minutes
(setq mu4e-change-filenames-when-moving t) ;; work with mbsync maildir format

;; store attachments in the downloads folder
(setq mu4e-attachment-dir "~/downloads")

;; use bookmark for starred messages
(add-to-list 'mu4e-bookmarks '("flag:flagged" "Starred messages" ?s))

;; add my email addresses
(setq mu4e-user-mail-address-list
  (append mu4e-user-mail-address-list '("ekbote@seas.harvard.edu"
                                        "ekbotec@chromium.org"
                                        "ekbote@mit.edu"
                                        "ekbote@g.harvard.edu"
                                        "ekbote@fas.harvard.edu")))

;; don't reply to myself
(setq mu4e-compose-dont-reply-to-self t)

;; automatically sign all my messages
(add-hook 'mu4e-compose-mode-hook 'mml-secure-message-sign-pgpmime)

;; give the signature a name
(defadvice mml2015-sign (after mml2015-sign-rename (cont) act)
  (save-excursion
    (search-backward "Content-Type: application/pgp-signature")
    (goto-char (point-at-eol))
    (insert "; name=\"signature.asc\"")))

;; use w3m to render html emails
;(setq mu4e-html2text-command "w3m -T text/html -dump")
(setq mu4e-html2text-command 'w3m-buffer)

;; other mu4e useful features
(setq mu4e-view-show-images t
      mu4e-view-show-addresses t
      mu4e-view-image-max-width 800)

;; create a keybinding for mu4e
(global-set-key (kbd "C-c m") 'mu4e)

;; use gnus smtp for sending mails
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; delete message buffer when we are done
(setq message-kill-buffer-on-exit t)

;; set up multiple smtp accounts
(defun select-smtp-account ()
  "Set smtp account information  based on the contact field of the
original message or prompt if there is no parent message"
  (interactive)
  (let ((msg mu4e-compose-parent-message))
(setq user-mail-address
  (cond
   ((and msg (mu4e-message-contact-field-matches msg :to "chirantan.ekbote@gmail.com"))
    "chirantan.ekbote@gmail.com")
   ((and msg (mu4e-message-contact-field-matches msg :to "ekbotec@chromium.org"))
    "ekbotec@chromium.org")
   ((and msg (mu4e-message-contact-field-matches msg :to "ekbote@mit.edu"))
    "ekbote@mit.edu")
   ((and msg (mu4e-message-contact-field-matches msg :to "ekbote@\\(seas\\|fas\\|g\\).harvard.edu"))
    "ekbote@seas.harvard.edu")
   (t (completing-read "Enter user-mail-address: " mu4e-user-mail-address-list))))
(setq smtpmail-smtp-user user-mail-address)
(cond
 ((string-match "ekbote@seas.harvard.edu" user-mail-address)
  (setq smtpmail-smtp-server "email.seas.harvard.edu"
        mu4e-sent-messages-behavior 'sent))
 ((string-match "ekbote@mit.edu" user-mail-address)
  (setq smtpmail-smtp-server "outgoing.mit.edu"
        mu4e-sent-messages-behavior 'sent))
 ((string-match ".*@\\(chromium.org\\|gmail.com\\)" user-mail-address)
  (setq smtpmail-smtp-server "smtp.gmail.com"
        mu4e-sent-messages-behavior 'delete))
 (t
  (setq smtpmail-smtp-server (completing-read "SMTP server: "
                               '("smtp.gmail.com" "email.seas.harvard.edu"))
        mu4e-sent-messages-behavior (make-symbol (completing-read "Sent message behavior: "
                                                   '("delete" "sent" "trash"))))))))

(add-hook 'mu4e-compose-pre-hook 'select-smtp-account)

;; use epa to interface with gpg
(require 'epa-file)
;(epa-file-enable)

;; load gnuplot
;(require 'gnuplot)
;(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))

;; use org-babel-gnuplot to create graphs from org
;(require 'ob-gnuplot)

;; emms for multimedia files
(require 'emms-setup)
(emms-standard)
(emms-default-players)

;; use mpd as a back-end
(require 'emms-player-mpd)
(setq emms-player-mpd-server-name "localhost"
      emms-player-mpd-server-port "6600")
(add-to-list 'emms-info-functions 'emms-info-mpd)
(add-to-list 'emms-player-list 'emms-player-mpd)
(setq emms-player-mpd-music-directory "~/music")

;; volume control and playback through emms
(require 'emms-volume)
(global-set-key (kbd "<XF86AudioNext>") 'emms-next)
(global-set-key (kbd "<XF86AudioPrev>") 'emms-previous)
(global-set-key (kbd "<XF86AudioPlay>") 'emms-pause)

;; Set up google and kernel styles for C
(require 'cc-mode)
(require 'google-c-style)

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

;; llvm coding style
(c-add-style "llvm.org"
             '((fill-column . 80)
               (c++-indent-level . 2)
               (c-basic-offset . 2)
               (indent-tabs-mode . nil)
               (c-offsets-alist . ((innamespace 0)))))


;;; allow use of clang-format without replacing the whole buffer
;;; lifted from go-mode

;; Delete the current line without putting it in the kill-ring.
(defun delete-whole-line (&optional arg)
  ;; Emacs uses both kill-region and kill-new, Xemacs only uses
  ;; kill-region. In both cases we turn them into operations that do
  ;; not modify the kill ring. This solution does depend on the
  ;; implementation of kill-line, but it's the only viable solution
  ;; that does not require to write kill-line from scratch.
  (flet ((kill-region (beg end)
                      (delete-region beg end))
         (kill-new (s) ()))
    (kill-whole-line arg)))

(defun apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current
buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "invalid rcs patch or internal error in apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (goto-char (point-min))
                (forward-line (- from line-offset 1))
                (incf line-offset len)
                (delete-whole-line len)))
             (t
              (error "invalid rcs patch or internal error in apply-rcs-patch")))))))))

;; use LLVM style in clang-format
(defvar clang-format-style "LLVM")

(defun clang-format ()
  "Formats the current buffer using clang-format with the style set in
`clang-format-style'"

  (interactive)
  (let ((tmpfile (make-temp-file "clangfmt" nil ".c"))
        (patchbuf (get-buffer-create "*clang-format patch*"))
        (errbuf (get-buffer-create "*clang-format errors*")))

    (with-current-buffer errbuf
      (setq buffer-read-only nil)
      (erase-buffer))
    (with-current-buffer patchbuf
      (erase-buffer))

    (write-region nil nil tmpfile)

    (if (zerop (call-process "clang-format" nil errbuf nil "-i" "-style"
                             clang-format-style tmpfile))
        (unless (zerop (call-process-region (point-min) (point-max) "diff" nil
                                            patchbuf nil "-n" "-" tmpfile))
          (apply-rcs-patch patchbuf)
          (message "Applied clang-format"))
      (progn
        (message "Could not apply clang-format")
        (display-buffer errbuf)))

    (kill-buffer patchbuf)
    (delete-file tmpfile)))

;; use llvm coding style unless we are in a linux tree
(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (if (and filename
                       (string-match (expand-file-name "~/src/linux-trees")
                                     filename))
                (progn
                  (setq indent-tabs-mode t)
                  (c-set-style "linux-tabs-only"))
                (progn
                  (c-set-style "llvm.org")
                  (add-hook 'before-save-hook 'clang-format nil t))))))

(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "llvm.org")
            (add-hook 'before-save-hook 'clang-format nil t)))

;; enable cc-mode for CUDA source file
(add-to-list 'auto-mode-alist '("\\.cu$" . c-mode))

;; use go mode for Go files
(require 'go-mode-load)

;; always run gofmt before saving
(add-hook 'go-mode-hook (lambda () (add-hook 'before-save-hook 'gofmt nil t)))
(add-hook 'go-mode-hook (lambda () (setq tab-width 4)))

;; load xcscope for indexing files
(require 'xcscope)

;; use haskell-mode in haskell files
(require 'haskell-mode-autoloads)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; show whitespace with special characters
(require 'whitespace)
(add-hook 'c-mode-common-hook 'whitespace-mode)
(add-hook 'emacs-lisp-mode-hook 'whitespace-mode)
(add-hook 'asm-mode-hook 'whitespace-mode)
(add-hook 'lisp-mode-hook 'whitespace-mode)
(add-hook 'python-mode-hook 'whitespace-mode)
(add-hook 'haskell-mode-hook 'whitespace-mode)
(add-hook 'scheme-mode-hook 'whitespace-mode)
(add-hook 'shell-script-mode 'whitespace-mode)

;; show whitespace with special characters
(setq whitespace-style '(face
                         tabs
                         spaces
                         trailing
                         lines-tail
                         space-before-tab
                         newline
                         empty
                         space-after-tab
                         space-mark
                         tab-mark
                         newline-mark))

;; turn off tabs in some modes
(add-hook 'lisp-mode-hook 'set-no-tabs)
(add-hook 'scheme-mode-hook 'set-no-tabs)
(add-hook 'emacs-lisp-mode-hook 'set-no-tabs)
(add-hook 'python-mode-hook 'set-no-tabs)
(add-hook 'shell-script-mode 'set-no-tabs)

;; use geiser-mode when working with scheme
(require 'geiser-install)

;; use dired-x with dired
(require 'dired)
(load "dired-x")

;; use header2 to generate and update file headers
(require 'header2)
;; (add-hook 'c-mode-common-hook 'auto-make-header)
;; (add-hook 'emacs-lisp-mode-hook 'auto-make-header)
;; (add-hook 'write-file-hooks 'auto-update-file-header)

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

;; enable shell-script-mode for AUR pkgbuilds
(add-to-list 'auto-mode-alist '("PKGBUILD\\'" . shell-script-mode))

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
(global-set-key (kbd "C-x /") 'set-fill-prefix)       ; used to be on C-x .

;; swap adjacent buffers without C-x b
(require 'buffer-move)
(global-set-key (kbd "C-c C-h") 'buf-move-left)       ; swap with left buffer
(global-set-key (kbd "C-c C-l") 'buf-move-right)      ; swap wth right buffer
(global-set-key (kbd "C-c C-k") 'buf-move-up)         ; swap with top buffer
(global-set-key (kbd "C-c C-j") 'buf-move-down)       ; swap with bottom buffer

;; easy commands for window switching
(require 'windmove)
(setq windmove-wrap-around t)
(global-set-key (kbd "C-c h") 'windmove-left)         ; move to left window
(global-set-key (kbd "C-c l") 'windmove-right)        ; move to right window
(global-set-key (kbd "C-c k") 'windmove-up)           ; move to upper window
(global-set-key (kbd "C-c j")  'windmove-down)        ; move to lower window

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
