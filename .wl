;; mode:-*-emacs-lisp-*-
;; wanderlust
(setq wl-summary-toggle-mime "mime")
(require 'mime-w3m)
(setq wl-draft-reply-buffer-style 'full)

;; select correct email address when we _start_ writing a draft.
(add-hook 'wl-mail-setup-hook 'wl-draft-config-exec)
;; don't apply the templates when sending the draft otherwise
;; choosing another template with C-c C-j won't have any effect
;(add-hook 'wl-draft-send-hook 'wl-draft-config-exec)

;;is run when wl-draft-send-and-exit or wl-draft-send is invoked:
;;(NOTE: "M-: wl-draft-parent-folder" => %INBOX:myname/clear@imap.gmail.com:993)
;;(setq wl-draft-config-alist nil)

(setq wl-draft-config-alist
			'(("^From: .*chirantan" (template . "gmail"))
				(reply "^To: .*ekbote@.*harvard" (template . "seas"))))

;;choose template with C-c C-j
(setq wl-template-alist
      '(("gmail"
         (wl-from . "Chirantan Ekbote <chirantan.ekbote@gmail.com>")
         ("From" . wl-from)
         (wl-smtp-posting-user . "chirantan.ekbote")
         (wl-smtp-posting-server . "smtp.gmail.com")
         (wl-smtp-authenticate-type ."plain")
         (wl-smtp-connection-type . 'starttls)
         (wl-smtp-posting-port . 587)
         (wl-local-domain . "gmail.com")
				 ("Fcc" . ".sent")
         (wl-message-id-domain . "smtp.gmail.com"))
        ("seas"
         (wl-from . "Chirantan Ekbote <ekbote@seas.harvard.edu>")
				 ("From" . wl-from)
				 (wl-smtp-posting-user . "ekbote")
         (wl-smtp-posting-server . "email.seas.harvard.edu")
				 (wl-smtp-authenticate-type ."login")
         (wl-smtp-connection-type . 'starttls)
         (wl-smtp-posting-port . 587)
         (wl-local-domain . "email.seas.harvard.edu")
				 ("Fcc" . "%[Wl]/Sent:ekbote/clear@email.seas.harvard.edu:993!")
				 (wl-message-id-domain . "email.seas.harvard.edu"))))


;;Cycle through templates with arrow keys
(define-key wl-template-mode-map (kbd "<right>") 'wl-template-next)
(define-key wl-template-mode-map (kbd "<left>") 'wl-template-prev)

(setq
 elmo-maildir-folder-path "~/.emacs.d/mail"          ;; where i store my mail
 elmo-localdir-folder-path "~/.emacs.d/mail"
 elmo-msgdb-directory "~/.emacs.d/mail/.elmo"

 bbdb-file "~/.emacs.d/.bbdb"
 bbdb-offer-save 'auto

 wl-stay-folder-window t                       ;; show the folder pane (left)
 wl-folder-window-width 30                     ;; toggle on/off with 'i'
 wl-user-mail-address-list '("chirantan.ekbote@gmail.com"
														 "ekbote@seas.harvard.edu")

 wl-from "Chirantan Ekbote <chirantan.ekbote@gmail.com>"
 wl-smtp-posting-user  "chirantan.ekbote"
 wl-smtp-posting-server  "smtp.gmail.com"
 wl-smtp-authenticate-type "plain"
 wl-smtp-connection-type  'starttls
 wl-smtp-posting-port  587
 wl-local-domain  "gmail.com"
 wl-fcc  ".sent"
 wl-draft-folder  "%[Gmail]/Drafts:chirantan.ekbote/clear@imap.gmail.com:993!"
 wl-message-id-domain  "smtp.gmail.com"

 ;; note: all below are dirs (Maildirs) under elmo-maildir-folder-path
 ;; the '.'-prefix is for marking them as maildirs
 wl-fcc ".sent"                       ;; sent msgs go to the "sent"-folder
 wl-fcc-force-as-read t               ;; mark sent messages as read
 wl-draft-folder ".drafts"
 ;wl-default-folder "%[Gmail]/"           ;; my main inbox
 wl-trash-folder ".trash"             ;; put trash in 'trash'
 wl-queue-folder ".queue"             ;; we don't use this
 wl-temporary-file-directory "~/.emacs.d/mail/tmp"
 wl-auto-save-drafts-interval nil
 ;; check this folder periodically, and update modeline
 wl-biff-check-folder-list '(".todo") ;; check every 180 seconds
                                      ;; (default: wl-biff-check-interval)
)

(require 'filladapt)

;; from a WL mailing list post by Per b. Sederber
;; Re-fill messages that arrive poorly formatted
(defun wl-summary-refill-message (all)
  (interactive "P")
  (if (and wl-message-buffer (get-buffer-window wl-message-buffer))
      (progn
        (wl-summary-toggle-disp-msg 'on)
        (with-current-buffer wl-message-buffer
          (goto-char (point-min))
          (re-search-forward "^$")
          (while (or (looking-at "^\\[[1-9]") (looking-at "^$"))
            (forward-line 1))
          (let* ((buffer-read-only nil)
                 (find (lambda (regexp)
                         (save-excursion
                           (if (re-search-forward regexp nil t)
                               (match-beginning 0)
                             (point-max)))))
                 (start (point))
                 (end (if all
                          (point-max)
                        (min (funcall find "^[^>\n]* wrote:[ \n]+")
                             (funcall find "^>>>>>")
                             (funcall find "^ *>.*\n *>")
                             (funcall find "^-----Original Message-----")))))
            (save-restriction
              (narrow-to-region start end)
              (filladapt-mode 1)
              (fill-region (point-min) (point-max)))))
        (message "Message re-filled"))
    (message "No message to re-fill")))

(define-key wl-summary-mode-map "\M-q" 'wl-summary-refill-message)
