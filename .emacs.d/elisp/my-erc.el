;;; my-erc.el -- Basic configuration for emacs irc cient

;; Author:        Chirantan Ekbote <chirantan.ekbote <at> gmail.com>
;; Maintainer:    Chirantan Ekbote <chirantan.ekbote <at> gmail.com>
;; Created:       2013-04-06
;; Last-Updated:  $Date: 2013/14/06 19:13:26 $
;; Revision:      $Revision: 0.10 $
;; Compatibility: GNU Emacs 24.x

;;; Code:

(require 'erc)

;; make sure to use wildcards for e.g. freenode as the actual server
;; name can be be a bit different, which would screw up autoconnect
(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist '((".*\\.freenode.net" "#archlinux"
                                     "#archlinux-pacman" "#emacs")
                                    ("localhost" "&bitlbee")))

;; check channels
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                 "324" "329" "332" "333" "353" "477"))
;; don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

;; minimal distraction
(setq erc-format-query-as-channel-p t)
(setq erc-track-faces-priority-list '(erc-error-face
                                      erc-current-nick-face
                                      erc-keyword-face
                                      erc-nick-msg-face
                                      erc-direct-msg-face
                                      erc-dangerous-host-face
                                      erc-notice-face
                                      erc-prompt-face))

;; don't disturb me with high volume channels unless I am mentioned
(setq erc-track-priority-faces-only '("#archlinux" "#emacs"))

;; truncate the length of the channel buffers
(add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
(setq erc-truncate-buffer-on-save t)

(provide 'my-erc)
