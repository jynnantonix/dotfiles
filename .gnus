;; show at least the last three levels of a group's name
(setq gnus-group-uncollapsed-levels 3)

;(require 'gnus-desktop-notify)
;(gnus-desktop-notify-mode)

;; use gnus-demon to get new mail
(require 'gnus-demon)

;; check for news every 10 minutes if emacs is idle for at least 1 minute
(setq gnus-demon-timestep 10)
(gnus-demon-add-handler 'gnus-demon-scan-news 60 6)
