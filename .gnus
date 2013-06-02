;; use gnus-demon to get new mail
(require 'gnus-demon)

;; check for news every 10 minutes if emacs is idle for at least 10 seconds
(setq gnus-demon-timestep 10)
(gnus-demon-add-handler 'gnus-demon-scan-news 60 1)
