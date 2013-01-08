;;; my-colors.el --- Sets basic color configuration for emacs.

;; Author:        Chirantan Ekbote <chirantan.ekbote <at> gmail.com>
;; Maintainer:    Chirantan Ekbote <chirantan.ekbote <at> gmail.com>
;; Created:       2012-12-10
;; Last-Updated:  $Date: 2012/12/10 19:13:26 $
;; Revision:      $Revision: 0.10 $
;; Compatibility: GNU Emacs 24.x

;;; Code:

;; Set foreground, background, and font
(set-face-attribute 'default nil :foreground "white" :background "black" :family "Terminus") 

;; Set compilation colors
(add-hook 'compilation-mode-hook
	  (function (lambda ()
		      (set-face-attribute 'compilation-error nil :foreground "firebrick2"))))

;; Set erc colors
(eval-after-load 'erc-mode
  '(progn
     (set-face-attribute 'bg:erc-color-face0 nil :background "dark gray")
     (set-face-attribute 'bg:erc-color-face10 nil :background "dodger blue")
     (set-face-attribute 'fg:erc-color-face1 nil :foreground "gainsboro")
     (set-face-attribute 'fg:erc-color-face2 nil :foreground "royal blue")))

;; Set diff-mode colors
(eval-after-load 'diff-mode
  '(progn
     (set-face-attribute 'diff-changed nil :foreground "gold1")
     (set-face-attribute 'diff-context nil :foreground "white")
     (set-face-attribute 'diff-file-header nil :foreground "DeepSkyBlue1" :background "black" :weight 'bold)
     (set-face-attribute 'diff-header nil :foreground "cyan" :background "black")
     (set-face-attribute 'diff-hunk-header nil :background "gold" :foreground "black")
     (set-face-attribute 'diff-removed nil :foreground "tomato")
     (set-face-attribute 'diff-added nil :foreground "SeaGreen")))

(provide 'my-colors)
