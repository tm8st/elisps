;;; init-twitter.el --- twitter setting

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, twitter
;; creation time: Wed Apr 28 01:00:27 2010
;;; Commentary:

;;; Code:

(require 'twittering-mode)
(setq twittering-use-wget t)
(setq twittering-use-master-password t)
(setq twittering-fill-column 40)
(setq twittering-timer-interval 10)
(setq twittering-use-icon-storage t)
(setq twittering-icon-storage-limit 1000)

(defun my-twittering-mode-other-window-or-split ()
  (interactive)
  (my-other-window-or-split)
  (twittering-mode))

(defun my-twittering-mode-other-window-or-split-not-move ()
  (interactive)
  (my-other-window-or-split)
  (twittering-mode)
  (my-other-window-or-split))

(global-set-key (kbd "C-l C-t C-w") 'my-twittering-mode-other-window-or-split)
(global-set-key (kbd "C-l C-t C-q") 'my-twittering-mode-other-window-or-split-not-move)

(define-key twittering-mode-map (kbd "C-j") 'twittering-enter)
(define-key twittering-mode-map (kbd "D") 'twittering-direct-message)
(define-key twittering-mode-map (kbd "d") nil)
(define-key twittering-mode-map (kbd "n") 'twittering-goto-next-status)
(define-key twittering-mode-map (kbd "p") 'twittering-goto-previous-thing)
(define-key twittering-mode-map (kbd "h") 'twittering-goto-next-status-of-user)
(define-key twittering-mode-map (kbd "j") 'twittering-goto-previous-thing-of-user)
(define-key twittering-mode-map (kbd "u") nil)
(define-key twittering-mode-map (kbd "U") 'twittering-update-status-interactive)
(define-key twittering-mode-map (kbd "C-c C-d") 'twittering-erase-old-statuses)
(define-key twittering-mode-map (kbd "C-c C-f") 'twittering-friends-timeline)
(define-key twittering-mode-map (kbd "C-c C-s") 'twittering-search)
(define-key twittering-mode-map (kbd "C-c C-t") 'twittering-set-current-hashtag)
(define-key twittering-mode-map (kbd "C-c C-v") 'twittering-view-user-page)

(defun toggle-truncate-lines ()
  "折り返し表示をトグル動作します."
  (interactive)
  (if truncate-lines
      (setq truncate-lines nil)
    (setq truncate-lines t))
  (recenter))

(define-key twittering-mode-map (kbd "l") 'toggle-truncate-lines)

(defun my-twittering-mode-hook ()
  (setq truncate-lines t)
  (yalinum-mode -1)
  )

(add-hook 'twittering-mode-hook 'my-twittering-mode-hook)

;; 112:C-c C-d         twittering-direct-messages-timeline
;; 113:C-c C-e         twittering-erase-old-statuses
;; 114:C-c C-f         twittering-friends-timeline
;; 115:C-c C-l         twittering-update-lambda
;; 116:C-c RET         twittering-retweet
;; 117:C-c C-p         twittering-toggle-proxy
;; 118:C-c C-q         twittering-search
;; 119:C-c C-r         twittering-replies-timeline
;; 120:C-c C-s         twittering-update-status-interactive
;; 121:C-c C-t         twittering-set-current-hashtag
;; 122:C-c C-u         twittering-user-timeline
;; 123:C-c C-v         twittering-view-user-page
;; 124:C-c D           twittering-delete-status

;; 72:TAB             twittering-goto-next-thing
;; 73:RET             twittering-enter
;; 74:C-v             twittering-scroll-up
;; 76:SPC             twittering-scroll-up
;; 80:H               twittering-goto-first-status
;; 81:L               twittering-other-user-list-interactive
;; 82:U               twittering-push-uri-onto-kill-ring
;; 83:V               twittering-visit-timeline
;; 85:a               twittering-toggle-activate-buffer
;; 86:b               twittering-switch-to-previous-timeline
;; 87:d               twittering-direct-message
;; 88:f               twittering-switch-to-next-timeline
;; 89:g               twittering-current-timeline
;; 91:i               twittering-icon-mode
;; 92:j               twittering-goto-next-status
;; 93:k               twittering-goto-previous-status
;; 95:n               twittering-goto-next-status-of-user
;; 96:p               twittering-goto-previous-status-of-user
;; 97:q               twittering-kill-buffer
;; 98:r               twittering-toggle-show-replied-statuses
;; 99:s               twittering-scroll-mode
;; 100:t               twittering-toggle-proxy
;; 101:u               twittering-update-status-interactive
;; 102:v               twittering-other-user-timeline
;; 104:<C-mouse-3>     twittering-push-tweet-onto-kill-ring
;; 105:<backspace>     twittering-scroll-down
;; 106:<backtab>       twittering-goto-previous-thing
;; 107:<mouse-1>       twittering-click
;; 109:M-TAB           twittering-goto-previous-thing
;; 110:M-v             twittering-scroll-down

(require 'init-private)

(provide 'init-twitter)
