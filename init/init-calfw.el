;;; init-calfw.el --- calfw setting

;; Copyright (C) 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, calfw

;;; Commentary:

;;; Code:

;; git clone git://github.com/kiwanami/emacs-calfw.git
;; (auto-install-from-url "https://github.com/kiwanami/emacs-calfw/raw/master/calfw.el")
;; (auto-install-from-url "https://github.com/kiwanami/emacs-calfw/raw/master/calfw-org.el")

(require 'calfw)
(require 'calfw-org)
(setq cfw:details-window-size 80)

(global-set-key (kbd "C-l C-o C-d") 'cfw:open-org-calendar)
(define-key cfw:calendar-mode-map (kbd "d") 'cfw:change-view-day)
(define-key cfw:calendar-mode-map (kbd "m") 'cfw:change-view-month)
(define-key cfw:calendar-mode-map (kbd "t") 'cfw:change-view-two-weeks)
(define-key cfw:calendar-mode-map (kbd "w") 'cfw:change-view-week)

;; ;; 月
;; (setq calendar-month-name-array
;;   ["January" "February" "March"     "April"   "May"      "June"
;;    "July"    "August"   "September" "October" "November" "December"])

;; ;; 曜日
;; (setq calendar-day-name-array
;;       ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"])

;; ;; 週の先頭の曜日
;; (setq calendar-week-start-day 0) ; 日曜日は0, 月曜日は1

;; 
(custom-set-faces
 '(cfw:face-title ((t (:foreground "#380f2f" :weight bold :height 2.5 :inherit variable-pitch))))
 '(cfw:face-header ((t (:foreground "#d08d8d" :weight bold))))
 '(cfw:face-sunday ((t :foreground "#cc9393" :background "#fee0d0" :weight bold)))
 '(cfw:face-saturday ((t :foreground "#8cd0d3" :background "#eee0d0" :weight bold)))
 '(cfw:face-holiday ((t :foreground "#8c5353" :background "#eee0d0" :weight bold)))
 '(cfw:face-default-content ((t :foreground "#bfebb0")))
 '(cfw:face-regions ((t :foreground "#866060")))
 '(cfw:face-day-title ((t :background "#ede6c3")))
 '(cfw:face-periods ((t :foreground "#8cd0d3")))
 '(cfw:face-today-title ((t :background "#7f9f7f" :weight bold)))
 '(cfw:face-today ((t :background: "pink3" :weight bold)))
 '(cfw:face-select ((t :background "pink1"))))

(provide 'init-calfw)