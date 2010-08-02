;;; init-window.el --- window manage setting.

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, manage
;; creation time: Sat Jun 19 15:38:16 2010
;;; Commentary:

;;; Code:

;; (require 'e2wm)

;; ;; レイアウト
;; (setq e2wm:c-code-recipe
;;   '(| (:left-max-size 15)
;;       (- (:upper-size-ratio 0.5)
;;          files history)
;;       (- (:upper-size-ratio 0.8)
;;          (| (:right-max-size 30)
;;             main imenu)
;;          sub)))

;; レイアウト
;; (setq e2wm:c-code-recipe
;;   '(| (:left-max-size 15)
;;       (- (:upper-size-ratio 0.75)
;;          imenu history)
;;       (- (:upper-size-ratio 0.75)
;;          (| (:right-max-size 10)
;;             main files)
;;          sub)))

;; (setq e2wm:c-code-recipe
;;   '(| (:left-max-size 35)
;;       (- (:upper-size-ratio 0.3)
;;          imenu files)
;;       (- (:upper-size-ratio 0.7)
;;          (| (:right-max-size 30)
;;             main history)
;;          sub)))

;; レイアウト
;; (setq e2wm:c-code-recipe
;;   '(| (:left-max-size 20)
;;       (- (:upper-size-ratio 0.5)
;;          files history)
;;       (- (:upper-size-ratio 0.8)
;; 	 main imenu)
;;          ;; (| (:right-max-size 20)
;;          ;;    main imenu)
;;          sub))

;; (global-set-key (kbd "C-l C-l C-s") `e2wm:start-management)
;; (global-set-key (kbd "C-l C-l C-e") `e2wm:stop-management)

(provide 'init-window)