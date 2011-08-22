;;; init-window.el --- window manage setting.

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, manage, window
;; creation time: Sat Jun 19 15:38:16 2010
;;; Commentary:

;;; Code:

;; (auto-install-from-url "http://github.com/kiwanami/emacs-window-layout/raw/master/window-layout.el")
;; (auto-install-from-url "http://github.com/kiwanami/emacs-window-manager/raw/master/e2wm.el")

(my-require 'e2wm)

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

(global-set-key (kbd "C-c ; i") `e2wm:start-management)
(global-set-key (kbd "C-c ; o") `e2wm:stop-management)
(global-set-key (kbd "C-c ; c") `e2wm:dp-code)
(global-set-key (kbd "C-c ; t") `e2wm:dp-two)
(global-set-key (kbd "C-c ; d") `e2wm:dp-dashboard)

;; (e2wm:add-keymap 
;;  e2wm:pst-minor-mode-keymap
;;  '(("<M-left>" . e2wm:dp-code) ; codeへ変更
;;    ("<M-right>"  . e2wm:dp-two)  ; twoへ変更
;;    ("<M-up>"    . e2wm:dp-doc)  ; docへ変更
;;    ("<M-down>"  . e2wm:dp-dashboard) ; dashboardへ変更
;;    ("C-."       . e2wm:pst-history-forward-command) ; 履歴を進む
;;    ("C-,"       . e2wm:pst-history-back-command) ; 履歴をもどる
;;    ("prefix L"  . ielm)
;;    ("M-m"       . e2wm:pst-window-select-main-command)
;;    ) e2wm:prefix-key)

(provide 'init-window)
