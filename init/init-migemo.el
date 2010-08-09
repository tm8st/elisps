;;; init-migemo.el --- init migemo

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, migemo
;; creation time: Wed Apr 28 00:55:04 2010
;;; Commentary:

;;; Code:

;;;-------------------------------
;;; migemo ローマ字で日本語検索
;;;-------------------------------
(require 'migemo)
(customize-set-value 'migemo-directory (expand-file-name "~/elip/misc/migemo-0.40"))
(migemo-init)

;; (setq migemo-command "ruby")

;; ;; 基本設定
;; (setq migemo-command "CMIGEMO")
;; (setq migemo-options '("-q" "--emacs"))
;; ;; migemo-dict のパスを指定
;; (setq migemo-dictionary "~/bin/cmigemo-1.1.013/dict")

;; (load-library "migemo")
;; (setq migemo-directory "~/elip/migemo")

;; ;; cache on.
;; (setq migemo-use-pattern-alist t)
;; (setq migemo-use-frequent-pattern-alist t)

;; ;; cmigemo
;; (setq migemo-command "cmigemo")
;; (setq migemo-options '("-q" "--emacs" "-i" "\a"))
;; (setq migemo-dictionary "somewhere/migemo/euc-jp/migemo-dict")
;; (setq migemo-user-dictionary nil)
;; (setq migemo-regex-dictionary nil)

;; (setq migemo-user-dictionary nil)
;; (setq migemo-regex-dictionary nil)

;; ;; キャッシュ機能を利用する
;; (setq migemo-use-pattern-alist t)
;; (setq migemo-use-frequent-pattern-alist t)
;; (setq migemo-pattern-alist-length 1024)
;; 辞書の文字コードを指定．
;; (setq migemo-coding-system 'utf-8)

(global-set-key (kbd "C-S-s") 'migemo-forward)
(global-set-key (kbd "C-S-r") 'migemo-backward)

(provide 'init-migemo)
