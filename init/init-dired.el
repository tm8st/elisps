;;; init-dired.el --- my dired setting.

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, dired

;;; Commentary:

;;; Code:

(require 'sorter)

;; dired + sorter 時に ls の -h オプションを付加する
(defadvice dired-sort-other
  (around dired-sort-other-h activate)
  (ad-set-arg 0 (concat (ad-get-arg 0) "h"))
  ad-do-it
  (setq dired-actual-switches (dired-replace-in-string "h" "" dired-actual-switches)))
(ad-activate 'dired-sort-other)

;; 今日変更したファイル表示
(defface my-face-dired-tody-change '((t (:foregound "Orarnge"))) nil :group 'my)

(defvar my-face-dired-tody-change 'my-face-dired-tody-change)
(defun my-dired-today-search (arg)
  "Fontlock search function for dired."
  (search-forward-regexp
   (concat (format-time-string "%b %e" (current-time)) " [0-9]....") arg t))

(add-hook 'dired-mode-hook
		  '(lambda ()
		     (font-lock-add-keywords
		      major-mode
		      (list
		       '(my-dired-today-search . my-face-dired-tody-change)
		       ))		     
		     ))

(defun my-dired-find-file-os ()
  "dired で選択中のファイルをOSの関連付けで開く"
  (interactive)
  (message "my-dired-find-file-os")
  (let ((find-file-run-dired t))
    (my-open-file-os (dired-get-file-for-visit))))

;; (define-key wdired-mode-map (kbd "C-j") 'my-dired-find-file-os)
;; (define-key wdired-mode-map "\C-j"     'wdired-newline)
;; (define-key wdired-mode-map (kbd "C-j") 'my-dired-find-file-os)

;;; dired::バッファを新しく開かない
(defun dired-find-alternate-file ()
  "In dired, visit this file or directory instead of the dired buffer."
  (interactive)
  (set-buffer-modified-p nil)
  (find-alternate-file (dired-get-filename)))

;;; dired::lsの環境差異をls-listで吸収したりwdiredで色々
(add-hook 'dired-load-hook
		  '(lambda ()
			 (load-library "ls-lisp")
			 (setq ls-lisp-dirs-first t)
			 (setq dired-listing-switches "-AFl")
			 (setq find-ls-option '("-exec ls -AFGl {} ＼＼;" . "-AFGl"))
			 (setq grep-find-command "find . -type f -print0 | xargs -0 -e grep -ns ")
			 (require 'wdired)
			 ))
;;; ;;; wdired
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; key binds
(define-key dired-mode-map (kbd "C-c C-p") 'dired-up-directory)
(define-key dired-mode-map (kbd "C-c C-n") 'dired-create-directory)
(define-key dired-mode-map (kbd "C-c C-r") 'dired-do-rename)
(define-key dired-mode-map (kbd "C-c C-m") 'dired-do-copy)
(define-key dired-mode-map (kbd "C-j") 'dired-do-shell-command)
(define-key dired-mode-map (kbd "C-c C-j") 'dired-do-async-shell-command)

(require 'browse-kill-ring)

(defun browse-kill-ring-insert-and-option-insert (&optional quit opt-str)
  "Insert the kill ring item at point into the last selected buffer.
If optional argument QUIT is non-nil, close the *Kill Ring* buffer as
well."
  (interactive "P")
  (browse-kill-ring-do-insert (current-buffer)
			      (point))
  (insert opt-str)
  (when quit
    (browse-kill-ring-quit)))

(global-set-key (kbd "C-l k") 'browse-kill-ring)
(define-key browse-kill-ring-mode-map (kbd "i") '(lambda () (interactive) browse-kill-ring-insert-and-option-insert(nil, "\n")))

;; kill-ring を一行で表示
(setq browse-kill-ring-display-style 'one-line)
;; browse-kill-ring 終了時にバッファを kill する
(setq browse-kill-ring-quit-action 'kill-and-delete-window)
;; 必要に応じて browse-kill-ring のウィンドウの大きさを変更する
(setq browse-kill-ring-resize-window t)
;; kill-ring の内容を表示する際の区切りを指定する
;; (setq browse-kill-ring-separator "\n---------------------")
;; 現在選択中の kill-ring のハイライトする
(setq browse-kill-ring-highlight-current-entry t)
;; 区切り文字のフェイスを指定する
(setq browse-kill-ring-separator-face 'region)
;; 一覧で表示する文字数を指定する． nil ならすべて表示される．
(setq browse-kill-ring-maximum-display-length 100)

(defun add-dired-file-name-to-killring ()
  "ファイル名のリスト作成用"
  (interactive)
  (setq kill-ring
	(append
	 (list (dired-file-name-at-point))
	 kill-ring)
	))

(define-key dired-mode-map (kbd "C-c C-f") 'add-dired-file-name-to-killring)
(global-set-key (kbd "C-c C-o") 'open-dired-dir-os)

;; ^		dired-up-directory
;; +		dired-create-directory
;; <		dired-prev-dirline
;; =		dired-diff
;; >		dired-next-dirline

(provide 'init-dired)
