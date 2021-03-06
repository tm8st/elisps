;;; init-dired.el --- my dired setting.

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, dired

;;; Commentary:

;;; Code:

(my-require 'sorter)

(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)

;; dired + sorter 時に ls の -h オプションを付加する
(defadvice dired-sort-other
  (around dired-sort-other-h activate)
  (ad-set-arg 0 (concat (ad-get-arg 0) "h"))
  ad-do-it
  (setq dired-actual-switches (dired-replace-in-string "h" "" dired-actual-switches)))
(ad-activate 'dired-sort-other)

;; 今日変更したファイル表示
(when use-gui-setting
  (defface my-face-dired-tody-change '((t (:foregound "Orange"))) nil :group 'my)

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
  )

;; フック
(add-hook 'dired-load-hook
          '(lambda ()
             (my-require 'ls-lisp)
	     (setq ls-lisp-dirs-first t)
             (setq dired-listing-switches "-oXaF") ;; 
             ;; (setq find-ls-option '("-exec ls -AFGl {} \\;" . "-AFGl"))
             ;; (setq grep-find-command "find . -type f -print0 | xargs -0 -e grep -ns ")
             (my-require 'wdired)
             ))

;; 色づけ
(my-require 'dired)
(defvar *original-dired-font-lock-keywords* dired-font-lock-keywords)
(defun dired-highlight-by-extensions (highlight-list)
  "highlight-list accept list of (regexp [regexp] ... face)."
  (let ((lst nil))
    (dolist (highlight highlight-list)
      (push `(,(concat "\\.\\(" (regexp-opt (butlast highlight)) "\\)$")
              (".+" (dired-move-to-filename)
               nil (0 ,(car (last highlight)))))
            lst))
    (setq dired-font-lock-keywords
          (append *original-dired-font-lock-keywords* lst))))

(dired-highlight-by-extensions
  (list
    (append my-source-file-extention-list (list font-lock-builtin-face))
    (append my-exe-file-extention-list (list font-lock-string-face))
    (append my-music-file-extention-list (list font-lock-keyword-face))
    (append my-archive-file-extention-list (list font-lock-comment-face))
    (append my-doc-file-extention-list (list font-lock-doc-face))
    ))

(my-require 'init-misc)
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
			 (my-require 'wdired)
			 ))

;;; wdired
(my-require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; key binds
(define-key dired-mode-map (kbd "C-c C-p") 'dired-up-directory)
(define-key dired-mode-map (kbd "C-c C-n") 'dired-create-directory)
(define-key dired-mode-map (kbd "C-c C-r") 'dired-do-rename)
(define-key dired-mode-map (kbd "C-c C-m") 'dired-do-copy)
(define-key dired-mode-map (kbd "C-c C-z") 'dired-do-shell-command)
(define-key dired-mode-map (kbd "C-c C-j") 'dired-do-async-shell-command)
(define-key dired-mode-map (kbd "C-c C-d") 'dired-do-delete)
(define-key dired-mode-map (kbd "C-c C-i") 'dired-mark-files-regexp)

(my-require 'browse-kill-ring)

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

(global-set-key (kbd "C-c k") 'browse-kill-ring)
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

(defun my-open-dired-dir-os ()
  "diredで開いているdirectoryをosの標準機能で開く"
  (interactive)
  (when (my-is-mac)
    (shell-command "open ."))
  (when (my-is-windows)
    (shell-command (concat "explorer " (dired-current-directory))))
  )

(defun my-play-dired-file ()
  "diredでカーソルがあっている音声ファイルを再生する"
  (interactive)
  (se-play (dired-file-name-at-point)))

(define-key dired-mode-map (kbd "C-c C-f") 'add-dired-file-name-to-killring)
(define-key dired-mode-map (kbd "C-c p") 'my-play-dired-file)
(global-set-key (kbd "C-c C-o") 'my-open-dired-dir-os)

(my-require `dired)
(my-require `dired-aux)

(unless my-initialized
(add-to-list 'dired-compress-file-suffixes
  '("\\.zip\\'" "" "unzip"))
  )

(defun open-file-dwim (filename)
  "Open file dwim"
  (let* ((winp (string-equal window-system "w32"))
         (opener (if (file-directory-p filename)
                     (if winp '("explorer.exe") '("open"))
                   (if winp '("fiber.exe") '("open"))))
         (fn (replace-regexp-in-string "/$" "" filename))
         (args (append opener (list (if winp
                                        (replace-regexp-in-string "/" (rx "\\") fn)
                                      fn))))
         (process-connection-type nil))
    (apply 'start-process "open-file-dwim" nil args)))

;; カーソル下のファイルやディレクトリを関連付けられたプログラムで開く
(defun my-dired-open-dwim ()
  "Open file under the cursor"
  (interactive)
  (open-file-dwim (dired-get-filename)))

;; 現在のディレクトリを関連付けられたプログラムで開く
(defun my-dired-open-here ()
  "Open current directory"
  (interactive)
  (open-file-dwim (expand-file-name dired-directory)))

;; カーソル下のファイルをパッチ適用
(defun my-dired-patch ()
  "run patch under the cursor"
  (interactive)
  (shell-command (concat "patch -p0 < " (dired-get-filename)) nil))

;; キーバインド
(add-hook 'dired-mode-hook
          (lambda ()
	    (define-key dired-mode-map (kbd "C-c C-o") 'my-dired-open-dwim)
	    (define-key dired-mode-map (kbd "C-c C-.") 'my-dired-open-here)
	    (define-key dired-mode-map (kbd "P") 'my-dired-patch)
	    ))

(provide 'init-dired)
