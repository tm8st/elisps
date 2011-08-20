;;; init-howm.el --- howm setting

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, howm
;; creation time: Wed Apr 28 00:15:00 2010
;;; Commentary:

;;; Code:

(require 'howm)

(setq howm-menu-lang 'ja)
(setq howm-directory "~/.emacs.d/howm/")

(setq auto-mode-alist
      (append
       '(("\\.howm$" . howm-mode))
       auto-mode-alist))

;; easy-imenu
(require 'easy-imenu-index-generator-config)
(add-hook
 'howm-mode-hook
 (lambda ()
   (easy-imenu-index-generator-set-for-current-buffer easy-imenu-index-generator-howm)
     ))

(global-set-key (kbd "C-l C-i") 'indent-region) ; 選択範囲をインデント
;; (global-set-key "¥C-m" 'newline-and-indent) ; リターンで改行とインデント
;; (global-set-key "¥C-j" 'newline)            ; 改行

;; リンクをタブでたどる
(eval-after-load "howm-mode"
  '(progn
     (define-key howm-mode-map [tab] 'action-lock-goto-next-link)
     (define-key howm-mode-map [(meta tab)] 'action-lock-goto-previous-link)))

(setq howm-list-recent-title t) ;; 最近のメモ一覧でタイトル表示
(setq howm-list-all-title t)    ;; 全メモ一覧でタイトル表示
(setq howm-menu-expiry-hours 2) ;;メニューを二時間キャッシュ
;; (add-hook 'howm-mode-on-hook '(lambda () (auto-fill-mode nil))) ;;auto-fill
(setq howm-view-summary-persistent nil)         ;; ファイルを開く時、バッファを消す
(setq howm-content-from-region t) ;; リージョン選択中であればそれを内容にする
;; メニューの表示範囲
(setq howm-menu-schedule-days-before 20)
(setq howm-menu-schedule-days 20)
;; howm の ファイル名フォーマット
;; (setq howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.howm") ;; １メモ１ファイル
(setq howm-file-name-format "%Y/%m/%Y-%m-%d.howm") ;; １日１ファイル
;;
(setq howm-view-grep-parse-line
      "^\\(\\([a-zA-Z]:/\\)?[^:]*\\.howm\\):\\([0-9]*\\):\\(.*\\)$")
;; 検索しないファイルの正規表現
(setq howm-excluded-file-regexp
      "/\\.#\\|[~#]$\\|\\.bak$\\|/CVS/\\|\\.doc$\\|\\.pdf$\\|\\.ppt$\\|\\.xls$")

;; 内容が0であればファイルを削除
(if (not (memq 'delete-file-if-no-contents after-save-hook))
    (setq after-save-hook
          (cons 'delete-file-if-no-contents after-save-hook)))
;;
(defun delete-file-if-no-contents ()
  (when (and
         (buffer-file-name (current-buffer))
         (string-match "\\.howm" (buffer-file-name (current-buffer)))
         (= (point-min) (point-max)))
    (delete-file
     (buffer-file-name (current-buffer)))))

;; http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?SaveAndKillBuffer
(defun my-save-and-kill-buffer ()
  (interactive)
  (when (and
         (buffer-file-name)
         (string-match "\\.howm"
                       (buffer-file-name)))
    (save-buffer)
    (kill-buffer nil)))

(defun my-howm-add-link ()
  "add link."
  (interactive)
  (insert (concat ">>> " (read-string "Link:")))
  )
(defun my-howm-add-keyword ()
  "add link."
  (interactive)
  (insert (concat "<<< " (read-string "Keyword:")))
  )
(defun my-howm-add-mark ()
  "add mark."
  (interactive)
  (insert "{-}")
  )

(defun my-howm-add-todo-switch ()
  "add todo-switch."
  (interactive)
  (insert "{_}")
  )

(defun my-howm-add-todo ()
  "add todo."
  (interactive)
  (calendar)
  )

(eval-after-load "howm"
  '(progn
     (define-key howm-mode-map (kbd "C-c C-c") 'my-save-and-kill-buffer)
     (define-key howm-mode-map (kbd "C-c C-l") 'my-howm-add-link)
     (define-key howm-mode-map (kbd "C-c C-k") 'my-howm-add-keyword)
     (define-key howm-mode-map (kbd "C-c C-m") 'my-howm-add-mark)
     (define-key howm-mode-map (kbd "C-c C-t") 'my-howm-add-todo-switch)
     (define-key howm-mode-map (kbd "C-c C-d") 'my-howm-add-todo)
     ))

(require 'calendar)
;; (setq calendar-setup 'one-frame)
(setq calendar-setup nil)

;; キーの設定
(define-key calendar-mode-map "f" 'calendar-forward-day)
(define-key calendar-mode-map "n" 'calendar-forward-day)
(define-key calendar-mode-map "b" 'calendar-backward-day)

;; 祝日をマークする
(setq calendar-mark-holidays-flag t)
(require 'japanese-holidays)
(setq calendar-holidays
      (append japanese-holidays holiday-local-holidays holiday-other-holidays))

;; 今日をマークする
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

;; 日曜日をマークする
;; (setq calendar-weekend-marker 'diary)
;; (add-hook 'today-visible-calendar-hook 'calendar-mark-weekend)
;; (add-hook 'today-invisible-calendar-hook 'calendar-mark-weekend)

;; カレンダーで日付入力
(eval-after-load "calendar"
  '(progn
     (define-key calendar-mode-map "\C-m" 'my-insert-day)
     (defun my-insert-day ()
       (interactive)
       (save-excursion
         (let ((day nil)
               (calendar-date-display-form
                '("[" year "-" (format "%02d" (string-to-int month))
                  "-" (format "%02d" (string-to-int day)) "]")))
           (setq day (calendar-date-string
                      (calendar-cursor-to-date t)))
           (exit-calendar)
           (insert (concat day "+ [TODO]"))
           )))))

(setq howm-menu-refresh-after-save nil)
(setq howm-refresh-after-save nil)

(defun my-howm-clip ()
  "名前をつけて範囲選択した部分を切り取り、名前をつけて前のバッファに戻る"
  (interactive)
  (let ((buf (current-buffer)))
    (let ((title (read-string "Memo Title:" "")))
      (howm-create)
      (insert title)
      (set-window-buffer nil buf)
      )))

(defun my-howm-command (arg)
  "howm用コマンドのまとめ関数。 C-uした回数で呼び変え"
  (interactive "P")
  (cond
   ((equal arg '(256)) (howm-menu))      ;;C-u C-u C-u C-u
   ((equal arg '(64)) (calendar))        ;;C-u C-u C-u
   ((equal arg '(16))  (howm-list-grep)) ;;C-u -u
   ((equal arg '(4)) (my-howm-clip))     ;;C-u
   (t (howm-create))))

(global-set-key (kbd "C-q C-,") 'my-howm-command)
(global-set-key (kbd "C-l C-,") 'howm-menu)

;; 色設定
(when use-gui-setting
  (set-face-foreground 'howm-view-hilit-face "white") ;; 検索時のヒット文字列
  (set-face-background 'howm-view-name-face "black")
	(set-face-background 'howm-reminder-today-face "black")
	(set-face-foreground 'howm-reminder-today-face "Pink")
	(set-face-background 'howm-reminder-tomorrow-face "gray70")
	(set-face-foreground 'howm-reminder-tomorrow-face "Pink")
  )

;; 内容バッファにも下線 (もちろん RET も効く)
(add-hook 'howm-view-contents-mode-hook
          (lambda ()
            (setq default-directory howm-directory)
            (howm-mode 1)))
(defadvice riffle-contents-show (around howm-mode (item-list) activate)
  ad-do-it
  (when howm-mode
    (howm-initialize-buffer)))

;; C-i・M-C-i を「次・前の下線へ」に変更
(let ((m howm-view-contents-mode-map))
  (define-key m "\C-i" 'action-lock-goto-next-link)
  (define-key m "\M-\C-i" 'action-lock-goto-previous-link))

;; howm todo grep-find用コマンドの設定/ windowsでfindコマンドを別名にしているため。
(defvar my-homw-todo-grep-find-command nil)
(if (my-is-windows)
    (setq my-homw-todo-grep-find-command "gnufind . -name \"*.howm\" -a -type f -a -not -name \"*.svn*\" -a -not -name \"*.bin\" -exec grep -ni -e \"\[\\[0-9-\\]\\\]+\" {} +")
  (setq my-homw-todo-grep-find-command "find . -name \"*.howm\" -a -type f -a -not -name \"*.svn*\" -a -not -name \"*.bin\" -exec grep -ni -e \"\[\\[0-9-\\]\\\]+\" {} +")
  )
(defvar my-homw-done-grep-find-command nil)
(if (my-is-windows)
    (setq my-homw-done-grep-find-command "gnufind . -name \"*.howm\" -a -type f -a -not -name \"*.svn*\" -a -not -name \"*.bin\" -exec grep -ni -e \"\[\\[0-9-\\]\\\]\\.\" {} +")
  (setq my-homw-done-grep-find-command "find . -name \"*.howm\" -a -type f -a -not -name \"*.svn*\" -a -not -name \"*.bin\" -exec grep -ni -e \"\[\\[0-9-\\]\\\]\\.\" {} +")
  )

;; howm-directory以下だとhowmのgrepに引っ掛るので、別の場所におく。 
(defvar my-howm-todo-grep-result-directory "~/.emacs.d/howm-todo-dayly-result/")

(defun my-howm-todo-grep-find-output-filename (is-morning)
  (expand-file-name (format "%s%s/howm-tood-%s-%s.log"
			    my-howm-todo-grep-result-directory
			    (format-time-string "%Y" (current-time))
			    (format-time-string "%m-%d" (current-time))
			    (if is-morning
				"morning"
			      "night"))))

(defun my-howm-todo-grep-find-morning ()
  "grep howm todo."
  (interactive)
  (my-howm-todo-grep-find-inner t))

(defun my-howm-todo-grep-find-night ()
  "grep howm todo."
  (interactive)
  (my-howm-todo-grep-find-inner nil)
  ;; diff
  (diff (my-howm-todo-grep-find-output-filename t)
	 (my-howm-todo-grep-find-output-filename nil) "-u")
  )

(defun my-howm-todo-grep-find-inner (is-morning)
  "grep howm todo."
  (interactive)
  (save-excursion
    (let ((buf (get-buffer "*Shell Command Output*"))
	  (output-filename (my-howm-todo-grep-find-output-filename is-morning)))
      (shell-command (concat "mkdir -p " (file-name-directory output-filename)
			     " && " "cd " howm-directory
			     " && " my-homw-todo-grep-find-command
			     " && " my-homw-done-grep-find-command) buf)
      (set-buffer buf)
      (write-region (point-min) (point-max) output-filename nil nil nil nil))))

(global-set-key (kbd "C-l C-u C-m") 'my-howm-todo-grep-find-morning)
(global-set-key (kbd "C-l C-u C-n") 'my-howm-todo-grep-find-night)

(define-key howm-mode-map (kbd "C-c C-j") 'my-howm-currentline-todo-toggle)

;;;-------------------------------
;;; howmのTODOリストをmoccur-editで編集
;;;-------------------------------
(require 'howm-mode)
(require 'color-moccur)
(require 'moccur-edit)

(defvar my-howm-active-todo-regexp (concat "\\[" howm-date-regexp "\\]\\([-~!@\+]\\)")
  "howmのtodoの正規表現")

(defvar my-howm-sleeping-todo-regexp (concat "\\[" howm-date-regexp "\\]\\.")
  "howmのsleeping-todoの正規表現")

(defun my-howm-active-todo-moccur ()
  (interactive)
  "howmのactive-todoをリストアップ。"
  (let ((moccur-display-result-buffer-filename t))
    (moccur-grep-find
     howm-directory
     (list my-howm-active-todo-regexp ".howm"))))

(defun my-howm-currentline-todo-toggle ()
  "同一行にあるhowmのtodoマークのアクティブ(+)、スリープ(.)をトグル。"
  (interactive)
  (save-excursion
    (goto-char (line-beginning-position))
    (when (re-search-forward my-howm-active-todo-regexp (line-end-position) t)
      (delete-backward-char 1)
      (insert "."))
    (when (re-search-forward my-howm-sleeping-todo-regexp (line-end-position) t)
      (delete-backward-char 1)
      (insert "+"))))

(defun my-howm-save-all-buffers ()
  "すべてのhowmバッファの保存"
  (interactive)
  (save-excursion
    (dolist (buf (buffer-list))
      (when (and
	     (buffer-file-name buf)
	     (string-match "\\.howm" (buffer-file-name buf))
	     (file-writable-p (buffer-file-name buf)))
	(set-buffer buf)
	(save-buffer)))))

(defun my-howm-moccur-all-save-and-kill-buffer ()
  "編集終了後に全変更の適用してMoccurバッファを閉じる。"
  (interactive)
  (moccur-edit-finish-edit)
  (my-howm-save-all-buffers)
  (kill-buffer "*Moccur*"))

(defun my-moccur-all-save-and-kill-buffer ()
  "編集終了後に全変更の適用してMoccurバッファを閉じる。"
  (interactive)
  (moccur-edit-finish-edit)
  (my-save-all-buffers)
  (kill-buffer "*Moccur*"))

(global-set-key (kbd "C-l C-u C-,") 'my-howm-active-todo-moccur)

(define-key moccur-edit-mode-map (kbd "C-c C-j") 'my-howm-currentline-todo-toggle)
(define-key moccur-edit-mode-map (kbd "C-c C-h") 'my-howm-moccur-all-save-and-kill-buffer)
(define-key moccur-edit-mode-map (kbd "C-c C-e") 'my-moccur-all-save-and-kill-buffer)
(define-key moccur-edit-mode-map (kbd "C-c C-m") 'moccur-grep-goto)
(define-key moccur-edit-mode-map (kbd "C-c C-n") 'moccur-next)
(define-key moccur-edit-mode-map (kbd "C-c C-p") 'moccur-prev)

;; 色設定
(when use-gui-setting
  (set-face-foreground 'howm-mode-title-face "pink")
  (set-face-background 'moccur-face "#005400")
  (set-face-foreground 'moccur-face "orange1")
  (set-face-underline-p 'moccur-face t)
  (set-face-foreground 'moccur-edit-done-face "gray60")
  (set-face-background 'moccur-edit-done-face "gray1")
  (set-face-foreground 'moccur-edit-face "orange2")
  (set-face-background 'moccur-edit-face "gray20")
  )

(provide 'init-howm)
