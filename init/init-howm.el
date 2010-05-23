;;; init-howm.el --- howm setting

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, howm
;; creation time: Wed Apr 28 00:15:00 2010
;;; Commentary:

;;; Code:

(require 'howm-mode)

(setq howm-menu-lang 'ja)
(setq howm-directory "~/.emacs.d/howm/")

(setq auto-mode-alist
      (append
       '(("\\.howm$" . text-mode))
       auto-mode-alist))

;;;-------------------------------
;;; text-mode
;;;-------------------------------
(add-hook 'text-mode-hook
          (lambda ()
            (setq tab-stop-list '(2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80 82 84 86 88 90 92 94 96 98 100 102 104 106 108 110 112 114 116 118 120))
            (setq tab-width 4)
	    (customize-set-value 'standard-indent 4)
	    (setq indent-tabs-mode t)))

(global-set-key (kbd "C-l C-i") 'indent-region) ; 選択範囲をインデント
;; (global-set-key "¥C-m" 'newline-and-indent) ; リターンで改行とインデント
;; (global-set-key "¥C-j" 'newline)            ; 改行

;; リンクをタブでたどる
(eval-after-load "howm-mode"
  '(progn
     (define-key howm-mode-map [tab] 'action-lock-goto-next-link)
     (define-key howm-mode-map [(meta tab)] 'action-lock-goto-previous-link)))

(setq howm-list-recent-title t)	;; 最近のメモ一覧でタイトル表示
(setq howm-list-all-title t)	;; 全メモ一覧でタイトル表示
(setq howm-menu-expiry-hours 2) ;;メニューを二時間キャッシュ
;; (add-hook 'howm-mode-on-hook '(lambda () (auto-fill-mode nil))) ;;auto-fill
(setq howm-view-summary-persistent nil)		;; ファイルを開く時、バッファを消す
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

(fset 'my-howm-todo-done
   "\C-a\C-m\C-s+\C-f\C-b\C-h.\C-c\C-c\C-n")

(eval-after-load "howm"
  '(progn
	 (define-key howm-mode-map (kbd "C-c C-c") 'my-save-and-kill-buffer)
	 (define-key howm-mode-map (kbd "C-c C-l") 'my-howm-add-link)
	 (define-key howm-mode-map (kbd "C-c C-k") 'my-howm-add-keyword)
	 (define-key howm-mode-map (kbd "C-c C-m") 'my-howm-add-mark)
	 (define-key howm-mode-map (kbd "C-c C-t") 'my-howm-add-todo-switch)
	 (define-key howm-mode-map (kbd "C-c C-d") 'my-howm-add-todo)
	 (define-key howm-mode-map (kbd "C-c C-j") 'my-howm-todo-done)
	 ))

;; カレンダーで日付入力
(eval-after-load "calendar"
  '(progn
     (define-key calendar-mode-map "\C-m" 'my-insert-day)
     (defun my-insert-day ()
       (interactive)
       (let ((day nil)
             (calendar-date-display-form
	      '("[" year "-" (format "%02d" (string-to-int month))
		"-" (format "%02d" (string-to-int day)) "]")))
         (setq day (calendar-date-string
                    (calendar-cursor-to-date t)))
         (exit-calendar)
         (insert (concat day "+ [TODO]"))))))

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
   ((equal arg '(256)) (howm-menu))	;;C-u C-u C-u C-u
   ;; ((equal arg '(64)) (calendar))	;;C-u C-u C-u
   ((equal arg '(64)) (my-howm-clip))	;;C-u C-u C-u
   ((equal arg '(16))  (howm-list-grep)) ;;C-u -u
   ((equal arg '(4)) (howm-list-all))	;;C-u
   (t (howm-create))))

;; ;; 行番号を挿入するように変更 リンク問題があるため一時封印改行などで変更？
;; (setq howm-template-file-format (concat howm-ref-header " %s [%d]"))
;; (defun howm-insert-template (title &optional
;;                                    previous-buffer which-template not-use-file)
;;   (let* ((beg (point))
;;          (f (buffer-file-name previous-buffer))
;;          (af (and f (howm-abbreviate-file-name f))))
;;     (insert (howm-template-string which-template previous-buffer))
;;     (let* ((date (format-time-string howm-template-date-format))
;;            (use-file (not not-use-file))
;;            (file (cond ((not use-file) "")
;;                        ((null f) "")
;;                        ((string= f (buffer-file-name)) "")
;; 					   (t (format howm-template-file-format af (my-get-buffer-line))))))
;;       (let ((arg `((title . ,title) (date . ,date) (file . ,file)))
;;             (end (point-marker)))
;;         (howm-replace howm-template-rules arg beg end)
;;         end))))

(global-set-key (kbd "C-q C-,") 'my-howm-command)
(global-set-key (kbd "C-l C-,") 'howm-menu)

;; 色設定
(when use-gui-setting  
  (set-face-foreground 'howm-view-hilit-face "pink") ;; 検索時のヒット文字列
  (set-face-background 'howm-view-name-face "grey54")
  (set-face-foreground 'howm-reminder-today-face "gray60")

  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(howm-reminder-today-face ((t (:background "Gray40" :foreground "Pink"))))))
(provide 'init-howm)
