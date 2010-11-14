;;; init-keybindings.el --- key binding

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, keybinds
;; creation time: Wed Apr 28 00:33:52 2010
;;; Commentary:

;;; Code:

(require 'init-my-misc)

;;;-------------------------------
;;; delete, mark, kill some ranges.
;;;-------------------------------
(require 'generic-range-opt)

;;標準操作
(global-set-key (kbd "C-f") 'forward-char)
(global-set-key (kbd "C-b") 'backward-char)
(global-set-key (kbd "C-t") 'my-forward-word)
(global-set-key (kbd "C-m") 'my-backward-word)
(global-set-key (kbd "C-;") 'my-scroll-down)
(global-set-key (kbd "C-v") 'my-scroll-up)
(global-set-key (kbd "C-a") 'back-to-indentation)
(global-set-key (kbd "C-S-t") 'forward-sexp)
(global-set-key (kbd "C-S-m") 'backward-sexp)
(global-set-key (kbd "C-x C-[") 'beginning-of-buffer)
(global-set-key (kbd "C-x C-]") 'end-of-buffer)
(global-set-key (kbd "C-j") 'newline-and-indent)
(global-set-key (kbd "C-h") 'delete-backward-char)
;; killではなくてdeleteに削除コマンドを変更
(global-set-key (kbd "C-k") 'my-delete-line-forward)

;; (global-set-key (kbd "C-l C-z") 'toggle-input-method) SKKへ

(global-set-key (kbd "C-z") 'undo)
(require 'redo)
(global-set-key (kbd "C-/") 'redo)
(global-set-key (kbd "C-S-z") 'redo)

(require 'ibuffer)
(global-set-key (kbd "C-x b") 'ibuffer-list-buffers) ;;バッファウィンドウを別ウィンドウに出してフォーカスを写す
(global-set-key (kbd "C-x C-b") 'ibuffer) ;;バッファウィンドウを現在ウィンドウに出す
;; (global-set-key (kbd "C-x b") 'buffer-menu-other-window) ;;バッファウィンドウを別ウィンドウに出してフォーカスを写す
;; (global-set-key (kbd "C-x C-b") 'buffer-menu) ;;バッファウィンドウを現在ウィンドウに出す

(global-set-key (kbd "C-x d") 'dired-other-window)
(global-set-key (kbd "C-x C-d") 'dired)

(global-set-key (kbd "C-x k") 'kill-buffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer)

(global-set-key (kbd "C-^") 'my-other-window-or-split)
;; (global-set-key [C-tab] 'other-window) ;; window 切り替え
;; (global-set-key (kbd "C-x o") 'other-window)
;; (global-set-key (kbd "C-x p") '(lambda (arg) (interactive "p") (other-window (- arg))))

(global-set-key (kbd "C-:") 'execute-extended-command)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-S-s") 'isearch-forward)
(global-set-key (kbd "C-S-r") 'isearch-backward)

;;; インクリメンタルサーチ中にバックスペースが使えるように
(define-key isearch-mode-map "\C-h" 'isearch-del-char)
(defun my-isearch-exit-and-move-backward-sexp ()
  "インクリメンタルサーチを終了させて現在位置からbackward-sexp"
  (interactive)
  (isearch-exit)
  (backward-sexp))
(defun my-isearch-exit-and-move-mark-string-begin ()
  "インクリメンタルサーチを終了させて現在マークしている文字列の先頭へ"
  (interactive)
  (isearch-exit)
  (goto-char (- (point) (length isearch-string))))

(define-key isearch-mode-map (kbd "C-j") 'my-isearch-exit-and-move-backward-sexp)
(define-key isearch-mode-map (kbd "C-u") 'my-isearch-exit-and-move-mark-string-begin)

(global-set-key (kbd "C-.") 'my-just-one-space-toggle)

;; (global-set-key (kbd "C-w") 'kill-word*)
(global-set-key (kbd "C-w") 'gro-kill-follow-word)
;; (global-set-key (kbd "C-S-w") 'my-delete-region-or-follow-kill-word)

;;;-------------------------------
;;; 標準操作
;;;-------------------------------
;; (global-set-key (kbd "C-l C-x") 'execute-extended-command)

;; window 切り替え
(global-set-key (kbd "C-l C-n") 'other-window)
(global-set-key (kbd "C-l C-p") #'(lambda (arg) (interactive "p") (other-window (- arg))))

;; window 操作
(global-set-key (kbd "C-l C-1") 'delete-other-windows)
(global-set-key (kbd "C-l C-2") 'split-window-vertically)
(global-set-key (kbd "C-l C-3") 'split-window-horizontally)
(global-set-key (kbd "C-l C-0") 'delete-window)

(global-set-key (kbd "C-l C-w") 'my-copy-region-or-follow-word) ;;copy
(global-set-key (kbd "C-l C-k") 'kill-whole-line) ;;line kill
(global-set-key (kbd "C-l C-d C-f") 'my-delete-forward-word)
(global-set-key (kbd "C-l C-d C-b") 'my-delete-backward-word)
(global-set-key (kbd "C-l C-q") 'my-match-paren)
(global-set-key (kbd "C-l C-c") 'calculator)
(global-set-key (kbd "C-l C-e") 'my-eval-buffer-or-region)

(global-set-key (kbd "C-l C-g")  'goto-line) ;;指定した行へ。

;; explorer で 開く
(global-set-key (kbd "C-l C-f C-e") 'my-open-directory-by-explorer)
;; windowsの関連付けで開く
(global-set-key (kbd "C-l C-f C-w") 'my-buffer-cygstart-exe)
;; windowsの関連付けでopen-file
(global-set-key (kbd "C-l C-f C-f") 'my-open-file-os)
;;ソースとヘッダファイルの移動用
(global-set-key (kbd "C-l C-f C-s") 'ff-find-other-file)

(global-set-key (kbd "C-l C-f C-o") 'my-save-all-buffers)

;;;-------------------------------
;;; frame alpha
;;;-------------------------------
(defun my-frame-alpha-setting (arg)
  "アルファの値の設定関数"
  (interactive "P")
  (cond
   ((equal arg '(8192)) (set-frame-parameter nil 'alpha 5))	;;C-u C-u C-u C-u C-u C-u C-u
   ((equal arg '(4096)) (set-frame-parameter nil 'alpha 10))	;;C-u C-u C-u C-u C-u C-u
   ((equal arg '(2048)) (set-frame-parameter nil 'alpha 25))	;;C-u C-u C-u C-u C-u
   ((equal arg '(1024)) (set-frame-parameter nil 'alpha 50))	;;C-u C-u C-u C-u
   ((equal arg '(256)) (set-frame-parameter nil 'alpha 60))	;;C-u C-u C-u C-u
   ((equal arg '(64)) (set-frame-parameter nil 'alpha 70))	;;C-u C-u C-u
   ((equal arg '(16))  (set-frame-parameter nil 'alpha 80)) 	;;C-u -u
   ((equal arg '(4)) (set-frame-parameter nil 'alpha 90))	;;C-u
   (t (set-frame-parameter nil 'alpha 100))))

;; 現在フレームの透明度変更
;; (define-key global-map (kbd "C-l C-j") `(lambda () (interactive) (set-frame-parameter nil 'alpha 30)))
;; (define-key global-map (kbd "C-l C-h") `(lambda () (interactive) (set-frame-parameter nil 'alpha 100)))

(define-key global-map (kbd "C-l C-z") `my-frame-alpha-setting)

;; 重複行削除
;; (require 'uniq)
(define-key global-map (kbd "C-l C-t C-d") 'uniq-remove-dup-lines)

;;大文字小文字変換
(global-set-key (kbd "C-q C-u") 'my-changecase-word)

;;;-------------------------------
;;; mocccur 置換用
;;;-------------------------------
(require 'color-moccur)
(global-set-key (kbd "C-q C-l C-s") 'occur-by-moccur) ;;現在バッファを検索
(global-set-key (kbd "C-q C-l C-g") 'moccur-grep)
(global-set-key (kbd "C-q C-l C-f") 'moccur-grep-find)
(global-set-key (kbd "C-q C-l C-d") 'dmoccur)
(global-set-key (kbd "C-q C-l C-c") 'clean-dmoccur-buffers)

(setq moccur-split-word t)
(setq *moccur-buffer-name-exclusion-list*
	  '(".+TAGS.+" "*Completions*" "*Messages*"
		"newsrc.eld" " *migemo*" ".bbdb"))

(setq dmoccur-list
      '(
	("cd" default-directory (".*") nil)
	))

(set 'dmoccur-use-list t)
(set 'dmoccur-maximum-size 1000)

;;;-------------------------------
;;; undo-tree
;;; redo bug??
;;;-------------------------------
;; (require 'undo-tree)
;; (global-undo-tree-mode)
;; (global-set-key (kbd "C-l C-u C-t") `undo-tree-visualize)
;; (define-key undo-tree-visualizer-map (kbd "C-g") `undo-tree-visualizer-quit)

;; Key bindings (describe-bindings)
;; C-b		undo-tree-visualize-switch-branch-left
;; C-f		undo-tree-visualize-switch-branch-right
;; C-n		undo-tree-visualize-redo
;; C-p		undo-tree-visualize-undo
;; C-q		undo-tree-visualizer-quit
;; ,		undo-tree-visualizer-scroll-left
;; .		undo-tree-visualizer-scroll-right
;; <		undo-tree-visualizer-scroll-left
;; >		undo-tree-visualizer-scroll-right
;; b		undo-tree-visualize-switch-branch-left
;; f		undo-tree-visualize-switch-branch-right
;; n		undo-tree-visualize-redo
;; p		undo-tree-visualize-undo
;; q		undo-tree-visualizer-quit
;; t		undo-tree-visualizer-toggle-timestamps
;; <down>	undo-tree-visualize-redo
;; <left>	undo-tree-visualize-switch-branch-left
;; <mouse-1>	undo-tree-visualizer-set

;; ブックマーク設定
(global-set-key (kbd "C-q C-b C-m") 'bookmark-set)
(global-set-key (kbd "C-q C-b C-j") 'bookmark-jump)

(global-set-key (kbd "C-q C-t C-t") 'toggle-case-fold-search)

(global-set-key (kbd "C-q C-q") 'quoted-insert)       ;;元のコマンド
(global-set-key (kbd "C-q C-@") 'yalinum-mode)        ;;行番号表示
(global-set-key (kbd "C-q C-w") 'gro-copy-follow-word) ;;copy
;; (global-set-key (kbd "C-q C-w") 'copy-region-as-kill) ;;copy
(global-set-key (kbd "C-q C-h") 'help-for-help)	      ;;ヘルプ
(global-set-key (kbd "C-q C-;") 'view-mode)

;; text edit.
(global-set-key (kbd "C-q C-c") 'comment-or-uncomment-region) ;;コメント付加、解除
(global-set-key (kbd "C-q C-t C-a") 'align-regexp)	      ;;特定文字での整列
(global-set-key (kbd "C-q C-t C-r")  'query-replace-regexp)   ;;置換
;; (global-set-key (kbd "C-q C-t C-o") 'overwrite-mode) ;;
(global-set-key (kbd "C-q C-t C-t") 'tabify)                  ;; tab化
(global-set-key (kbd "C-q C-t C-u") 'untabify)                ;; untab化
;; (global-set-key (kbd "C-q C-t C-i")  'indent-region) ;;インデント

;; Perform general cleanup.
(global-set-key (kbd "C-q C-d C-b") 'clean-buffer-list)

(global-set-key (kbd "C-q C-k") 'my-delete-line-backward)

;;;-------------------------------
;;; delete, mark, kill some ranges.
;;;-------------------------------
(require 'generic-range-opt)

(defun my-delete-char (arg)
  (interactive "P")
  (if arg
      (let ((key (read-event "gro-delete-opt: ")))
	(cond
	 ((eq key ?\^F) (gro-delete-forward-line))
	 ((eq key ?\^B) (gro-delete-backward-line))
	 ((eq key ?\^D) (gro-delete-current-line))
	 ((eq key ?\^E) (gro-delete-follow-sexp))
	 ((eq key ?\^N) (gro-delete-next-line))
	 ((eq key ?\^P) (gro-delete-prev-line))
	 ((eq key ?\^L) (gro-delete-goto-line))
	 ((eq key ?\^ ) (gro-delete-between-jaunte))
	 ((eq key ?\^C) (gro-delete-yafastnav-prev))))
    (delete-char 1)))

(global-set-key (kbd "C-d") 'my-delete-char)

;; ;; マイナーモードの定義
;; (easy-mmode-define-minor-mode gro-delete-mode
;;  ;; ドキュメント
;;    "This is Generic Range Operation for delete Mode."
;;  ;; 初期値
;;  nil
;;  ;; on の時のモード行への表示
;;  " GROd"
;;  ;; マイナーモード用キーマップの初期値
;;  '(keymap
;;    ("C-f" . gro-delete-forward-line)
;;    ("C-b" . gro-delete-backward-line)
;;    ) 
;;  )

;; (global-unset-key (kbd "C-d"))
;; (global-set-key (kbd "C-d C-j") 'delete-char)
;; (global-set-key (kbd "C-d C-i") 'gro-delete-follow-word)
;; (global-set-key (kbd "C-d C-f") 'gro-delete-forward-line)
;; (global-set-key (kbd "C-d C-b") 'gro-delete-backward-line)
;; (global-set-key (kbd "C-d C-s") 'gro-delete-search-forward-char)
;; (global-set-key (kbd "C-d C-r") 'gro-delete-search-backward-char)
;; (global-set-key (kbd "C-d C-e") 'gro-delete-follow-sexp)
;; (global-set-key (kbd "C-d C-w") 'gro-delete-follow-string)
;; (global-set-key (kbd "C-d C-d") 'gro-delete-current-line)
;; (global-set-key (kbd "C-d C-n") 'gro-delete-next-line)
;; (global-set-key (kbd "C-d C-p") 'gro-delete-prev-line)
;; (global-set-key (kbd "C-d C-h") 'gro-delete-jaunte-prev)
;; (global-set-key (kbd "C-d C-l") 'gro-delete-goto-line)
;; (global-set-key (kbd "C-d C-SPC") 'gro-delete-between-jaunte)
;; (global-set-key (kbd "C-d C-x") 'gro-delete-between-yafastnav)
;; (global-set-key (kbd "C-d C-c") 'gro-delete-yafastnav-prev)

(global-unset-key (kbd "C-8"))
(global-set-key (kbd "C-8 C-i") 'gro-mark-follow-word)
(global-set-key (kbd "C-8 C-f") 'gro-mark-forward-line)
(global-set-key (kbd "C-8 C-b") 'gro-mark-backward-line)
(global-set-key (kbd "C-8 C-s") 'gro-mark-search-forward-char)
(global-set-key (kbd "C-8 C-r") 'gro-mark-search-backward-char)
(global-set-key (kbd "C-8 C-e") 'gro-mark-follow-sexp)
(global-set-key (kbd "C-8 C-w") 'gro-mark-follow-string)
(global-set-key (kbd "C-8 C-8") 'gro-mark-current-line)
(global-set-key (kbd "C-8 C-n") 'gro-mark-next-line)
(global-set-key (kbd "C-8 C-p") 'gro-mark-prev-line)
(global-set-key (kbd "C-8 C-h") 'gro-mark-jaunte-prev)
(global-set-key (kbd "C-8 C-d") 'gro-mark-defun*)

;; エラー箇所へのジャンプ用
(global-set-key (kbd "C-l C-;") 'compilation-minor-mode)
(define-key compilation-minor-mode-map (kbd "C-c C-c") 'comint-interrupt-subjob)

(provide 'init-keybindings)
