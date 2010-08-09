;;; init-keybindings.el --- key binding

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, keybinds
;; creation time: Wed Apr 28 00:33:52 2010
;;; Commentary:

;;; Code:

(require 'init-my-misc)

;;標準操作
(global-set-key (kbd "C-f") 'forward-char)
(global-set-key (kbd "C-b") 'backward-char)
(global-set-key (kbd "C-t") 'my-forward-word)
(global-set-key (kbd "C-m") 'my-backward-word)
(global-set-key (kbd "C-;") 'my-scroll-down)
(global-set-key (kbd "C-v") 'my-scroll-up)
(global-set-key (kbd "C-a") 'back-to-indentation)
(global-set-key (kbd "C-x C-[") 'beginning-of-buffer)
(global-set-key (kbd "C-x C-]") 'end-of-buffer)
;; (global-set-key (kbd "C-l C-z") 'toggle-input-method)

(global-set-key (kbd "C-j") 'newline-and-indent)

(global-set-key (kbd "C-h") 'delete-backward-char)
;; killではなくてdeleteに削除コマンドを変更
(global-set-key (kbd "C-k") 'my-delete-line-forward)
(global-set-key (kbd "C-z") 'undo)
(require 'redo)
(global-set-key (kbd "C-/") 'redo)
(global-set-key (kbd "C-S-z") 'redo)
;; (global-set-key (kbd "C-x b") 'buffer-menu-other-window) ;;バッファウィンドウを別ウィンドウに出してフォーカスを写す
;; (global-set-key (kbd "C-x C-b") 'buffer-menu) ;;バッファウィンドウを現在ウィンドウに出す
(require 'ibuffer)
(global-set-key (kbd "C-x b") 'ibuffer-list-buffers) ;;バッファウィンドウを別ウィンドウに出してフォーカスを写す
(global-set-key (kbd "C-x C-b") 'ibuffer) ;;バッファウィンドウを現在ウィンドウに出す

(global-set-key (kbd "C-x d") 'dired-other-window)
(global-set-key (kbd "C-x C-d") 'dired)

(global-set-key (kbd "C-x k") 'kill-buffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer)

(global-set-key (kbd "C-^") 'my-other-window-or-split)
;; (global-set-key [C-tab] 'other-window) ;; window 切り替え
;; (global-set-key (kbd "C-x o") 'other-window)
;; (global-set-key (kbd "C-x p") '(lambda (arg) (interactive "p") (other-window (- arg))))

(global-set-key (kbd "C-:") 'execute-extended-command)

;;選択中の文字のisearch用
(defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
  (if (and transient-mark-mode mark-active)
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
;; (global-set-key (kbd "C-S-s") 'isearch-forward)
;; (global-set-key (kbd "C-S-r") 'isearch-backward)
;;; インクリメンタルサーチ中にバックスペースが使えるように
(define-key isearch-mode-map "\C-h" 'isearch-del-char)
(global-set-key (kbd "C-.") 'my-just-one-space-toggle)

(global-set-key (kbd "C-w") 'my-kill-region)
;; (global-set-key (kbd "C-S-w") 'my-delete-region-or-follow-kill-word)

;;;-------------------------------
;;; sequential-command-config
;;;-------------------------------
(require 'sequential-command)
(define-sequential-command beginning-of-anything-seq
  back-to-indentation beginning-of-line seq-return)

(define-sequential-command end-of-anything-seq
  end-of-line seq-return)

(require 'sequential-command-config)
(global-set-key (kbd "C-a") 'beginning-of-anything-seq)
(global-set-key (kbd "C-e") 'end-of-anything-seq)

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
;;; emacsから一発で検索
;;;-------------------------------
(require 'search-web)

(defun my-search-web (engine)
  (interactive)
  (let ((word (read-string "search-word:")))
    (browse-url
     (format
      (cdr (assoc engine search-engines)) (url-hexify-string word)))))

;; google
(define-key global-map (kbd "C-l C-s C-s") (lambda () (interactive) (my-search-web "g")))
;; 英辞郎
(define-key global-map (kbd "C-l C-s C-e") (lambda () (interactive) (my-search-web "eow")))
;; AMAZON
(define-key global-map (kbd "C-l C-s C-a") (lambda () (interactive) (my-search-web "zj")))
;; udn
(define-key global-map (kbd "C-l C-s C-u") (lambda () (interactive) (my-search-web "udn")))

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
(load "uniq.el" t)
(define-key global-map (kbd "C-l C-t C-d") 'uniq-remove-dup-lines)

;;;-------------------------------
;;; Profile
;;;-------------------------------
(require `elp)

(global-set-key (kbd "C-l C-a C-s") `elp-instrument-package)
(global-set-key (kbd "C-l C-a C-r") `elp-results)
(global-set-key (kbd "C-l C-a C-e") `elp-reset-all)

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

;; keyboard-macro
;; C-x (
;; キーボードマクロの定義を開始する （start-kbd-macro）。
;; C-x )
;; キーボードマクロの定義を終了する （end-kbd-macro）。
;; C-x e
;; もっとも最近のキーボードマクロを実行する （call-last-kbd-macro）。
;; C-u C-x (
;; もっとも最近のキーボードマクロを再実行したうえで、 その定義にキーを追加する。
;; C-x q
;; キーボードマクロの実行中にこの場所に到達したら、 実行の確認を求める （kbd-macro-query）。
;; M-x name-last-kbd-macro
;; もっとも最近に定義したキーボードマクロに（現在のEmacsセッションだけで有効な） コマンド名を与える。
;; M-x insert-kbd-macro
;; キーボードマクロの定義をLispコードとしてバッファに挿入する。
;; C-x C-k
;; まえに定義したキーボードマクロを編集する （edit-kbd-macro）。
;; M-x apply-macro-to-region-lines
;; リージョン内の各行に対して、最後に定義したキーボードマクロを実行する。

(defun my-last-kbd-macro-name-and-insert ()
  (interactive)
  (let ((name (read-string "Macro Name is:")))
    (name-last-kbd-macro name)
    (insert-kbd-macro name)
    ))

(global-set-key (kbd "C-q C-8") 'start-kbd-macro)
(global-set-key (kbd "C-q C-9") 'end-kbd-macro)
(global-set-key (kbd "C-q C-0") 'my-last-kbd-macro-name-and-insert)

(global-set-key (kbd "C-q C-t C-t") 'toggle-case-fold-search)

(global-set-key (kbd "C-q C-q") 'quoted-insert)        ;;元のコマンド
(global-set-key (kbd "C-q C-@") 'yalinum-mode)           ;;行番号表示
(global-set-key (kbd "C-q C-w") 'copy-region-as-kill) ;;copy
(global-set-key (kbd "C-q C-h") 'help-for-help)   ;;ヘルプ
(global-set-key (kbd "C-q C-;") 'view-mode)

;; text edit.
(global-set-key (kbd "C-q C-c") 'comment-or-uncomment-region);;コメント付加、解除
(global-set-key (kbd "C-q C-t C-a") 'align-regexp) ;;特定文字での整列
(global-set-key (kbd "C-q C-t C-r")  'query-replace-regexp);;置換
;; (global-set-key (kbd "C-q C-t C-o") 'overwrite-mode) ;;
(global-set-key (kbd "C-q C-t C-t") 'tabify) ;; tab化
(global-set-key (kbd "C-q C-t C-u") 'untabify) ;; untab化
;; (global-set-key (kbd "C-q C-t C-i")  'indent-region) ;;インデント

;;大文字小文字変換
(global-set-key (kbd "C-q C-u")  'my-changecase-word)

;; killではなくてdelete削除コマンド
(global-set-key (kbd "C-q C-k")  'my-delete-line-backward)
(global-set-key (kbd "C-q C-d C-d")  'my-delete-line)
;; Perform general cleanup.
(global-set-key (kbd "C-q C-d C-b") 'clean-buffer-list)

;;補完
;; e(global-set-key "\C-o" 'dabbrev-expand)    ;;動的補間 anything へ変更
;; (global-set-key "\C-q\C-o" 'expand-abbrev)     ;;略語展開 yasnippetへ変更
;; (global-set-key "\C-xam"   'add-mode-abbrev)   ;;MODE略語の追加
;; (global-set-key "\C-xag"   'add-global-abbrev) ;;GLOBAL略語の追加

;;; jump function
;; (require 'goto-chg)
;; (global-set-key "\C-q\C-g\C-["  'goto-last-change)
;; (global-set-key "\C-q\C-g\C-]"  'goto-last-change-reverse)

;; ;;;google gtags (ggtags)
;; (setq load-path (append (list (expand-file-name "~/elisps/ggtags")) load-path))
;; (defvar gtags-use-gtags-mixer nil)
;; (require 'gtags)

;; (require 'sr-speedbar)
;; (global-set-key (kbd "s-s") 'sr-speedbar-toggle)

;; (require 'sr-speedbar)
;; (global-set-key (kbd "C-q C-s") 'sr-speedbar-toggle)
;; (setq sr-speedbar-width-x 60)
;; (setq sr-speedbar-max-width 50)
;; (setq sr-speedbar-right-side nil)

;;-------------------------------
;; smartchr
;;-------------------------------
(require 'smartchr)

;; substitute `!!' with cursor

(global-set-key (kbd "{") (smartchr '("{`!!'}" "{")))
(global-set-key (kbd "}") (smartchr '("}" "{`!!'}" "}")))
(global-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
(global-set-key (kbd "\'") (smartchr '("\'" "\'`!!'\'")))
(global-set-key (kbd "`") (smartchr `("`" "``!!'`")))
(global-set-key (kbd "(") (smartchr '("(`!!')" "(")))
(global-set-key (kbd ")") (smartchr '(")" "(`!!')" )))
(global-set-key (kbd "+") (smartchr '("+" "++" "+++")))
(global-set-key (kbd "[") (smartchr '("[`!!']" "[" "]")))
(global-set-key (kbd "]") (smartchr '("]" "[`!!']" "[]")))

;; (global-set-key (kbd "-") (smartchr '("-" "--" "---")))

;; (global-set-key (kbd "C-,") `my-replace-string)
(global-set-key (kbd "C-,") '(lambda () (interactive) (insert "_")))

;;;-------------------------------
;;; windowナンバリング
;;;-------------------------------
(require 'window-numbering)
(window-numbering-mode 1)
;; (global-set-key (kbd "C-l C-b C-0") 'select-window-0)
;; (global-set-key (kbd "C-l C-b C-1") 'select-window-1)
;; (global-set-key (kbd "C-l C-b C-2") 'select-window-2)
;; (global-set-key (kbd "C-l C-b C-3") 'select-window-3)
;; (global-set-key (kbd "C-l C-b C-4") 'select-window-4)
;; (global-set-key (kbd "C-l C-b C-5") 'select-window-5)
;; (global-set-key (kbd "C-l C-b C-6") 'select-window-6)
;; (global-set-key (kbd "C-l C-b C-7") 'select-window-7)
;; (global-set-key (kbd "C-l C-b C-8") 'select-window-8)
;; (global-set-key (kbd "C-l C-b C-9") 'select-window-9)

;;;-------------------------------
;;; region selectinon
;;;-------------------------------
(require 'thing-opt)
(define-thing-commands)
(global-unset-key (kbd "C-l C-j"))
(global-set-key (kbd "C-l C-j C-w") 'mark-word*)
(global-set-key (kbd "C-l C-j C-e") 'mark-sexp*)
(global-set-key (kbd "C-l C-j C-s") 'mark-string*)
(global-set-key (kbd "C-l C-j C-f") 'mark-defun*)

;;;-------------------------------
;;; vc keybind
;;;-------------------------------
;; C-x v v vc-next-action          次の動作 (commit)
;; C-x v d vc-directory            登録されたファイルを表示
;; C-x v = vc-diff                 diff表示
;; C-x v u vc-revert-buffer        checkinしたものに戻す
;; C-x v ~ vc-version-other-window 所定のrevを別のwindowへ
;; C-x v l vc-print-log            log表示
;; C-x v i vc-register             add
;; C-x v h vc-insert-headers       version headerを挿入
;; C-x v r vc-retrieve-snapshot    tag指定checkout
;; C-x v s vc-create-snapshot      tagをつける
;; C-x v c vc-cancel-version       保存されたrevを捨てる。
;; C-x v a vc-update-change-log    GNUスタイルでchangeLogを更新

;;;-------------------------------
;;; 日本語入力
;;;-------------------------------
(require 'quail)
(define-key quail-translation-keymap (kbd "C-h") 'quail-conversion-backward-char)
(define-key quail-conversion-keymap (kbd "C-h") 'quail-conversion-backward-char)

;; (require 'sticky)
;; (use-sticky-key ";" sticky-alist:ja)
;; (use-sticky-key ";" sticky-alist:ja)


(provide 'init-keybindings)
