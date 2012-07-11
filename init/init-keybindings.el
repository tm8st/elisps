;;; init-keybindings.el --- key binding

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, keybinds
;; creation time: Wed Apr 28 00:33:52 2010
;;; Commentary:

;;; Code:

(my-require 'init-my-misc)
(my-require 'generic-range-opt)
(my-require 'prefix-arg-commands)

;;; sequential-command-config
(my-require 'sequential-command)
(my-require 'sequential-command-config)
(define-sequential-command beginning-of-anything-seq
  back-to-indentation beginning-of-line seq-return)

(define-sequential-command end-of-anything-seq
  end-of-line seq-return)

(prefix-arg-commands-defun prefix-arg-commands-forward-long-move-commands
			   '(forward-word forward-sentence forward-paragraph))
(prefix-arg-commands-defun prefix-arg-commands-backward-long-move-commands
			   '(backward-word backward-sentence backward-paragraph))

(defvar my-forward-word-command 'prefix-arg-commands-forward-long-move-commands)
(defvar my-backward-word-command 'prefix-arg-commands-backward-long-move-commands)

;; 基本操作
(global-set-key (kbd "C-f") 'prefix-arg-commands-forward-move-commands)
(global-set-key (kbd "C-b") 'prefix-arg-commands-backward-move-commands)
(global-set-key (kbd "C-t") my-forward-word-command)
(global-set-key (kbd "C-m") my-backward-word-command)
;; (global-set-key (kbd "C-a") 'prefix-arg-commands-back-to-indentation-move-commands)
;; (global-set-key (kbd "C-e") 'prefix-arg-commands-end-of-line-move-commands)

(global-set-key (kbd "C-a") 'beginning-of-anything-seq)
(global-set-key (kbd "C-e") 'end-of-anything-seq)

(global-set-key (kbd "C-a") 'beginning-of-anything-seq)
(global-set-key (kbd "C-e") 'end-of-anything-seq)
(global-set-key (kbd "C-;") 'my-scroll-down)
(global-set-key (kbd "C-v") 'my-scroll-up)
(global-set-key (kbd "C-S-t") 'forward-sexp)
(global-set-key (kbd "C-S-m") 'backward-sexp)
(global-set-key (kbd "C-x C-[") 'beginning-of-buffer)
(global-set-key (kbd "C-x C-]") 'end-of-buffer)
(global-set-key (kbd "C-j") 'newline)
(global-set-key (kbd "C-l C-e") 'my-eval-buffer-or-region)
(global-set-key (kbd "C-q C-e") 'my-eval-buffer-or-region)
;; (global-set-key (kbd "C-c C-e") 'my-eval-buffer-or-region)
(global-set-key (kbd "C-l C-l") '(lambda () (interactive) (recenter 3)))
(global-set-key (kbd "C-l C-o C-e") 'my-scratch)
(global-set-key (kbd "C-l C-o C-c") 'elint-current-buffer)
(global-set-key (kbd "C-l C-o C-w") 'my-which)

(defun my-which (name)
  (interactive "sCommand name: ")
  (message
   (substring (shell-command-to-string (format "which %s" name)) 0 -1)))

;; forward-sentenceで行の最後ではなく、次の行まですすめる。
(defadvice forward-sentence
  (after forward-sentence-forward-one activate)
  (forward-char)
  ad-do-it)

(global-set-key (kbd "C-h") 'delete-backward-char)
;; killではなくてdeleteに削除コマンドを変更
(global-set-key (kbd "C-k") 'my-delete-line-forward)
;; (global-set-key (kbd "C-l C-z") 'toggle-input-method) SKKへ

(global-set-key (kbd "C-z") 'undo)

(my-require 'redo)
(global-set-key (kbd "C-/") 'redo)
(global-set-key (kbd "C-S-z") 'redo)

(my-require 'ibuffer)
(global-set-key (kbd "C-x b") 'ibuffer-list-buffers) ;;バッファウィンドウを別ウィンドウに出してフォーカスを写す
(global-set-key (kbd "C-x C-b") 'ibuffer) ;;バッファウィンドウを現在ウィンドウに出す

(global-set-key (kbd "C-x d") 'dired-other-window)
(global-set-key (kbd "C-x C-d") 'dired)

(global-set-key (kbd "C-x k") 'kill-buffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer)

(global-set-key (kbd "C-x :") 'execute-extended-command)
(global-set-key (kbd "C-:") 'execute-extended-command)
;; (global-set-key (kbd "C-,") `my-replace-string)
(global-set-key (kbd "C-,") '(lambda () (interactive) (insert "_")))
(global-set-key (kbd "C-.") 'my-just-one-space-toggle)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-S-s") 'isearch-forward)
(global-set-key (kbd "C-S-r") 'isearch-backward)
(global-set-key (kbd "C-q C-t C-t") 'toggle-case-fold-search)

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

(prefix-arg-commands-defun prefix-arg-commands-word-op
													 (list
														'gro-kill-follow-word
														'gro-copy-follow-word))

(global-set-key (kbd "C-w") 'prefix-arg-commands-word-op)

;; window 切り替え
(global-set-key (kbd "C-l C-n") 'other-window)
(global-set-key (kbd "C-l C-p") #'(lambda (arg) (interactive "p") (other-window (- arg))))

;; window 操作
(global-set-key (kbd "C-l C-1") 'delete-other-windows)
(global-set-key (kbd "C-l C-2") 'split-window-vertically)
(global-set-key (kbd "C-l C-3") 'split-window-horizontally)
(global-set-key (kbd "C-l C-0") 'delete-window)

;; (global-set-key (kbd "C-l C-w") 'my-copy-region-or-follow-word) ;;copy
(global-set-key (kbd "C-l C-k") 'kill-whole-line) ;;line kill
(global-set-key (kbd "C-l C-d C-f") 'my-delete-forward-word)
(global-set-key (kbd "C-l C-d C-b") 'my-delete-backward-word)
(global-set-key (kbd "C-l C-q") 'my-match-paren)
(global-set-key (kbd "C-l C-c") 'calculator)

(global-set-key (kbd "C-l C-g") 'goto-line) ;;指定した行へ。
(global-set-key (kbd "C-q C-d C-e") 'ediff-buffers)

(global-set-key (kbd "C-l C-f C-b") 'browse-url)
(global-set-key (kbd "C-l C-f C-d") 'find-function)
(global-set-key (kbd "C-l C-f C-j") 'ffap)

;; 外部のfiler で 開く
(global-set-key (kbd "C-l C-f C-e") 'my-open-directory-by-external-filer)
;; osの関連付けで開く
(global-set-key (kbd "C-l C-f C-f") 'my-open-file-os)

;;ソースとヘッダファイルの移動用
(global-set-key (kbd "C-l C-f C-s") 'ff-find-other-file)

(global-set-key (kbd "C-l C-f C-o") 'my-save-all-buffers)

(define-key global-map (kbd "C-l C-z") 'prefix-arg-commands-set-frame-alpha)

;; 重複行削除
(my-require 'uniq)
(define-key global-map (kbd "C-l C-t C-d") 'uniq-remove-dup-lines)

;;大文字小文字変換
(global-set-key (kbd "C-q C-u") 'my-changecase-word)

;;;-------------------------------
;;; mocccur 置換用
;;;-------------------------------
(my-require 'color-moccur)
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
;;;-------------------------------
 (my-require 'undo-tree)
;; (global-undo-tree-mode)
(global-set-key (kbd "C-l C-u C-t") `undo-tree-visualize)
(define-key undo-tree-visualizer-map (kbd "C-g") `undo-tree-visualizer-quit)

;; ブックマーク設定
(global-set-key (kbd "C-q C-b C-m") 'bookmark-set)
(global-set-key (kbd "C-q C-b C-j") 'bookmark-jump)

(global-set-key (kbd "C-q C-q") 'quoted-insert)       ;;元のコマンド
;; (global-set-key (kbd "C-q C-@") 'yalinum-mode)        ;;行番号表示のトグル。
(global-set-key (kbd "C-q C-h") 'help-for-help)       ;;ヘルプ

(global-set-key (kbd "C-q C-w") 'gro-copy-follow-word) ;;copy
;; (global-set-key (kbd "C-q C-w") 'copy-region-as-kill) ;;copy

;; text edit.
(global-set-key (kbd "C-q C-c") 'comment-or-uncomment-region) ;;コメント付加、解除
(global-set-key (kbd "C-q C-t C-a") 'align-regexp)            ;;特定文字での整列
(global-set-key (kbd "C-q C-t C-r")  'query-replace-regexp)   ;;置換
;; (global-set-key (kbd "C-q C-t C-o") 'overwrite-mode) ;;
(global-set-key (kbd "C-q C-t C-t") 'tabify)                  ;; tab化
(global-set-key (kbd "C-q C-t C-u") 'untabify)                ;; untab化
;; (global-set-key (kbd "C-q C-t C-i")  'indent-region) ;;インデント

;; Perform general cleanup.
(global-set-key (kbd "C-q C-d C-b") 'clean-buffer-list)

(global-set-key (kbd "C-q C-k") 'my-delete-line-backward)

(global-set-key (kbd "C-q C-2") '(lambda () (interactive) (insert-string "\"\"") (backward-char)))
(global-set-key (kbd "C-q C-7") '(lambda () (interactive) (insert-string "''") (backward-char)))
(global-set-key (kbd "C-q C-@") '(lambda () (interactive) (insert-string "``") (backward-char)))

;;;-------------------------------
;;; delete, mark, kill some ranges.
;;;-------------------------------
(my-require 'generic-range-opt)

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
		(my-delete-region-or-delete-char)))

(global-set-key (kbd "C-d") 'my-delete-char)

;; 範囲選択
(global-unset-key (kbd "C-8"))
(global-set-key (kbd "C-8 C-w") 'gro-mark-follow-word)
(global-set-key (kbd "C-8 C-e") 'gro-mark-follow-sexp)
(global-set-key (kbd "C-8 C-s") 'gro-mark-follow-string)
(global-set-key (kbd "C-8 C-p") 'mark-paragraph*)
(global-set-key (kbd "C-8 C-d") 'gro-mark-defun*)
(global-set-key (kbd "C-8 C-u") 'gro-mark-match-brace)
(global-set-key (kbd "C-8 C-i") 'gro-mark-match-paren)
(global-set-key (kbd "C-8 C-j") 'gro-mark-match-brace-1)
(global-set-key (kbd "C-8 C-k") 'gro-mark-match-paren-1)
(global-set-key (kbd "C-8 C-y") 'gro-mark-string-1)
(global-set-key (kbd "C-8 C-h") 'gro-mark-char-1)
(global-set-key (kbd "C-8 C-q") 'gro-mark-cppif)

(global-set-key (kbd "C-8 C-v") '(lambda () (interactive) (gro-paren-search-forward "#if" "#endif")))
(global-set-key (kbd "C-8 C-;") '(lambda () (interactive) (gro-paren-search-barkward "#if" "#endif")))

;; (global-set-key (kbd "C-8 C-f") 'gro-mark-search-forward-char)
;; (global-set-key (kbd "C-8 C-b") 'gro-mark-search-backward-char)
;; (global-set-key (kbd "C-8 C-f") 'gro-mark-forward-line)
;; (global-set-key (kbd "C-8 C-b") 'gro-mark-backward-line)
;; (global-set-key (kbd "C-8 C-8") 'gro-mark-current-line)
;; (global-set-key (kbd "C-8 C-n") 'gro-mark-next-line)
;; (global-set-key (kbd "C-8 C-p") 'gro-mark-prev-line)

(defun my-face-at-point ()
	(interactive)
	(face-at-point))

(global-set-key (kbd "C-l C-.") 'my-face-at-point)

;; エラー箇所へのジャンプ用
(global-set-key (kbd "C-l C-;") 'compilation-minor-mode)
(define-key compilation-minor-mode-map (kbd "C-c C-c") 'comint-interrupt-subjob)

(global-set-key (kbd "M-n") 'my-move-line-down)
(global-set-key (kbd "M-p") 'my-move-line-up)

(my-require 'bm)
(global-set-key (kbd "M-C-m") 'bm-toggle)
(global-set-key (kbd "M-C-n") 'bm-next)
(global-set-key (kbd "M-C-p") 'bm-previous)

(global-set-key (kbd "C-l C-b C-m") 'bm-toggle)
(global-set-key (kbd "C-l C-b C-n") 'bm-next)
(global-set-key (kbd "C-l C-b C-p") 'bm-previous)

;; save bookmarks
(setq-default bm-buffer-persistence t)
;; Filename to store persistent bookmarks
(setq bm-repository-file "~/.emacs.d/.bm-repository")

;; Loading the repository from file when on start up.
(add-hook' after-init-hook 'bm-repository-load)

;; Restoring bookmarks when on file find.
(add-hook 'find-file-hooks 'bm-buffer-restore)
 
;; Saving bookmark data on killing and saving a buffer
(add-hook 'kill-buffer-hook 'bm-buffer-save)
(add-hook 'auto-save-hook 'bm-buffer-save)
(add-hook 'after-save-hook 'bm-buffer-save)
 
;; Saving the repository to file when on exit.
;; kill-buffer-hook is not called when emacs is killed, so we
;; must save all bookmarks first.
(add-hook 'kill-emacs-hook '(lambda nil
                              (bm-buffer-save-all)
                              (bm-repository-save)))

(defadvice bm-goto (before bm-mark-set activate)
  (unless (bm-bookmark-at (point))
    (push-mark)))

(my-require 'anything)
(defvar anything-c-source-bm-global-use-candidates-in-buffer
  '((name . "Global Bookmarks")
    (init . anything-c-bm-global-init)
    (candidates-in-buffer)
    (type . file-line))
  "show global bookmarks list. Global means All bookmarks exist in `bm-repository'.
	 Needs bm.el. http://www.nongnu.org/bm/")

(defvaralias 'anything-c-source-bm-global 'anything-c-source-bm-global-use-candidates-in-buffer)

(defun my-anything-bm-global ()
	(interactive)
	(anything 'anything-c-source-bm-global))

(defun anything-c-bm-global-init ()
  "Init function for `anything-c-source-bm-global'."
  (when (my-require 'bm)
    (with-no-warnings
      (let ((files bm-repository)
            (buf (anything-candidate-buffer 'global)))
        (dolist (file files)            ;ブックマークされてるファイルリストから，1つ取り出す
          (dolist (bookmark (cdr (assoc 'bookmarks (cdr file)))) ;1つのファイルに対して複数のブックマークがあるので
            (let ((position (cdr (assoc 'position bookmark))) ;position
                  (annotation (cdr (assoc 'annotation bookmark))) ;annotation
                  (file (car file))                               ;file名を取り出す
                  line
                  str)
              (setq str (with-current-buffer (find-file-noselect file) ;anything用の文字列にformat
                               (goto-char position)
                               (beginning-of-line)
                               (unless annotation
                                   (setq annotation ""))
                               (if (string= "" line)
                                   (setq line  "<EMPTY LINE>")
                                 (setq line (car (split-string (thing-at-point 'line) "[\n\r]"))))
                               (format "%s:%d: [%s]: %s\n" file (line-number-at-pos) annotation line)))
              (with-current-buffer buf (insert str)))))))))

(global-set-key (kbd "C-q C-a C-m") 'my-anything-bm-global)

(defun my-insert-zenkakuspace-rectangle ()
  (interactive)
  (when mark-active
    (string-rectangle
     (min (point) (mark))
     (max (point) (mark))
     "　")))

(define-key global-map (kbd "C-l C-j C-t") 'my-insert-zenkakuspace-rectangle)

(defun my-insert-space-rectangle ()
  (interactive)
  (when mark-active
    (string-rectangle
     (min (point) (mark))
     (max (point) (mark))
     (make-string tab-width ? ))))

(define-key global-map (kbd "C-l C-j C-r") 'my-insert-space-rectangle)

(my-require 'sub-frame)
(setq sf:frame-left -1920)
(setq sf:frame-top 20)
(setq sf:frame-width 60)
(setq sf:frame-height 30)

(global-set-key (kbd "C-l C-s C-[") 'sf:jump-to-buffer-top)
(global-set-key (kbd "C-l C-s C-]") 'sf:jump-to-buffer-bottom)
(global-set-key (kbd "C-l C-s C-;") 'sf:scroll-down)
(global-set-key (kbd "C-l C-s C-v") 'sf:scroll-up)
(global-set-key (kbd "C-l C-s C-c") 'sf:current-buffer)
(global-set-key (kbd "C-l C-s C-h") 'sf:toggle-hidden)
(global-set-key (kbd "C-l C-s C-r") 'sf:set-frame-parameters)
(global-set-key (kbd "C-l C-s C-:") 'sf:async-shell-command)

(defun toggle-truncate-lines ()
  "折り返し表示をトグル動作します."
  (interactive)
  (if truncate-lines
      (setq truncate-lines nil)
    (setq truncate-lines t))
  (recenter))

(global-set-key (kbd "C-q C-l C-l") 'toggle-truncate-lines)

(provide 'init-keybindings)
