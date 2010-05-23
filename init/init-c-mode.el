;;; init-c-mode.el --- c, c++, objc mode setting

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, c, c++, objc mode setting
;; creation time: Sat May  1 22:50:26 2010
;;; Commentary:

;;; Code:

;;c-mode key-binding
;; C-c C-a			c-toggle-auto-newline
;; C-c C-b			c-submit-bug-report
;; C-c C-c			comment-region
;; C-c C-d			c-hungry-delete-forward
;; C-c C-e			c-macro-expand
;; C-c C-l			c-toggle-electric-state
;; C-c C-n			c-forward-conditional
;; C-c C-o			c-set-offset
;; C-c C-p			c-backward-conditional
;; C-c C-q			c-indent-defun
;; C-c C-s			c-show-syntactic-information
;; C-c C-u			c-up-conditional
;; C-c C-w			c-subword-mode
;; C-c C-\			c-backslash-region
;; C-c .				c-set-style
;; C-c :				c-scope-operator
;; C-c DEL			c-hungry-delete-backwards
;; C-c C-DEL		c-hungry-delete-backwards
;; C-c <C-backspace>			c-hungry-delete-backwards
;; C-c <C-delete>				c-hungry-delete-forward
;; C-c <C-deletechar>			c-hungry-delete-forward
;; C-c <deletechar>				c-hungry-delete-forward

(require 'init-compile-env)

;;;-------------------------------
;;; c-mode-style
;;;-------------------------------
(defconst my-c-style
  `((c-recognize-knr-p . nil)
    (c-enable-xemacs-performance-kludge-p . t) ; speed up indentation in XEmacs

    ;;change
    (c-basic-offset . 4)
    (indent-tabs-mode . t)
    ;;(c-basic-offset . 2)
    ;;(indent-tabs-mode . nil)
    (c-comment-only-line-offset . 0)
    
    (c-hanging-braces-alist . ((defun-open after)
			       (defun-close before after)
			       (class-open after)
			       (class-close before after)
			       (namespace-open after)
			       (inline-open after)
			       (inline-close before after)
			       (block-open after)
			       (block-close . c-snug-do-while)
			       (extern-lang-open after)
			       (extern-lang-close after)
			       (statement-case-open after)
			       (substatement-open after)))
    (c-hanging-colons-alist . ((case-label)
			       (label after)
			       (access-label after)
			       (member-init-intro before)
			       (inher-intro)))
    (c-hanging-semi&comma-criteria
     . (c-semi&comma-no-newlines-for-oneline-inliners
	c-semi&comma-inside-parenlist
	c-semi&comma-no-newlines-before-nonblanks))
    (c-indent-comments-syntactically-p . nil)
    (comment-column . 40)
    (c-cleanup-list . (brace-else-brace
		       brace-elseif-brace
		       brace-catch-brace
		       empty-defun-braces
		       defun-close-semi
		       list-close-comma
		       scope-operator))
    (c-offsets-alist . (
			(arglist-intro . +)
			(func-decl-cont . +)
			(member-init-intro . +)
			(inher-intro . ++)
			(comment-intro . 0)
			(cpp-macro . -)
			(statement . 0)
			(arglist-cont-nonempty . 0)
			(arglist-close . 0)
			(topmost-intro . 0)
			(block-open . 0)
			(inline-open . 0)
			(substatement-open . 0)
			(statement-cont
			 .
			 (,(when (fboundp 'c-no-indent-after-java-annotations)
			     'c-no-indent-after-java-annotations)
			  ,(when (fboundp 'c-lineup-assignments)
			     'c-lineup-assignments)
			  0))
			(label . /)
			(case-label . 0)
			(statement-case-open . +)
			(statement-case-intro . +) ; case w/o {
			(access-label . -)
			(innamespace . +))))
  "My C/C++ Programming Style")

(defun my-set-c-style ()
  "Set the current buffer's c-style to My C/C++ Programming
  Style. Meant to be added to `c-mode-common-hook'."
  (interactive)
  (c-add-style "My" my-c-style t))

;;;-------------------------------
;;; ソース<->ヘッダの移動用コマンドの拡張子の関連付け追加
;;; objective-c 用に追加
;;;-------------------------------
(require 'find-file)
(unless my-initialized
  (add-to-list 'cc-other-file-alist '("\\.h\\'"  (".m" ".mm" ".c" ".cpp")))
  (add-to-list 'cc-other-file-alist '("\\.m\\'"  (".h")))
  (add-to-list 'cc-other-file-alist '("\\.mm\\'"  (".h")))
  (add-to-list 'cc-other-file-alist '("\\.h\\'"  (".inl")))
  )

;;;----------------------------------------
;;;c-mode, c++-mode
;;;----------------------------------------
(require 'highlight-parentheses)
(require 'unreal)

(defun my-c-mode-hook ()
  (hl-line-mode t)
  (highlight-parentheses-mode t)
  (setq tab-width 4)
  (my-set-c-style)
  (modify-syntax-entry ?_ "w" c-mode-syntax-table)
  (modify-syntax-entry ?_ "w" c++-mode-syntax-table)
  (global-set-key (kbd "C-c C-t") 'c-end-of-defun)
  (global-set-key (kbd "C-c C-m") 'c-beginning-of-defun)
  ;; (c-set-offset 'c-comment-only-line-offset c-basic-offset)
  ;; (c-set-offset 'innamespace c-basic-offset)

  (when (string-match "\.uc$" (buffer-file-name))
    (unreal-imenu-set-for-current-buffer))
  )

(setq auto-mode-alist
      (append
       '(("\\.[ch][pp]*$" . c++-mode)
	 ("\\.usf$" . c++-mode)
	 ("\\.inl$" . c++-mode)
	 ("\\.cg$" . c++-mode)
	 ("\\.fx$" . c++-mode)
	 ("\\.cgh$" . c++-mode)
	 ("\\.hlsl$" . c++-mode)
	 ("\\.uc$" . c++-mode)
	 ("\\.uci$" . c++-mode)
	 ("\\.uch$" . c++-mode)
	 ("\\.cs$" . c++-mode)
	 ("\\.mm$" . objc-mode)
	 ("\\.m$" . objc-mode)
	 ("\\.vsh$" . c++-mode)
	 ("\\.fsh$" . c++-mode)
	 )
       auto-mode-alist))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-hook 'objc-mode-hook 'my-c-mode-hook)

;;;-------------------------------
;; google-c-style
;;;-------------------------------
;; (require 'google-c-style)
;; (add-hook 'c-mode-common-hook 'google-set-c-style)
;; (add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; ;; 左ブレス{、コロン:、セミコロン;などが 入力された時に、
;; ;; 自動的に改行とインデントが挿入され、次の入力位置に カーソルを移動させる
;; (add-hook 'c-mode-common-hook
;;			 '(lambda () (c-toggle-auto-state 1)))

;; ;;バックスペースを押した時に、カーソルとそれ以前にある非空白文字までの
;; ;;空白文字を消去してしまう
;; (add-hook 'c-mode-common-hook
;;			 '(lambda () (c-toggle-hungry-state 1)))

(provide 'init-c-mode)

