;;; init-c-mode.el --- c, c++, objc mode setting

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, c, c++, objc mode setting
;; creation time: Sat May  1 22:50:26 2010
;;; Commentary:

;;; Code:

(my-require 'cc-mode)
;; (my-require 'cc-mode+)

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

(my-require 'init-compile-env)

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
(my-require 'find-file)

(unless my-initialized
  (add-to-list 'cc-other-file-alist '("\\.h\\'"  (".m" ".mm" ".c" ".cpp" ".inl")))
  (add-to-list 'cc-other-file-alist '("\\.m\\'"  (".h")))
  (add-to-list 'cc-other-file-alist '("\\.mm\\'"  (".h")))
  )

;;;----------------------------------------
;;;c-mode, c++-mode
;;;----------------------------------------
(my-require 'highlight-parentheses)
(my-require 'easy-imenu-index-generator-config)

(defun my-c-mode-hook ()
  (hl-line-mode t)
  (highlight-parentheses-mode t)
  (setq tab-width 4)
  (yalinum-mode t)
  (my-set-c-style)
  (modify-syntax-entry ?_ "w" c-mode-syntax-table)
  (modify-syntax-entry ?_ "w" c++-mode-syntax-table)
  (global-set-key (kbd "C-c C-t") 'c-end-of-defun)
  (global-set-key (kbd "C-c C-m") 'c-beginning-of-defun)
  ;; (c-set-offset 'c-comment-only-line-offset c-basic-offset)
  ;; (c-set-offset 'innamespace c-basic-offset)

  (rainbow-mode t)
  (rainbow-delimiters-mode t)

  (when (string-match "\.uc$" (buffer-file-name))
    (easy-imenu-index-generator-set-for-current-buffer easy-imenu-index-generator-unreal)
    )
  )

(setq auto-mode-alist
      (append
       '(("\\.[ch][pp]*$" . c++-mode)
				 ("\\.h$" . c++-mode)
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

;;;-------------------------------
;;; ff-find-other-file
;;;-------------------------------
(defun my-ff-find-other-file ()
	"add current directory files."
	(interactive)
	(unless (eq buffer-file-name nil)
		(let* ((current-dir (file-name-directory (buffer-file-name)))
					 (cc-search-directories
					 	(append (list
					 					 (concat current-dir "../Inc")
					 					 (concat current-dir "../Src"))
					 					cc-search-directories))
					 (ff-ignore-include t))
			(ff-find-other-file)))
	(ff-find-other-file))

(global-set-key (kbd "C-l C-f C-s") 'my-ff-find-other-file)

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-hook 'objc-mode-hook 'my-c-mode-hook)

(define-key c-mode-base-map (kbd "C-c C-r") '(lambda () (interactive) (insert-string "->")))
(define-key c-mode-base-map (kbd "C-c C--") '(lambda () (interactive) (insert-string "=")))

;;;-------------------------------
;; google-c-style
;;;-------------------------------
;; (my-require 'google-c-style)
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

(defun my-run-c++ ()
  (interactive)
  (shell-command
   ;; (concat "g++ *.cpp -o temp")
   (concat "g++ " (buffer-file-name (current-buffer)) " -o temp")
   "* C++ Build *")
  ;; (shell-command
  ;;  (concat "./temp")
  ;;  "* C++ Run *")
  )

;; c++ run.
(define-key c++-mode-map (kbd "C-c C-c")
  `my-run-c++)

(defun my-build-and-run-c++ ()
  (interactive)
  (async-shell-command "run.sh" "*C++ Build and Run*"))

(defun my-msdn ()
  (interactive)
  (browse-url (concat "http://social.msdn.microsoft.com/search/ja-jp?query=" (read-string "find:"))))

;; c++ run.
(define-key c++-mode-map (kbd "C-c C-v") `my-build-and-run-c++)
(define-key c++-mode-map (kbd "C-c C-h") `my-msdn)

(my-require 'lua-mode)

(my-require 'rc-vstudio)

(setq vstudio-remote-exe-name "c:/Users/tm8st/Dropbox/Emacs/visual_studio_hidemaru.exe")

(setq my-cc-imenu-c++-generic-expression
  `(
    ;; Try to match ::operator definitions first. Otherwise `X::operator new ()'
    ;; will be incorrectly recognized as function `new ()' because the regexps
    ;; work by backtracking from the end of the definition.
    (nil
     ,(concat
       "^\\<.*"
       "[^" c-alnum "_:<>~]"                  ; match any non-identifier char
                                              ; (note: this can be `\n')
       "\\("
       "\\([" c-alnum "_:<>~]*::\\)?"      ; match an operator
          "operator\\>[ \t]*"
          "\\(()\\|[^(]*\\)"                  ; special case for `()' operator
       "\\)"

       "[ \t]*([^)]*)[ \t]*[^ \t;]"           ; followed by ws, arg list,
                                              ; require something other than
                                              ; a `;' after the (...) to
                                              ; avoid prototypes.  Can't
                                              ; catch cases with () inside
                                              ; the parentheses surrounding
                                              ; the parameters.  e.g.:
                                              ; `int foo(int a=bar()) {...}'
       ) 1)

    ;; Special case to match a line like `main() {}'
    ;; e.g. no return type, not even on the previous line.
    (nil
     ,(concat
       "^"
       "\\([" c-alpha "_][" c-alnum "_:<>~]*\\)" ; match function name
       "[ \t]*("			      ; see above, BUT
       "[ \t]*\\([^ \t(*][^)]*\\)?)"          ; the arg list must not start
       "[ \t]*[^ \t;(]"                       ; with an asterisk or parentheses
       ) 1)

    ;; General function name regexp
    ("Func"
     ,(concat
       "[\t ]?"                                  ; beginning of line is required, allow spaces.
       "\\<"                                     ; line MUST start with word char
       ;; \n added to prevent overflow in regexp matcher.
       ;; http://lists.gnu.org/archive/html/emacs-pretest-bug/2007-02/msg00021.html
       "[^()\n]*"                                ; no parentheses before
       "\\(template[ \t]*<[^>]+>[ \t]*\\)?"      ; there may be a `template <...>'
       "[" c-alnum "_::][::]? "                 ; match return type
       "\\([" c-alpha "_][" c-alnum "_:<>~]*\\)" ; match function name
       "\\([ \t\n]\\|\\\\\n\\)*("                ; see above, BUT the arg list
       "\\([ \t\n]\\|\\\\\n\\)*"                 ; must not start
       "\\([^ \t\n(*]"                           ; with an asterisk or parentheses
       "[^()]*\\(([^()]*)[^()]*\\)*"             ; Maybe function pointer arguments
       "\\)?)"
       "\\([ \t\n]\\|\\\\\n\\)*[^ \t\n           ;(]"
       ) 2)

    ;; Special case for definitions using phony prototype macros like:
    ;; `int main _PROTO( (int argc,char *argv[]) )'.
    ;; This case is only included if cc-imenu-c-prototype-macro-regexp is set.
    ;; Only supported in c-code, so no `:<>~' chars in function name!
    ,@(if cc-imenu-c-prototype-macro-regexp
            `((nil
                 ,(concat
                   "^\\<.*"                   ; line MUST start with word char
                   "[^" c-alnum "_]"          ; match any non-identifier char
                   "\\([" c-alpha "_][" c-alnum "_]*\\)" ; match function name
                   "[ \t]*"                   ; whitespace before macro name
                   cc-imenu-c-prototype-macro-regexp
                   "[ \t]*("                  ; ws followed by first paren.
                   "[ \t]*([^)]*)[ \t]*)[ \t]*[^ \t;]" ; see above
                   ) 1)))

    ;; Class definitions
    ("Class"
     ,(concat
       "[\t ]?"                              ; beginning of line is required, allow spaces.
       "\\(template[ \t]*<[^>]+>[ \t]*\\)?" ; there may be a `template <...>'
       "\\(class\\|struct\\)[ \t]+"
       "\\("                                ; the string we want to get
       "[" c-alnum "_]+"                    ; class name
       "\\(<[^>]+>\\)?"                     ; possibly explicitly specialized
       "\\)"
       "\\([ \t\n]\\|\\\\\n\\)*[:{]"
       ) 3)

    ;; enum definitions
    ("Enum"
     ,(concat
       "[\t ]?"                              ; beginning of line is required, allow spaces.
       "enum[ \t]+"
       "\\("                                ; the string we want to get
       "[" c-alnum "_]+"                    ; class name
       "\\(<[^>]+>\\)?"                     ; possibly explicitly specialized
       "\\)"
       "\\([ \t\n]\\|\\\\\n\\)*[:{]"
       ) 1)
    ))

(setq cc-imenu-c-generic-expression my-cc-imenu-c++-generic-expression)

(provide 'init-c-mode)
