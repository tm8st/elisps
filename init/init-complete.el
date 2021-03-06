;;; init-complete.el --- my complete setting.

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, complete

;;; Commentary:

;;; Code:

(my-require 'init-compile-env)
(my-require 'eshell)
(my-require 'imenu)
;; (my-require 'complete) ;; 強力な補完機能を使う
;; (partial-completion-mode 1)
(icomplete-mode t) ;; 補完可能なものを随時表示

;;;-------------------------------------
;;;auto-complete setting
;;;-------------------------------------
(my-require 'auto-complete)
(my-require 'auto-complete-config)
;; (ac-config-default)

(global-auto-complete-mode t)
(customize-set-value 'ac-dictionary-directories
                     (list "~/elisps/external/complete/m2ym-auto-complete/dict"
                           (concat my-dropbox-directory "/Emacs/resource/dict")))

(setq ac-auto-start 2)
(setq ac-dwim 1)
(customize-set-value 'ac-ignore-case nil)
(customize-set-value 'ac-candidate-limit 500)
(customize-set-value 'ac-use-fuzzy 't)
(customize-set-value 'ac-use-comphist 't)
(customize-set-value 'ac-use-quick-help 't)
(customize-set-value 'ac-delay 0.1)

(global-set-key (kbd "C-o") 'ac-start)

(define-key ac-complete-mode-map (kbd "C-i") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-o") 'ac-previous)
(define-key ac-complete-mode-map (kbd "C-u") 'ac-stop)
(define-key ac-complete-mode-map (kbd "C-i") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-j") 'ac-complete)

(my-require 'ac-anything)
(define-key ac-complete-mode-map (kbd "C-@") 'ac-complete-with-anything)

(my-require 'popup)
(my-require 'pcomplete)

(ac-define-source pcomplete
  '((candidates . pcomplete-completions)))

;; ac source setting.
(setq-default ac-sources
              '(ac-source-words-in-buffer
                ac-source-words-in-same-mode-buffers
                ac-source-dictionary
                ac-source-yasnippet
                ;; ac-source-pcomplete
                ;; ac-source-gtags
                ;; ac-source-imenu
                ;; ac-source-filename
                ;; ac-source-symbols
                ))

;; use ac modes.
(add-to-list 'ac-modes 'shell-mode)
(add-to-list 'ac-modes 'eshell-mode)
(add-to-list 'ac-modes 'fundamental-mode)
(add-to-list 'ac-modes 'text-mode)

;;;-------------------------------------
;;;標準補間機能のカスタマイズ completion
;;;-------------------------------------
;;key binding
(define-key completion-list-mode-map "\C-n" 'next-completion)
(define-key completion-list-mode-map "\C-f" 'next-completion)
(define-key completion-list-mode-map "\C-p" 'previous-completion)
(define-key completion-list-mode-map "\C-b" 'previous-completion)
(define-key completion-list-mode-map "\C-m" 'my-choose-completion)

(defun my-choose-completion ()
  "Choose the completion that point is in or next to."
  (interactive)
  (let (beg end completion (buffer completion-reference-buffer)
	    (base-size completion-base-position))
    (if (and (not (eobp)) (get-text-property (point) 'mouse-face))
	(setq end (point) beg (1+ (point))))
    (if (and (not (bobp)) (get-text-property (1- (point)) 'mouse-face))
	(setq end (1- (point)) beg (point)))
	(if (null beg)
	    (error "No completion here"))
	(setq beg (previous-single-property-change beg 'mouse-face))
	(setq end (or (next-single-property-change end 'mouse-face) (point-max)))
	;; (setq pletion (buffer-substring-no-properties beg end))
	(delete-completion-window)
	(choose-completion-string completion buffer base-size)))

;; 動的補完で無視する要素の正規表現
(customize-set-value 'dabbrev-abbrev-skip-leading-regexp "-")

;;;-------------------------------
;;;
;;;-------------------------------
;; (my-require 'complete)
;; (define-key minibuffer-local-completion-map "C-j" 'PC-complete-and-exit)
;; (define-key minibuffer-local-completion-map "C-m" 'backward-word)
;; (define-key minibuffer-local-completion-map "RET" 'backward-word)

;; (my-require 'skeleton)
;; (global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "'") 'skeleton-pair-insert-maybe)
;; (setq skeleton-pair 2)

(provide 'init-complete)
