;;; init-view-mode.el --- view-mode setting

;; Copyright (C) 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, view-mode

;;; Commentary:

;; set vim like key bindings.

;;; Code:

(my-require 'view)
(global-set-key (kbd "C-q C-;") 'view-mode)
(define-key view-mode-map "q" 'view-mode)
(define-key view-mode-map "h" 'backward-char)
(define-key view-mode-map "j" 'next-line)
(define-key view-mode-map "k" 'previous-line)
(define-key view-mode-map "l" 'forward-char)
(define-key view-mode-map "v" 'my-scroll-up)
(define-key view-mode-map ";" 'my-scroll-down)
(define-key view-mode-map "0" 'beginning-of-line)
(define-key view-mode-map "^" 'back-to-indentation)
(define-key view-mode-map "$" 'end-of-line)
(define-key view-mode-map "b" 'backward-word)
(define-key view-mode-map "w" 'forward-word)
(define-key view-mode-map "G" 'end-of-buffer)
(define-key view-mode-map "t" 'isearch-backward)
(define-key view-mode-map "f" 'isearch-forward)
(define-key view-mode-map "%" 'my-match-paren)

(provide 'init-view-mode)
