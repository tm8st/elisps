;;; easy-imenu-index-create.el --- easy implement imenu-create-index-function

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: imenu
;; creation time: Wed May 19 00:03:07 2010
;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

;; sample alist
(defvar easy-imenu-index-create-imenu-alist-sample
	`(
	  ((caption . "[setq] ")
	   (regexp . "^[ \t]*setq "))
	  )
	"use this when create index."
	)

(defun easy-imenu-index-create-imenu-alist-attr (name iter)
  (cdr (assq name iter)))

(defun easy-imenu-index-create-imenu-create-index (alist)
  "get current buffer imenu index."
  (save-excursion
    ;; (set-buffer buffer)
    (let ((index) (case-fold-search nil))
      (dolist (iter alist)
	(goto-char (point-min))
	(while (re-search-forward (easy-imenu-index-create-imenu-alist-attr 'regexp iter) nil t)
	  (goto-char (match-beginning 0))
	  (push (cons
		 (concat (easy-imenu-index-create-imenu-alist-attr 'caption iter)
			 (replace-regexp-in-string "[\n\t]" " " (thing-at-point 'line)))
		 ;; buffer
		 (point))
		index)
	  (goto-char (match-end 0))))
      (nreverse index))))

(defun easy-imenu-index-create-imenu-set-for-current-buffer (alist)
  ""
  (interactive)
  (setq imenu-create-index-function
	`(lambda () (interactive) (easy-imenu-index-create-imenu-create-index alist))))

(provide 'easy-imenu-index-create)
