;;; scala-imenu.el --- scala imenu

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: scala, imenu
;; creation time: [Thu Jun 10 23:00:56 2010]
;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

(defvar scala-imenu-alist
	`(
	  ((symbol . "[Class] ")
	   (regexp . "^[ \t]*\\(abstract \\)?\\(case \\)?class[ \n\t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"))
	  ((symbol . "[Object] ")
	   (regexp . "^[ \t]*object[ \n\t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"))
	  ((symbol . "[trait] ")
	   (regexp . "^[ \t]*trait[ \n\t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"))
	  ((symbol . "[F] ")
	   (regexp . "^[ \t]*\\(private \\)?\\(override \\)?\\(final \\)?\\(def \\)+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"))
	  ((symbol . "[VAL] ")
	   (regexp . "^[ \t]*\\(private \\)?\\var[ \t]"))
	  ((symbol . "[VAR] ")
	   (regexp . "^[ \t]*\\(private \\)?\\val[ \t]"))
	  )
	"use this when create index."
	)

(defun scala-imenu-alist-attr (name iter)
  (cdr (assq name iter)))

(defun scala-imenu-create-index ()
  "get current buffer imenu index."
  (save-excursion
    ;; (set-buffer buffer)
    (let ((index) (case-fold-search nil))
      (dolist (iter scala-imenu-alist)
	(goto-char (point-min))
	(while (re-search-forward (scala-imenu-alist-attr 'regexp iter) nil t)
	  (goto-char (match-beginning 0))
	  (push (cons
		 (concat (scala-imenu-alist-attr 'symbol iter)
			 (replace-regexp-in-string "[\n\t]" " " (thing-at-point 'line)))
		 ;; buffer
		 (point))
		index)
	  (goto-char (match-end 0))))
      (nreverse index))))

(defun scala-imenu-set-for-current-buffer ()
  ""
  (interactive)
  (setq imenu-create-index-function 'scala-imenu-create-index))

(provide 'scala-imenu)
