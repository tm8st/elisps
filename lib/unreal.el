;;; unreal.el --- unreal script miscs

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: unreal
;; creation time: Wed May 19 00:03:07 2010
;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

(defvar unreal-imenu-alist
      '(
    	((symbol . "[V] ")
	 (regexp . "^[ \t]*var"))
	((symbol . "[S] ")
	 (regexp . "^[ \t]*state\\(()\\)?[ \t]*"))
	((symbol . "[F] ")
	 (regexp . "^[ \t]*\\(native \\)?\\(final \\)?\\(function\\)[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"))
	((symbol . "[E] ")
	 (regexp . "^[ \t]*\\(event \\)\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"))	
	((symbol . "[C] ")
	 (regexp . "^[ \t]*class[ \n\t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)[ \t\n]*\\(:\\|{\\)"))
	((symbol . "[S] ")
	 (regexp . "^[ \t]*struct[ \n\t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)[ \t\n]*[{]"))
	)
      "use this when create index."
      )

(defun unreal-imenu-alist-attr (name iter)
  (cdr (assq name iter)))

(defun unreal-imenu-create-index ()
  "get current buffer imenu index."
  (save-excursion
    ;; (set-buffer buffer)
    (let ((index) (case-fold-search nil))
      (dolist (iter unreal-imenu-alist)
	(goto-char (point-min))
	(while (re-search-forward (unreal-imenu-alist-attr 'regexp iter) nil t)
	  (goto-char (match-beginning 0))
	  (push (cons
		 (concat (unreal-imenu-alist-attr 'symbol iter)
			 (replace-regexp-in-string "[\n\t]" "" (thing-at-point 'line)))
		 ;; buffer
		 (point))
		index)
	  (goto-char (match-end 0))))
      (nreverse index))))

(defun unreal-imenu-set-for-current-buffer ()
  ""
  (interactive)
  (setq imenu-create-index-function 'unreal-imenu-create-index))

;; (global-set-key (kbd "C-l C-u") 'unreal-imenu-set-for-current-buffer)

(provide 'unreal)
