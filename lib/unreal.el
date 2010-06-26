;;; unreal.el --- unreal script miscs

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: unreal
;; creation time: Wed May 19 00:03:07 2010
;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))
(require 'easy-imenu-index-create)

(defvar unreal-imenu-alist
	`(
	  ((caption . "[S] ")
	   (regexp . "^[ \t]*state\\(()\\)?[ \t]*"))
	  ((caption . "[V] ")
	   (regexp . "^[ \t]*var"))
	  ((caption . "[F] ")
	   (regexp . "^[ \t]*\\(native \\)?\\(final \\)?\\(function\\)[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"))
	  ((caption . "[E] ")
	   (regexp . "^[ \t]*\\(event \\)\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"))	
	  ((caption . "[Class] ")
	   (regexp . "^[ \t]*class[ \n\t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"))
	  ;; (regexp . "^[ \t]*class[ \n\t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)[ \t\n]*\\(:\\|{\\)"))
	  ((caption . "[Strucs] ")
	   (regexp . "^[ \t]*struct[ \n\t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)[ \t\n]*[{]"))
	  )
	"use this when create index."
	)

(defun unreal-imenu-create-index ()
  ""
  (interactive)
  (easy-imenu-index-create-imenu-create-index unreal-imenu-alist))

(defun unreal-imenu-set-for-current-buffer ()
  ""
  (interactive)
  (setq imenu-create-index-function 'unreal-imenu-create-index))
  ;; (easy-imenu-index-create-imenu-set-for-current-buffer unreal-imenu-alist))
  ;; (easy-imenu-index-create-imenu-set-for-current-buffer 'unreal-imenu-create-index))

(provide 'unreal)
