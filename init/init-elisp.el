;;; init-elisp.el --- elisp

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, elisp
;; creation time: Wed Apr 28 00:07:49 2010
;;; Commentary:

;;; Code:

(defun my-emacs-lisp-mode-hook ()
  ;; (linum-mode t)
  (hl-line-mode t)
  (when use-gui-setting
    (highlight-parentheses-mode)
    )
  )

(setq auto-mode-alist
	  (append
	   '(("\\.el*$" . emacs-lisp-mode)
		 )
	   auto-mode-alist))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

;;;-------------------------------
;;; eldoc lispのヘルプ表示
;;;-------------------------------
(require 'eldoc)
;; (install-elisp-from-emacswiki "eldoc-extension.el")
(require 'eldoc-extension)
(setq eldoc-idle-delay 0.15)
(setq eldoc-echo-area-use-multiline-p t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;;-------------------------------
;;; auto-install misc
;;;-------------------------------
(defun my-auto-install-from-requires ()
  "Batch install many packages form elisp requires."
  (interactive)
  (setq auto-install-save-confirm nil)
   (dolist (req (split-string (shell-command-to-string "grep 'require ' *.el") "\n"))
     (unless (string-match "$;" req)
       (progn 
	 (let ((begin (string-match "'" req)))
	   (if (eq begin nil) nil
	     (let ((end (string-match "\\()\\| \\|\n\\|\t\\)" req begin)))
	       (if (eq end nil) nil
		 (progn
		   (sleep-for 3)
		   (auto-install-from-url
		    (concat "http://www.emacswiki.org/emacs/download/"
			    (substring req (+ 1 begin) end) ".el")))))))))))

(provide 'init-elisp)
