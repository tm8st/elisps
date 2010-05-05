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

(provide 'init-elisp)
