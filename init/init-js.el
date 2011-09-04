;;; init-js.el --- java script setting

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, go
;; creation time: Wed Apr 28 00:10:36 2010
;;; Commentary:

;;; Code:

(when (my-require 'js2-mode)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (setq js2-cleanup-whitespace nil
        js2-mirror-mode nil
        js2-bounce-indent-flag nil)

  (defun indent-and-back-to-indentation ()
    (interactive)
    (indent-for-tab-command)
    (let ((point-of-indentation
           (save-excursion
             (back-to-indentation)
             (point))))
      (skip-chars-forward "\s " point-of-indentation)))
  (define-key js2-mode-map "\C-i" 'indent-and-back-to-indentation)
  
  (define-key js2-mode-map "\C-m" nil)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  )

(add-hook 'js2-mode-hook
	  (lambda () 
	    (my-require 'flymake-jsl)
	    (setq flymake-check-was-interrupted t)
      (yalinum-mode 1)))

(provide 'init-js)

