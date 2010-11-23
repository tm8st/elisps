;;; init-js.el --- java script setting

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, go
;; creation time: Wed Apr 28 00:10:36 2010
;;; Commentary:

;;; Code:

(require 'js2)

(when (require 'js2 nil t)
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

(provide 'init-js)

