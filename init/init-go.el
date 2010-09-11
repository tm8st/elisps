;;; init-go.el --- go setting

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, go
;; creation time: Wed Apr 28 00:10:36 2010
;;; Commentary:

;;; Code:

(require 'go-mode)
(load "go-mode-load.el")

(defun my-go-build-and-run ()
  (interactive)
  (shell-command (concat "8g " (buffer-file-name) " && 8l " (file-name-sans-extension (buffer-file-name)) ".8 && ./8.out " (read-string "arg?:")))
  )

;;(file-name-sans-extension (buffer-file-name))  
(define-key go-mode-map (kbd "C-c C-c") `my-go-build-and-run)

(provide 'init-go)
